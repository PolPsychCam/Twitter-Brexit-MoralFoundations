#### Denotes code segment
## Explains process below
# Provides note


#### Directory, Files, Libraries####
##Clears the environment, sets the working directory, calls the libraries
rm(list = ls())
setwd("C:/Users/jaack/OneDrive - University Of Cambridge/Summer Political Psychology/Brexit Twitter/Data")
library('psych') #statistical tools
library('tidyr') #makes things tidyr
library('dplyr') #general data handling
library('XML') #xml handling
library('methods')#
library('pdftools') #pdf handling
library('tm') #textmining package
library('SnowballC') #required for some Corpus cleaning functions
library('car') #bonus regression tools
library('ResourceSelection') #Hosmer Lemeshow test
library('stargazer') #nice regression tables
library('tesseract') #OCR
library('magick') #Image refinement

#### Hansard XMLs in ####
##Creates a list of strings for each xml file name in the folder
filenames<-list.files("C:/Users/jaack/OneDrive - University Of Cambridge/Summer Political Psychology/Brexit Twitter/Data/Hansard",full.names=T)

##Parses those files into an R-readable format. 
#Note this format holds only references to the external file, not the data itself
allxmls<-lapply(filenames,xmlParse)

##Get segments from all politicians of interest (1st line creates an entry of matches from each xml, 2nd deletes null entries)
#No data for non-MPS, Iain Duncan Smith, Boris Johnson, or Sadiq Khan
Parlspeeches<-lapply(allxmls,getNodeSet,path="/publicwhip/speech[@speakername=\"Michael Gove\"]/p | 
                     /publicwhip/speech[@speakername=\"David Cameron\"]/p | 
                     /publicwhip/speech[@speakername=\"Boris Johnson\"]/p | 
                     /publicwhip/speech[@speakername=\"Liam Fox\"]/p | 
                     /publicwhip/speech[@speakername=\"Penny Mordaunt\"]/p | 
                     /publicwhip/speech[@speakername=\"Andrea Leadsom\"]/p | 
                     /publicwhip/speech[@speakername=\"Tim Farron\"]/p |
                     /publicwhip/speech[@speakername=\"David Davis\"]/p | 
                     /publicwhip/speech[@speakername=\"Yvette Cooper\"]/p | 
                     /publicwhip/speech[@speakername=\"Chris Grayling\"]/p |          
                     /publicwhip/speech[@speakername=\"Gisela Stuart\"]/p |
                     /publicwhip/speech[@speakername=\"George Osborne\"]/p |
                     /publicwhip/speech[@speakername=\"Iain Duncan Smith\"]/p | 
                     /publicwhip/speech[@speakername=\"Priti Patel\"]/p |
                     /publicwhip/speech[@speakername=\"Sadiq Khan\"]/p |
                     /publicwhip/speech[@speakername=\"Yvette Cooper\"]/p |
                     /publicwhip/speech[@speakername=\"Tim Farron\"]/p")
Parlspeeches<-Parlspeeches[-which(sapply(Parlspeeches, is.null))]
rm(allxmls)

##Extract metadata on speakername and timestamp (in id) for each segment and place into a df. Essentially creates a skeleton which knows how many segments are coming but
##doesn't have data for what they are
Parlmeta<-lapply(Parlspeeches, function(x) lapply(x, xmlParent))
Parlmeta<-lapply(Parlmeta, function(x) lapply(x, xmlAttrs))
Parlmeta<-lapply(Parlmeta, function(x) lapply(x, unlist))
allParl<-data.frame(speaker=unlist(sapply(Parlmeta, function(x) lapply(x, function(y) y[["speakername"]]))), 
                     days_to_go=unlist(sapply(Parlmeta, function(x) lapply(x, function(y) y[["id"]]))))
rm(Parlmeta)

##Add the text segments to their metadata in the dataframe
Parlspeeches<-unlist(Parlspeeches)
allParl$text<-as.character(lapply(Parlspeeches, xmlValue, recursive = TRUE, encoding = "UTF-8"))
rm(Parlspeeches)

##In each entry, convert timestamp into days until referendum
allParl$days_to_go<-regmatches(allParl$days_to_go, gregexpr(pattern = "[[:digit:]][[:digit:]][[:digit:]][[:digit:]]-[[:digit:]][[:digit:]]-[[:digit:]][[:digit:]]", text = allParl$days_to_go))
allParl$days_to_go<-substring(allParl$days_to_go,1,10)
allParl$days_to_go<-as.Date(allParl$days_to_go, format = "%Y-%m-%d")
allParl$days_to_go<-as.integer(as.Date("2016-06-23", format = "%Y-%m-%d")-allParl$days_to_go)

#There is now a dataframe with entries for every time a politician of interest spoke in parliament during the campaign period, with the text segment
#(period of uninterrupted speech), speaker's name, and number of days until the referendum attached
parlsentences <- strsplit(allParl$text, split = "(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?)\\s", perl=T) #finds pattern of .or?, followed by space, followed by capital.
parlsentences <- data.frame(speaker = rep(allParl$speaker, sapply(parlsentences, length)), text = unlist(parlsentences), days_to_go = rep(allParl$days_to_go, sapply(parlsentences, length)))


#### Marr PDFs in ####
##Creates a list of strings for each xml file name in the folder
filenames<-list.files("C:/Users/jaack/OneDrive - University Of Cambridge/Summer Political Psychology/Brexit Twitter/Data/Marr",full.names=T)

##Processes all files into a single corpus
MarrCorpus<-Corpus(URISource(filenames), readerControl = list(reader = readPDF))

##Extracts dates metadata into a list (possible regressor)
MarrDates<-lapply(MarrCorpus, function(dates) dates[["meta"]][["datetimestamp"]])
MarrDates<-lapply(MarrDates, grep, pattern = "[[:digit:]][[:digit:]][[:digit:]][[:digit:]]-[[:digit:]][[:digit:]]-[[:digit:]][[:digit:]]", value = TRUE)
MarrDates<-substring(MarrDates,1,10)
MarrDates<-as.Date(MarrDates, format = "%Y-%m-%d")
MarrDates<-as.integer(as.Date("2016-06-23", format = "%Y-%m-%d")-MarrDates)

##Remove white space
MarrCorpus<-tm_map(MarrCorpus, stripWhitespace)

##def and use new function to remove control character \r\n pattern
stripControlChar<-content_transformer(function(x) gsub("\r\n", "", x))
MarrCorpus<-tm_map(MarrCorpus, stripControlChar) #tm_map is a bit like sapply for Corpus

##Collapses each file in the corpus into a long string object (previously one object per page, now one per file)
allMarr<-list()
for (doc in 1:length(MarrCorpus)) {
  allMarr[doc]<-paste(MarrCorpus[[doc]][[1]], sep = '', collapse = '')
}
rm(MarrCorpus)

##Splits each text block back into a list, now one object per uninterrupted speech fragment.
breakInitials<-function(x) strsplit(x, "(?<=.)(?= [[:upper:]][[:upper:]]: )", perl = TRUE)
allMarr<-sapply(allMarr, breakInitials)

##Turns these lists into a dataframe
allMarr<-lapply(allMarr, as.data.frame, stringsAsFactors = FALSE)
allMarr<-bind_rows(allMarr, .id = "speaker")

##Clarify column name
allMarr<-rename(allMarr,"text" = "X[[i]]")

##Select only those cases where a study politician is speaking
allMarr<-allMarr[grepl(" DC: | YC: | DD: | NF: | TF: | AF: | LF: | MG: | CG: | BJ: | SK: | AL: | PM: | NS: | LW: ",allMarr$text)==TRUE,] #note this loses a few cases where initials have typos

##Add in dates (using speaker number as a proxy index before they are named
allMarr$days_to_go<-MarrDates[match(allMarr$speaker, c(1:17))]

##Match names to speaker numbers (in alphabetical order - the order of files read in)
allMarr$speaker<-recode_factor(allMarr$speaker, "1" = "David Cameron", "2" = "Yvette Cooper", "3" = "David Davis", 
                                 "4" = "Nigel Farage", "5" = "Nigel Farage", "6" = "Tim Farron",
                                 "7" = "Arlene Foster", "8" = "Liam Fox", "9" = "Michael Gove", "10" = "Michael Gove",
                                 "11" = "Chris Grayling", "12" = "Boris Johnson", "13" = "Sadiq Khan", "14" = "Andrea Leadsom",
                                 "15" = "Penny Mordaunt", "16" = "Nicola Sturgeon", "17" = "Leanne Wood", .default="NA", .ordered=FALSE)
allMarr$text<-substring(allMarr$text, 5)
marrsentences <- strsplit(allMarr$text, split = "(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?)\\s", perl=T) #finds pattern of .or?, followed by space, followed by capital.
marrsentences <- data.frame(speaker = rep(allMarr$speaker, sapply(marrsentences, length)), text = unlist(marrsentences), days_to_go = rep(allMarr$days_to_go, sapply(marrsentences, length)))

#### Twitter CSVs in ####
##Creates a list of strings for each csv file name in the folder
# filenames<-list.files("C:/Users/jaack/OneDrive - University Of Cambridge/Summer Political Psychology/Brexit Twitter/Data/Full Tweets",full.names=T)
# 
# ##Reads in those files, removes replies, and one useless and misbehaving variable, and turns them into a dataframe
# alltweets<-lapply(filenames,read.csv,stringsAsFactors = FALSE)
# alltweets<-lapply(alltweets,select,-is_retweet)
# alltweets<-bind_rows(alltweets, .id = "speaker")
# row.names(alltweets) <- 1:nrow(alltweets)

##Label speakers - they should default to being read in alphabetical order of twitter handle (note e.g. Arlene Foster = "dupleader"), but its good to check
# alltweets$speaker<-recode_factor(alltweets$speaker, "1" = "Andrea Leadsom", "2" = "Boris Johnson", "3" = "David Cameron", 
#               "4" = "David Davis", "5" = "Arlene Foster", "6" = "Frances O'Grady", "7" = "George Osborne", "8" = "Gisela Stuart",
#               "9" = "Leanne Wood", "10" = "Leave.EU", "11" = "Liam Fox", "12" = "Nicola Sturgeon", "13" = "Nigel Farage", "14" = "Penny Mordaunt", 
#               "15" = "Remain", "16" = "Ruth Davidson", "17" = "Sadiq Khan", "18" = "Tim Farron", "19" = "Vote Leave", .default="NA", .ordered=FALSE)

alltweets <- read.csv("all4.csv", encoding = "UTF-8")
alltweets$imagetext <- NA
##OCR
ocrengine = tesseract(language = "eng") #creates the OCR engine with default params
for (i in which(!is.na(alltweets$imagelink))) {
    print(alltweets$imagelink[i])
  
    try({ #Try loop used in case of URL request failure #1485
    inputimg <- image_read(alltweets$imagelink[i], strip = TRUE)
    image_write(inputimg, 
                path = paste("images/", substring(alltweets$imagelink[i],29,nchar(alltweets$imagelink[i])-4), ".png", sep = ""), 
                format = "png", 
                quality = 100,
                flatten = TRUE)
    
    processedimg <- inputimg %>%
      image_resize("2000x") %>%
      image_convert(type = "Grayscale")
    imgtext <- tesseract::ocr_data(processedimg, engine = ocrengine) #Run OCR on the URL at hand with the English engine     
    
    likelywords <- imgtext$word[imgtext$confidence>60] #strip out low-confidence guesses. Change this value if too few/many words are being picked up.
    likelywords <- unlist(lapply(likelywords, tolower)) #lowercase everything
    likelywords <- likelywords[nchar(likelywords)>1|likelywords=="a"|likelywords=="i"] #strip out non-word 1-letter signals (these are very common, e.g. | for straight lines)
    likelysentence <- paste(likelywords, collapse = " ") #collapse the list of found words into a single string
    alltweets$imagetext[i] <- if (length(likelysentence) == 0) NA else likelysentence #if there were any words found in the image, record them, else act as if there was no image at all
    print(likelysentence)
    })
}
#Delete OCR objects
rm(inputimg,likelysentence,likelywords,ocrengine,processedimg,imgtext)


##Rationalise creation date into days until referendum, replyname to a logical, and rename columns
alltweets$created_at<-substring(alltweets$created_at,5,11)
alltweets$created_at<-as.Date(alltweets$created_at, format = "%b %d") #defaults to 2020, but this doesn't matter if we pretend the referendum was this year (both 2020 and 2016 are leap years, though the campaign period was after February anyway)
alltweets$created_at<-as.integer(as.Date("2020-06-23", format = "%Y-%m-%d")-alltweets$created_at)
alltweets<-select(alltweets,  "author", "favourites" = "favorite_count", "retweets" = "retweet_count", "text", "imagetext", "reply" = "in_reply_to_screen_name", "days_to_go" = "created_at", "imagelink")
alltweets$reply<-as.logical(alltweets$reply != "" & is.na(alltweets$reply) == FALSE)
##Clarify NAs
alltweets$imagetext[alltweets$imagetext==""] <- NA
alltweets$imagetext[alltweets$imagetext=="NA"] <- NA
##remove tweets not from politicians for study
alltweets$author <- tolower(alltweets$author)
alltweets <- filter(alltweets, author == "andrealeadsom"| author == "borisjohnson"| author == "david_cameron"| 
                       author == "daviddavismp"| author == "dupleader"| author == "francesogrady"| author == "george_osborne"| 
                       author == "giselastuart"| author == "leannewood"| author == "leaveeuofficial"| author == "liamfox"| 
                       author == "nicolasturgeon"| author == "nigel_farage"| author == "pennymordaunt"| author == "peoplesvote_hq"|
                       author == "ruthdavidsonmsp"| author == "sadiqkhan"| author == "timfarron"| author == "vote_leave")

alltweets$speaker = recode_factor(alltweets$author, andrealeadsom = "Andrea Leadsom", borisjohnson = "Boris Johnson", 
                      david_cameron = "David Cameron", daviddavismp = "David Davis", dupleader = "Arlene Foster", 
                      francesogrady = "Frances O'Grady", george_osborne = "George Osborne", giselastuart = "Gisela Stuart",
                      leannewood = "Leanne Wood", leaveeuofficial = "Leave.EU", liamfox = "Liam Fox", 
                      nicolasturgeon = "Nicola Sturgeon", nigel_farage = "Nigel Farage", pennymordaunt = "Penny Mordaunt", 
                      peoplesvote_hq = "Remain", ruthdavidsonmsp = "Ruth Davidson", sadiqkhan = "Sadiq Khan", 
                      timfarron = "Tim Farron", vote_leave = "Vote Leave")
alltweets = select(alltweets, -author)

#### Create central df ####
#Standardisation
marrsentences$text<-as.character(marrsentences$text)
alltweets$text<-as.character(alltweets$text)
alltweets$imagetext<-as.character(alltweets$imagetext)
parlsentences$text<-as.character(parlsentences$text)

#Binding
# alltext<-bind_rows(allMarr, allParl, alltweets, .id = "medium")
alltext<-bind_rows(marrsentences,parlsentences,alltweets, .id = "medium")
alltext$medium<-recode_factor(alltext$medium, "1" = "Marr Interview", "2" = "Parliament", "3" = "Twitter")
#Remove duplicated segments (some exist due to errors in the base transcipt. Where two speakers have used the same text, or the same speaker has used it twice, this is retained and treated as a double-endorsement)
alltext<-alltext[-which(duplicated(alltext)),] #this doesn't refer to the $text column, so looks for completely duplicated entries including imagetext

#Create a variable to document OCR presence or absence
alltext$hasimage = !is.na(alltext$imagetext)
#create a new variable for the functional text of each segment by concatenating the shortened image text to the plain text, 
#and creating an exception for when no image text exists. Then split that text into individual words
alltext$countingtext <- paste(alltext$text, ifelse(is.na(alltext$imagetext), "", alltext$imagetext), sep = " ")
alltext$countingtext <- strsplit(alltext$countingtext, split = "\\s+")

#Drop web links. Almost certainly only affects Twitter but apply universally just in case
alltext$countingtext<-lapply(alltext$countingtext, gsub, pattern = "(?:(?:https?|ftp):\\/\\/)?[\\w/\\-?=%.]+\\.[\\w/\\-?=%.]+", replacement = "", perl = TRUE) #Remove URLs

#Remove any segments not referring to the EU in either text *or* image text
brexhitwords = "\\bEU(?!\\w)|Brexit|Euro|EU *Ref|Leav|Remain|23 *June|June *23|stronger *in|take *back *control|breaking *point|single *market|350 *m|In *Campaign|Out *Campaign|Single Currency|Schengen|Juncker|Believe *in *Britain|We *Want *Our *Country *Back|Stronger *Safer *and *Better *Off|membership|EU *member|member *state|Brussels|common *market|lexit|stay in|in *or *out|BBC *Debate"
brexhitsegments = unique(c(grep(brexhitwords,alltext$text,ignore.case = TRUE, perl = TRUE), grep(brexhitwords,alltext$imagetext,ignore.case = TRUE, perl = TRUE)))
alltext<-alltext[brexhitsegments,]

#Remove old objects
rm(doc,filenames,MarrDates,stripControlChar,breakInitials,brexhitwords,brexhitsegments)

#### Adjust word counts ####
#Count the pre-split 'counting text' column to get word counts
alltext$wordcount <- lengths(alltext$countingtext) 
#Check distributions of word count. Twitter has clear outliers, caused by extensive OCR entries.
summary(alltext$wordcount)
summary(filter(alltext,medium == "Twitter")$wordcount)
summary(filter(alltext,medium == "Marr Interview")$wordcount)
summary(filter(alltext,medium == "Parliament")$wordcount)

#Crop excessive OCR entries by ensuring no text has more than 100 words
alltext$countingtext <- lapply(alltext$countingtext, '[', 1:100) #who knew that's how to subset in lapply!
alltext$countingtext <- lapply(alltext$countingtext, na.omit) #required because subsetting above the existing length of a list imputs NAs
#re-run wordcount
alltext$wordcount <- lengths(alltext$countingtext) 
#Check distributions again, now Twitter is more in line. They don't need to be identical, just close enough to be 'fair analogues'.
summary(filter(alltext,medium == "Twitter")$wordcount)
summary(filter(alltext,medium == "Marr Interview")$wordcount)
summary(filter(alltext,medium == "Parliament")$wordcount)

#### Input person regressors####
index <- names(table(alltext$speaker))
#The labels below are contingent on the order of the speakers read in above THIS MUST BE DOUBLE CHECKED WHEN RUN BECAUSE IT CAN EASILY WARP RESULTS!
Brexiteer <- c(T,T,T,T,F,T,F,F,T,F,T,T,T,F,T,T,T,F,F,F,F,T,F)
Conservative <- c(T,F,T,T,T,T,F,T,F,F,F,T,T,F,F,T,T,F,T,F,F,F,F)
Labour <- c(F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F,F,F,T,F,F,T)
alltext$brexiteer <- Brexiteer[match(alltext$speaker, index)]
alltext$conservative <- Conservative[match(alltext$speaker, index)]
alltext$labour <- Labour[match(alltext$speaker, index)]
rm(Brexiteer,Conservative,Labour,index)

#### Data verification ####
##Verify sample manually
indices<-as.list(sample(1:length(alltext$text),50,replace = FALSE))
for (j in 1:length(indices)) {
  print(alltext[indices[[j]],])
}
#RESULT:50 out of 50 are correctly identified. Noteworthy that many tweets are quotes from other campaigners

##Check remain tags
Remaintagged<-alltext[grep("StrongerIN|Stronger In", alltext$countingtext, ignore.case = TRUE),]
table(Remaintagged$brexiteer)
print(Remaintagged$countingtext[which(Remaintagged$brexiteer)])
#RESULT: 0/387 are written by leavers

##Check Leave tags
Leavetagged<-alltext[grep("VoteLeave|TakeControl|TakeBackControl", alltext$countingtext, ignore.case = TRUE),]
table(Leavetagged$brexiteer)
print(Leavetagged$countingtext[-which(Leavetagged$brexiteer)])
#RESULT: 0/775 are written by remainers

rm(Leavetagged,Remaintagged,indices,j)

#### Calculate MFT Scores ####
Care <- c("safe|peace|compassion|empath|sympath|care(?!\\w)|caring(?!\\w)|protect|shield(?!\\w)|shelter(?!\\w)|amity(?!\\w)|secur|benefit|defen|guard|preserve(?!\\w)|harm|suffer|war(?!\\w)|wars(?!\\w)|warl|warring(?!\\w)|fight|violen|hurt|kill(?!\\w)|kills(?!\\w)|killer|killed(?!\\w)|killing(?!\\w)|endanger|cruel|brutal|abuse|damag|ruin|ravage(?!\\w)|detriment|crush|attack|annihilate|destroy(?!\\w)|stomp(?!\\w)|abandon|spurn(?!\\w)|impair(?!\\w)|exploit(?!\\w)|exploits(?!\\w)|exploited(?!\\w)|exploiting(?!\\w)|wound")
Fair <- c("fair(?!\\w)|fairly(?!\\w)|fairness(?!\\w)|fair-|fairmind|fairplay(?!\\w)|equal|justice(?!\\w)|justness(?!\\w)|justifi|reciproc|impartial|egalitar|rights(?!\\w)|equity(?!\\w)|evenness(?!\\w)|equivalent(?!\\w)|unbias|tolerant(?!\\w)|equable(?!\\w)|balance|homologous(?!\\w)|unprejudice|reasonable(?!\\w)|constant(?!\\w)|honest|unfair|unequal|bias|unjust|injust|bigot|disciminat|disproportion|inequitable(?!\\w)|prejud|dishonest(?!\\w)|unscrupulous(?!\\w)|dissociate(?!\\w)|preference(?!\\w)|favoritism(?!\\w)|segregat|exclusion(?!\\w)|exclud")
Loyal <-c("abandon|segregat|together(?!\\w)|nation|homeland|family(?!\\w)|families(?!\\w)|familial(?!\\w)|group(?!\\w)|loyal|patriot|communal(?!\\w)|commune|communit|communis|comrad|cadre(?!\\w)|collectiv|joint(?!\\w)|unison(?!\\w)|unite|fellow|guild(?!\\w)|solidarity(?!\\w)|devot|member(?!\\w)|cliqu|cohort(?!\\w)|ally(?!\\w)|insider(?!\\w)|foreign|enem|betray|treason|traitor|treacher|disloyal|individual|apostasy(?!\\w)|apostate(?!\\w)|deserted(?!\\w)|deserter|deserting(?!\\w)|deceiv|jilt|imposter(?!\\w)|miscreant(?!\\w)|spy(?!\\w)|sequester(?!\\w)|renegade(?!\\w)|terroris|immigra")
Auth <- c("preserve(?!\\w)|loyal|betray|treason|traitor|treacher|disloyal|apostasy(?!\\w)|apostate(?!\\w)|deserted|deserter|deserting|obey|obedien|duty(?!\\w)|law(?!\\w)|legal|duti|honor|respect(?!\\w)|respectful|respected(?!\\w)|respects(?!\\w)|order|father|mother(?!\\w)|motherl|mothering(?!\\w)|mothers(?!\\w)|tradition|hierarch|authorit|permit(?!\\w)|permission(?!\\w)|status|rank|leader|class(?!\\w)|bourgeoisie(?!\\w)|caste|position(?!\\w)|complian|command(?!\\w)|supremacy(?!\\w)|control(?!\\w)|submi|allegian|serve(?!\\w)|abide(?!\\w)|defere|defer(?!\\w)|revere|venerat|comply(?!\\w)|defian|rebel|dissent|subver|disrespect|disobe|sediti|agitat|insubordinat|illegal|insurgent(?!\\w)|mutinous(?!\\w)|defy|dissident(?!\\w)|unfaithful(?!\\w)|alienate(?!\\w)|defector(?!\\w)|heretic|nonconformist(?!\\w)|oppose(?!\\w)|protest(?!\\w)|refuse(?!\\w)|denounce(?!\\w)|remonstrate(?!\\w)|riot|obstruct(?!\\w)")
Pure <- c("preserve(?!\\w)|ruin|exploit(?!\\w)|exploits(?!\\w)|exploited(?!\\w)|exploiting(?!\\w)|apostasy(?!\\w)|apostate(?!\\w)|heretic|piety(?!\\w)|pious(?!\\w)|purity(?!\\w)|pure|clean|steril|sacred|chast|holy(?!\\w)|holiness(?!\\w)|saint|wholesome|celiba|abstention(?!\\w)|virgin(?!\\w)|virgins(?!\\w)|virginity(?!\\w)|virginal(?!\\w)|austerity(?!\\w)|integrity(?!\\w)|modesty(?!\\w)|abstinen|abstemiousness(?!\\w)|upright(?!\\w)|limpid(?!\\w)|unadulterated(?!\\w)|maiden(?!\\w)|virtuous(?!\\w)|refined(?!\\w)|decen|immaculate(?!\\w)|innocent(?!\\w)|pristine(?!\\w)|church|disgust|deprav|disease|unclean|contagio|indecen|sin(?!\\w)|sinful|sinner|sins(?!\\w)|sinned(?!\\w)|sinning(?!\\w)|slut|whore(?!\\w)|dirt|impiety(?!\\w)|impious(?!\\w)|profan|gross(?!\\w)|repuls|sick|promiscu|lewd|adulter|debauche|defile|tramp(?!\\w)|prostitut|unchaste(?!\\w)|intemperate(?!\\w)|wanton(?!\\w)|profligate(?!\\w)|filth|trashy(?!\\w)|obscen|lax(?!\\w)|taint|stain|tarnish|debase|desecrat|wicked|blemish(?!\\w)|exploitat|pervert(?!\\w)|wretched")
Lib <-c("exploit(?!\\w)|exploits(?!\\w)|exploited(?!\\w)|exploting(?!\\w)|rights(?!\\w)|obey|obedient|duti|order|supremacy(?!\\w)|control(?!\\w)|submi|serve(?!\\w)|abide(?!\\w)|defere|defer(?!\\w)|defian|rebel|dissent|subver|disobe|defy|defector(?!\\w)|nonconformist(?!\\w)|protest(?!\\w)|free(?!\\w)|freedom(?!\\w)|liberty(?!\\w)|autonom|choice(?!\\w)|choose(?!\\w)|liberate(?!\\w)|liberation(?!\\w)|sovereign|independent(?!\\w)|independence(?!\\w)|dictat|totalitar|coerc|authoritarian|tyran")

alltext$care <- as.logical(lapply(lapply(alltext$countingtext, grepl, pattern = Care, ignore.case = TRUE, perl = TRUE), any))
alltext$fair <- as.logical(lapply(lapply(alltext$countingtext, grepl, pattern = Fair, ignore.case = TRUE, perl = TRUE), any))
alltext$loyal <- as.logical(lapply(lapply(alltext$countingtext, grepl, pattern = Loyal, ignore.case = TRUE, perl = TRUE), any))
alltext$auth <- as.logical(lapply(lapply(alltext$countingtext, grepl, pattern = Auth, ignore.case = TRUE, perl = TRUE), any))
alltext$pure <- as.logical(lapply(lapply(alltext$countingtext, grepl, pattern = Pure, ignore.case = TRUE, perl = TRUE), any))
alltext$lib <- as.logical(lapply(lapply(alltext$countingtext, grepl, pattern = Lib, ignore.case = TRUE, perl = TRUE), any))
alltext$anymoral <- as.logical(ifelse(alltext$care == TRUE|alltext$fair == TRUE|alltext$loyal == TRUE|alltext$auth == TRUE|alltext$pure == TRUE|
                                      alltext$lib == TRUE, "TRUE", "FALSE"))
rm(Auth,Care,Fair,Lib,Loyal,Pure)

#### Export for Gorilla Context Analysis ####
gorillabase <- pivot_longer(alltext, cols = c("care","fair","loyal","auth","pure","lib","anymoral"), names_to = "Foundation", values_to = "ispresent")
gorillabase <- filter(gorillabase, ispresent == TRUE)  
gorillabase <- select(gorillabase, -c(ispresent, countingtext))
gorillabase$Foundation <- recode_factor(gorillabase$Foundation, "care" = "Care", "fair" = "Fairness", "loyal" = "Loyalty", "pure" = "Purity", "auth" = "Authority", "lib" = "Liberty")
gorillabase <- filter(gorillabase, Foundation !="anymoral")
for (i in 1:length(gorillabase$imagelink)) {
  if (is.na(gorillabase$imagelink[i]) == FALSE && grepl("video", gorillabase$imagelink[i]) == FALSE) {
    gorillabase$imagelink[i] <- paste(substring(gorillabase$imagelink[i],29,nchar(gorillabase$imagelink[i])-4), ".png", sep = "")
  } else {
    gorillabase$imagelink[i] <- NA
  }
}
write.csv(gorillabase, "gorillaexport.csv")

#### Gorilla Context Analysis ####
contexts = read.csv("contexts.csv")
#Remove non-data events
contexts = dplyr::filter(contexts, Zone.Name == "Otherexplain" | Zone.Name == "Endorsement" | Zone.Name == "IsSlogan" | Zone.Name == "Outofcontext" | Zone.Name == "Rejection")
#Rename columns
contexts = rename(contexts, ReviewerNumber = ï..Participant.Private.ID, Excerpt = Spreadsheet.Row)
#Recode data
contexts[contexts$Response == "",]$Response = NA
contexts[contexts$ReviewerNumber == 1880680,]$ReviewerNumber = 1
contexts[contexts$ReviewerNumber == 1924705,]$ReviewerNumber = 2
contexts[is.na(contexts$Response),]$Response = TRUE
#Pivot
contexts = pivot_wider(contexts, names_from = Zone.Name, values_from = Response)
contexts$Endorsement = as.logical(contexts$Endorsement)
contexts$Outofcontext = as.logical(contexts$Outofcontext)
contexts$Rejection = as.logical(contexts$Rejection)

#where a button wasn't pressed, implicitly false
contexts$IsSlogan = ifelse(is.na(contexts$IsSlogan), FALSE, TRUE)
contexts[is.na(contexts$Otherexplain),]$Otherexplain = FALSE
contexts[is.na(contexts$Endorsement),]$Endorsement = FALSE
contexts[is.na(contexts$Outofcontext),]$Outofcontext = FALSE
contexts[is.na(contexts$Rejection),]$Rejection = FALSE

#Merge rows referring to the same excerpt
contexts = contexts %>%
  group_by(Excerpt, ReviewerNumber) %>% 
  fill(IsSlogan,Otherexplain,Endorsement,Outofcontext,Rejection, .direction = "downup")
contexts = unique(contexts2)
contexts = ungroup(contexts2)

metadata = data.frame(Totalacc = 1)
metadata$Totalacc = 100 * table(contexts$Endorsement)["TRUE"]/length(contexts$Endorsement)
metadata$R1acc = 100 * table(filter(contexts, ReviewerNumber == 1)$Endorsement)["TRUE"]/nrow(filter(contexts, ReviewerNumber == 1))
metadata$R2acc = 100 * table(filter(contexts, ReviewerNumber == 2)$Endorsement)["TRUE"]/nrow(filter(contexts, ReviewerNumber == 2))
metadata$twotickacc


R1 = select(filter(contexts, ReviewerNumber == 1), Excerpt, Outofcontext, Endorsement, Rejection)
R2 = select(filter(contexts, ReviewerNumber == 2), Excerpt, Outofcontext, Endorsement, Rejection)
R1 = transmute(R1, response = ifelse(Endorsement == TRUE, 1, ifelse(Outofcontext == TRUE, 2, 3)), Excerpt = Excerpt) #1 = Endorse, 2 = OOC, 3 = Reject
R2 = transmute(R2, response = ifelse(Endorsement == TRUE, 1, ifelse(Outofcontext == TRUE, 2, 3)), Excerpt = Excerpt)

Agreement = left_join(R1, R2, by = "Excerpt", suffix = c("_R1", "_R2"))
Agreement$response_R1 = factor(Agreement$response_R1, levels = c("1", "2", "3"))
Agreement$response_R2 = factor(Agreement$response_R2, levels = c("1", "2", "3"))

Agreementtable = table(Agreement$response_R1, Agreement$response_R2)

metadata$twotickacc = 100 * Agreementtable[1,1]/sum(Agreementtable)
metadata$InterRaterReliability =  cohen.kappa(Agreementtable, alpha=.05)$kappa
#### Models ####
LogReg <- function(DV, model) {
  ##ASSUMPTION CHECKS
  #Independent variable is binary
  if (typeof(DV)!="logical") {
    return("ERROR: Non-logical response variable")
  }

  ##TEST
  #Univariate
  if (model == "univariate") {
    model <- glm(family = binomial(link = "logit"), DV ~ brexiteer, data = alltext)
    return(model)
  }
  #Multivariate - person predictors
  if (model == "person") {
    model <- glm(family = binomial(link = "logit"), DV ~ brexiteer + conservative + labour, data = alltext)
    if (any(vif(model)>5)) {
      print(vif(model))
      return("MODEL NOT RUN - Multicollinearity violation")
    } else {
      return(model)
    }
  }
  #Multivariate - context and person predictors
  if (model == "context") {
    model <- glm(family = binomial(link = "logit"), DV ~ brexiteer + conservative + labour + medium + days_to_go + hasimage, data = alltext)
    if (any(vif(model)>5)) {
      print(vif(model))
      return("MODEL NOT RUN - Multicollinearity violation")
    } else {
      return(model)
    }
  }
  #Multivariate - leavecamps
  if (model == "leavecamps") {
    model <- glm(family = binomial(link = "logit"), DV ~ care + fair + auth + loyal + pure + lib + days_to_go, data = leavecamps)
    if (any(vif(model)>5)) {
      print(vif(model))
      return("MODEL NOT RUN - Multicollinearity violation")
    } else {
      return(model)
    }
  }
}

LinReg <- function(DV, model) {
  ##ASSUMPTION CHECKS
  #Independent variable is binary
  if (class(DV)!="numeric") {
    return("ERROR: Non-numeric response variable")
  }
  
  ##TEST
  #Multivariate - framings
  if (model == "twitterframes") {
    model <- lm(DV ~ care + fair + loyal + auth + lib + pure, data = allTwitter)
    
    if (any(vif(model)>5)) {
      print(vif(model))
      return("MODEL NOT RUN - Multicollinearity violation")
    } else {
      return(model)
    }
  }
  
  #Multivariate - framings
  if (model == "twitterall") {
    model <- lm(DV ~ care + fair + loyal + auth + lib + pure + days_to_go + reply + hasimage, data = allTwitter)
    
    if (any(vif(model)>5)) {
      print(vif(model))
      return("MODEL NOT RUN - Multicollinearity violation")
    } else {
      return(model)
    }
  }
}

##Run univariate regressions
unicare <- LogReg(alltext$care, "univariate")
unifair <- LogReg(alltext$fair, "univariate")
uniloyal <- LogReg(alltext$loyal, "univariate")
uniauth <- LogReg(alltext$auth, "univariate")
unipure <- LogReg(alltext$pure, "univariate")
unilib <- LogReg(alltext$lib, "univariate")
##Run person-level regressions
personcare <- LogReg(alltext$care, "person")
personfair <- LogReg(alltext$fair, "person")
personloyal <- LogReg(alltext$loyal, "person")
personauth <- LogReg(alltext$auth, "person")
personpure <- LogReg(alltext$pure, "person")
personlib <- LogReg(alltext$lib, "person")
##Run context-level regressions
contextcare <- LogReg(alltext$care, "context")
contextfair <- LogReg(alltext$fair, "context")
contextloyal <- LogReg(alltext$loyal, "context")
contextauth <- LogReg(alltext$auth, "context")
contextpure <- LogReg(alltext$pure, "context")
contextlib <- LogReg(alltext$lib, "context")

unicare$aic<-round(unicare$aic)
unifair$aic<-round(unifair$aic)
uniloyal$aic<-round(uniloyal$aic)
uniauth$aic<-round(uniauth$aic)
unipure$aic<-round(unipure$aic)
unilib$aic<-round(unilib$aic)
contextcare$aic<-round(contextcare$aic)
contextfair$aic<-round(contextfair$aic)
contextloyal$aic<-round(contextloyal$aic)
contextauth$aic<-round(contextauth$aic)
contextpure$aic<-round(contextpure$aic)
contextlib$aic<-round(contextlib$aic)
personcare$aic<-round(personcare$aic)
personfair$aic<-round(personfair$aic)
personloyal$aic<-round(personloyal$aic)
personauth$aic<-round(personauth$aic)
personpure$aic<-round(personpure$aic)
personlib$aic<-round(personlib$aic)

stargazer(alltext, type = "html", keep = c("days_to_go","favourites","retweets"), omit.summary.stat = c("min","max"), out = "tables/summary.html", digits = 0)

##Get Univariate logistic regression table
stargazer(unicare,unifair,uniloyal,uniauth,unipure,unilib,
          type = "html", title = "Fig. 1 Univariate Models", align = TRUE, out = "tables/model1.html", omit.stat = c("ll"), 
          dep.var.labels = "Moral Foundation",
          column.labels = c("Care","Fairness","Loyalty","Authority","Purity","Liberty"))
rm(unicare,unifair,uniloyal,uniauth,unipure,unilib)
##Get Person-level Multivariate logistic regression table
stargazer(personcare,personfair,personloyal,personauth,personpure,personlib,
          type = "html", title = "Fig. 2 Person-level multivariate Models", align = TRUE, out = "tables/model2.html", omit.stat = c("ll"),
          dep.var.labels = "Moral Foundation",
          column.labels = c("Care","Fairness","Loyalty","Authority","Purity","Liberty"))
rm(personcare,personfair,personloyal,personauth,personpure,personlib)
##Get Context-level Multivariate logistic regression table
stargazer(contextcare,contextfair,contextloyal,contextauth,contextpure,contextlib,
          type = "html", title = "Fig. 3 Context-level multivariate Models", align = TRUE, out = "tables/model3.html", omit.stat = c("ll"),
          dep.var.labels = "Moral Foundation",
          column.labels = c("Care","Fairness","Loyalty","Authority","Purity","Liberty"))
##Make exponent-coefficient tables
stargazer(contextcare,contextfair,contextloyal,contextauth,contextpure,contextlib,
          type = "html", title = "Fig. 4 Final model coefficient exponents", align = TRUE, out = "tables/table4.html", 
          omit.stat = c("ll","aic"), dep.var.labels = "Moral Foundation", apply.coef = function(x) exp(x), report = "vc", 
          omit.table.layout = "sn", column.labels = c("Care","Fairness","Loyalty","Authority","Purity","Liberty"), omit = "Constant")
rm(contextcare,contextfair,contextloyal,contextauth,contextpure,contextlib)
##Compare Leave.EU and Vote Leave
leavecamps<-filter(alltext, speaker == "Vote Leave"|speaker == "Leave.EU")
leavecamps$leaveeu<-ifelse(leavecamps$speaker == "Leave.EU",TRUE,FALSE)
leavecampsmodel<-LogReg(leavecamps$leaveeu, model = "leavecamps")
stargazer(leavecampsmodel,
          type = "html", title = "Fig. 5 Leave camps differentiated", align = TRUE, out = "tables/model4.html", 
          omit.stat = c("ll"), dep.var.labels.include = FALSE, 
          covariate.labels = c("Care","Fairness","Loyalty","Authority","Purity","Liberty"),
          column.labels = c("Tweet is from Leave.EU rather than Vote Leave"))

##Z score favourites and retweets
#Get person means
allTwitter = filter(alltext, medium == "Twitter")
personmeans <- group_by(allTwitter,speaker) %>%
  summarise(meanfavs = mean(favourites), meanretweets = mean(retweets), sdfavs = sd(favourites), sdretweets = sd(retweets))
allTwitter = left_join(allTwitter,personmeans)
#Get z-scores
allTwitter$zfavs = (log(allTwitter$favourites+1) - log(allTwitter$meanfavs+1))/log(allTwitter$sdfavs+1)
allTwitter$zrts = (log(allTwitter$retweets+1) - log(allTwitter$meanretweets+1))/log(allTwitter$sdretweet+1)
##Model tweet popularity
stargazer(LinReg(allTwitter$zfavs, model = "twitterframes"), LinReg(allTwitter$zfavs, model = "twitterall"), LinReg(allTwitter$zrts, model = "twitterframes"),
          LinReg(allTwitter$zrts, model = "twitterall"), type = "html", title = "Fig. 6 Multivariate Linear Models", align = TRUE, out = "tables/model5.html",
          table.placement = "h", column.labels = c("Favourites","Favourites","Retweets","Retweets"))
rm(allTwitter,leavecamps,leavecampsmodel,personmeans)


#### Graphics ####
plotdata = alltext %>%
  group_by(brexiteer) %>% 
  summarise(Care = sum(care == TRUE) / n(),
            Fairness = sum(fair == TRUE) / n(),
            Loyalty = sum(loyal == TRUE) / n(),
            Authority = sum(auth == TRUE) / n(),
            Purity = sum(pure == TRUE) / n(),
            Liberty = sum(lib == TRUE) / n())

plotdata = as.data.frame(t(as.matrix(plotdata, ncol = 6)))
colnames(plotdata) <- c("Remain", "Leave")
plotdata <- plotdata[-1,]
plotdata = plotdata * 100
plotdata$foundation = rownames(plotdata)
plotdata = pivot_longer(plotdata, cols = c("Remain", "Leave"), names_to = "Campaign")

library(ggplot2)
foundplot = ggplot(plotdata, aes(x = foundation, y = value, fill = Campaign)) + 
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Comparative Frequency of Framings", x = "Foundation", y = "Frequency (% of segments)") +
  scale_fill_manual(values = c("firebrick", "steelblue1")) +
  scale_y_continuous(breaks = c(0,5,10,15,20,25)) +
  theme_classic()
print(foundplot)
ggsave("foundplot.jpg", plot = foundplot, device = "jpeg")
