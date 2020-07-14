#### Denotes code segment
## Explains process below
# Provides note


#### Directory, Files, Libraries####
##Clears the environment, sets the working directory, calls the libraries
rm(list = ls())
setwd("C:/Users/jaack/OneDrive - University Of Cambridge/Summer Political Psychology/Brexit Twitter/Data")
library('psych') #statistical tools
library('dplyr') #general data handling
library('XML') #xml handling
library('methods')#
library('pdftools') #pdf handling
library('tm') #textmining package
library('SnowballC') #required for some Corpus cleaning functions
library('car') #bonus regression tools
library('ResourceSelection') #Hosmer Lemeshow test
library('stargazer') #nice regression tables

#### Hansard XMLs in ####
##Creates a list of strings for each xml file name in the folder
filenames<-list.files("C:/Users/jaack/OneDrive - University Of Cambridge/Dissertation/Data/Hansard",full.names=T)

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
allParl$text<-lapply(Parlspeeches, xmlValue, recursive = TRUE)
rm(Parlspeeches)

##In each entry, convert timestamp into days until referendum
allParl$days_to_go<-regmatches(allParl$days_to_go, gregexpr(pattern = "[[:digit:]][[:digit:]][[:digit:]][[:digit:]]-[[:digit:]][[:digit:]]-[[:digit:]][[:digit:]]", text = allParl$days_to_go))
allParl$days_to_go<-substring(allParl$days_to_go,1,10)
allParl$days_to_go<-as.Date(allParl$days_to_go, format = "%Y-%m-%d")
allParl$days_to_go<-as.integer(as.Date("2016-06-23", format = "%Y-%m-%d")-allParl$days_to_go)

#There is now a dataframe with entries for every time a politician of interest spoke in parliament during the campaign period, with the text segment
#(period of uninterrupted speech), speaker's name, and number of days until the referendum attached


#### Marr PDFs in ####
##Creates a list of strings for each xml file name in the folder
filenames<-list.files("C:/Users/jaack/OneDrive - University Of Cambridge/Dissertation/Data/Marr",full.names=T)

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
stripControlChar <- content_transformer(function(x) gsub("\r\n", "", x))
MarrCorpus<-tm_map(MarrCorpus, stripControlChar)

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

#### Twitter CSVs in ####
#Creates a list of strings for each csv file name in the folder
filenames<-list.files("C:/Users/jaack/OneDrive - University Of Cambridge/Dissertation/Data/Tweets",full.names=T)

#Reads in those files, removes one useless and misbehaving variable, and turns them into a dataframe
alltweets<-lapply(filenames,read.csv,stringsAsFactors = FALSE)
alltweets<-lapply(alltweets,select,-is_retweet)
alltweets<-bind_rows(alltweets, .id = "speaker")
#Label speakers
alltweets$speaker<-recode_factor(alltweets$speaker, "1" = "Andrea Leadsom", "2" = "Boris Johnson", "3" = "David Cameron", 
              "4" = "David Davis", "5" = "Arlene Foster", "6" = "Frances O'Grady", "7" = "George Osborne", "8" = "Gisela Stuart",
              "9" = "Leanne Wood", "10" = "Leave.EU", "11" = "Liam Fox", "12" = "Nicola Sturgeon", "13" = "Nigel Farage", "14" = "Penny Mordaunt", 
              "15" = "Remain", "16" = "Ruth Davidson", "17" = "Sadiq Khan", "18" = "Tim Farron", "19" = "Vote Leave", .default="NA", .ordered=FALSE)
#Remove punctuation
alltweets$text<-lapply(alltweets$text, removePunctuation, ucp = TRUE)
#Rationalise creation date into days until referendum, replyname to a logical, and rename columns
alltweets$created_at<-substring(alltweets$created_at,5,11)
alltweets$created_at<-as.Date(alltweets$created_at, format = "%b %d") #defaults to 2020, but this doesn't matter if we pretend the referendum was this year (both 2020 and 2016 are leap years, though the campaign period was after February anyway)
alltweets$created_at<-as.integer(as.Date("2020-06-23", format = "%Y-%m-%d")-alltweets$created_at)
alltweets<-rename(alltweets, "favourites" = "favorite_count", "reply" = "in_reply_to_screen_name", "days_to_go" = "created_at", 
                  "retweets" = "retweet_count", "tweet_ID" = "id_str")
alltweets$reply<-as.logical(alltweets$reply != "" & is.na(alltweets$reply) == FALSE)


#### Bring it together ####
#Standardisation
allMarr$text<-as.character(allMarr$text)
alltweets$text<-as.character(alltweets$text)
allParl$text<-as.character(allParl$text)
alltext<-bind_rows(allMarr, allParl, alltweets, .id = "medium")
alltext$medium<-recode_factor(alltext$medium, "1" = "Marr Interview", "2" = "Parliament", "3" = "Twitter")

#Drop @s and web links. Almost certainly only affects Twitter but apply universally just in case
alltext$text<-lapply(alltext$text, gsub, pattern = "https:.*?[[:space:]]", replacement = "")
alltext$text<-lapply(alltext$text, gsub, pattern = "https:.*?$", replacement = "") #Any remaining web links did not have a space afterwards so must be the end of string, hence delete for this case
alltext$text<-lapply(alltext$text, gsub, pattern = "@.*?\\>", replacement = "") #remove @tags

#Resolve apostrophe unicode error
alltext$text<-gsub("â€™","'",alltext$text)

#Remove punctuation
alltext$text<-as.list(removePunctuation(unlist(alltext$text), ucp =TRUE))

#Remove any segments not referring to the EU
alltext<-alltext[c(grep(" EU |Brexit|Euro|Referendum|Leave|Remain|Campaign|Exit|Stay|Single Market|Single Currency|350 million|Schengen|Juncker|23 June|June 23|UKIP|member|Stronger In|Take Back Control|takebackcontrol|passporting|Believe in Britain|We Want Our Country Back|Stronger Safer and Better Off|strongerin|renegotiation|breaking point|Australia|Pointsbased|Dyson|sovereign|national level|Brussels",alltext$text,ignore.case = TRUE)),]

#Remove blank entries
alltext<-filter(alltext, text!= "")

#Remove duplicated segments (some exist due to errors in the base transcipt)
alltext<-alltext[-which(print(duplicated(alltext))),]

#Remove old objects
rm(allMarr,allParl,alltweets,doc,filenames,MarrDates,stripControlChar,breakInitials)

####Input person regressors####
index <- names(table(alltext$speaker))
Brexiteer <- c(TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)
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
Remaintagged<-alltext[grep("StrongerIN|Stronger In", alltext$text, ignore.case = TRUE),]
table(Remaintagged$brexiteer)
print(Remaintagged$text[which(Remaintagged$brexiteer)])
#RESULT: 34/1703 are Leave-side tweets, all can be manually verified as pro-Leave sentiment

##Check Leave tags
Leavetagged<-alltext[grep("VoteLeave|TakeControl|TakeBackControl", alltext$text, ignore.case = TRUE),]
table(Leavetagged$brexiteer)
print(Leavetagged$text[-which(Leavetagged$brexiteer)])
#RESULT: 189/1173 are Remain-side tweets, all can be manually verified as pro-Remain sentiment. Lots of �350M bashing.

rm(Leavetagged,Remaintagged,indices,j)

#### Calculate MFT Scores ####
Care <- c("safe|peace|compassion|empath|sympath|care(?!\\w)|caring(?!\\w)|protect|shield(?!\\w)|shelter(?!\\w)|amity(?!\\w)|secur|benefit|defen|guard|preserve(?!\\w)|harm|suffer|war(?!\\w)|wars(?!\\w)|warl|warring(?!\\w)|fight|violen|hurt|kill(?!\\w)|kills(?!\\w)|killer|killed(?!\\w)|killing(?!\\w)|endanger|cruel|brutal|abuse|damag|ruin|ravage(?!\\w)|detriment|crush|attack|annihilate|destroy(?!\\w)|stomp(?!\\w)|abandon|spurn(?!\\w)|impair(?!\\w)|exploit(?!\\w)|exploits(?!\\w)|exploited(?!\\w)|exploiting(?!\\w)|wound")
Fair <- c("fair(?!\\w)|fairly(?!\\w)|fairness(?!\\w)|fair-|fairmind|fairplay(?!\\w)|equal|justice(?!\\w)|justness(?!\\w)|justifi|reciproc|impartial|egalitar|rights(?!\\w)|equity(?!\\w)|evenness(?!\\w)|equivalent(?!\\w)|unbias|tolerant(?!\\w)|equable(?!\\w)|balance|homologous(?!\\w)|unprejudice|reasonable(?!\\w)|constant(?!\\w)|honest|unfair|unequal|bias|unjust|injust|bigot|disciminat|disproportion|inequitable(?!\\w)|prejud|dishonest(?!\\w)|unscrupulous(?!\\w)|dissociate(?!\\w)|preference(?!\\w)|favoritism(?!\\w)|segregat|exclusion(?!\\w)|exclud")
Loyal <-c("abandon|segregat|together(?!\\w)|nation|homeland|family(?!\\w)|families(?!\\w)|familial(?!\\w)|group(?!\\w)|loyal|patriot|communal(?!\\w)|commune|communit|communis|comrad|cadre(?!\\w)|collectiv|joint(?!\\w)|unison(?!\\w)|unite|fellow|guild(?!\\w)|solidarity(?!\\w)|devot|member(?!\\w)|cliqu|cohort(?!\\w)|ally(?!\\w)|insider(?!\\w)|foreign|enem|betray|treason|traitor|treacher|disloyal|individual|apostasy(?!\\w)|apostate(?!\\w)|deserted(?!\\w)|deserter|deserting(?!\\w)|deceiv|jilt|imposter(?!\\w)|miscreant(?!\\w)|spy(?!\\w)|sequester(?!\\w)|renegade(?!\\w)|terroris|immigra")
Auth <- c("preserve(?!\\w)|loyal|betray|treason|traitor|treacher|disloyal|apostasy(?!\\w)|apostate(?!\\w)|deserted|deserter|deserting|obey|obedien|duty(?!\\w)|law(?!\\w)|legal|duti|honor|respect(?!\\w)|respectful|respected(?!\\w)|respects(?!\\w)|order|father|mother(?!\\w)|motherl|mothering(?!\\w)|mothers(?!\\w)|tradition|hierarch|authorit|permit(?!\\w)|permission(?!\\w)|status|rank|leader|class(?!\\w)|bourgeoisie(?!\\w)|caste|position(?!\\w)|complian|command(?!\\w)|supremacy(?!\\w)|control(?!\\w)|submi|allegian|serve(?!\\w)|abide(?!\\w)|defere|defer(?!\\w)|revere|venerat|comply(?!\\w)|defian|rebel|dissent|subver|disrespect|disobe|sediti|agitat|insubordinat|illegal|insurgent(?!\\w)|mutinous(?!\\w)|defy|dissident(?!\\w)|unfaithful(?!\\w)|alienate(?!\\w)|defector(?!\\w)|heretic|nonconformist(?!\\w)|oppose(?!\\w)|protest(?!\\w)|refuse(?!\\w)|denounce(?!\\w)|remonstrate(?!\\w)|riot|obstruct(?!\\w)")
Pure <- c("preserve(?!\\w)|ruin|exploit(?!\\w)|exploits(?!\\w)|exploited(?!\\w)|exploiting(?!\\w)|apostasy(?!\\w)|apostate(?!\\w)|heretic|piety(?!\\w)|pious(?!\\w)|purity(?!\\w)|pure|clean|steril|sacred|chast|holy(?!\\w)|holiness(?!\\w)|saint|wholesome|celiba|abstention(?!\\w)|virgin(?!\\w)|virgins(?!\\w)|virginity(?!\\w)|virginal(?!\\w)|austerity(?!\\w)|integrity(?!\\w)|modesty(?!\\w)|abstinen|abstemiousness(?!\\w)|upright(?!\\w)|limpid(?!\\w)|unadulterated(?!\\w)|maiden(?!\\w)|virtuous(?!\\w)|refined(?!\\w)|decen|immaculate(?!\\w)|innocent(?!\\w)|pristine(?!\\w)|church|disgust|deprav|disease|unclean|contagio|indecen|sin(?!\\w)|sinful|sinner|sins(?!\\w)|sinned(?!\\w)|sinning(?!\\w)|slut|whore(?!\\w)|dirt|impiety(?!\\w)|impious(?!\\w)|profan|gross(?!\\w)|repuls|sick|promiscu|lewd|adulter|debauche|defile|tramp(?!\\w)|prostitut|unchaste(?!\\w)|intemperate(?!\\w)|wanton(?!\\w)|profligate(?!\\w)|filth|trashy(?!\\w)|obscen|lax(?!\\w)|taint|stain|tarnish|debase|desecrat|wicked|blemish(?!\\w)|exploitat|pervert(?!\\w)|wretched")
Lib <-c("exploit(?!\\w)|exploits(?!\\w)|exploited(?!\\w)|exploting(?!\\w)|rights(?!\\w)|obey|obedient|duti|order|supremacy(?!\\w)|control(?!\\w)|submi|serve(?!\\w)|abide(?!\\w)|defere|defer(?!\\w)|defian|rebel|dissent|subver|disobe|defy|defector(?!\\w)|nonconformist(?!\\w)|protest(?!\\w)|free(?!\\w)|freedom(?!\\w)|liberty(?!\\w)|autonom|choice(?!\\w)|choose(?!\\w)|liberate(?!\\w)|liberation(?!\\w)|sovereign|independent(?!\\w)|independence(?!\\w)|dictat|totalitar|coerc|authoritarian|tyran")

alltext$care<-as.logical(lapply(alltext$text, grepl, pattern = Care, ignore.case = TRUE, perl = TRUE))
alltext$fair<-as.logical(lapply(alltext$text, grepl, pattern = Fair, ignore.case = TRUE, perl = TRUE))
alltext$loyal<-as.logical(lapply(alltext$text, grepl, pattern = Loyal, ignore.case = TRUE, perl = TRUE))
alltext$auth<-as.logical(lapply(alltext$text, grepl, pattern = Auth, ignore.case = TRUE, perl = TRUE))
alltext$pure<-as.logical(lapply(alltext$text, grepl, pattern = Pure, ignore.case = TRUE, perl = TRUE))
alltext$lib<-as.logical(lapply(alltext$text, grepl, pattern = Lib, ignore.case = TRUE, perl = TRUE))
alltext$anymoral<-as.logical(ifelse(alltext$care == TRUE|alltext$fair == TRUE|alltext$loyal == TRUE|alltext$auth == TRUE|alltext$pure == TRUE|
                                      alltext$lib == TRUE, "TRUE", "FALSE"))
rm(Auth,Care,Fair,General,Lib,Loyal,Pure)
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
    model <- glm(family = binomial(link = "logit"), DV ~ brexiteer + conservative + labour + medium + days_to_go , data = alltext)
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
    model <- lm(DV ~ care + fair + loyal + auth + lib + pure + days_to_go + reply, data = allTwitter)
    
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
unilib$aic<-round(uniclib$aic)
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

stargazer(alltext, type = "html", keep = c("days_to_go","favourites","retweets"), omit.summary.stat = c("min","max"), out = "", digits)

##Get Univariate logistic regression table
stargazer(unicare,unifair,uniloyal,uniauth,unipure,unilib,
          type = "html", title = "Fig. 1 Univariate Models", align = TRUE, out = "model1.html", omit.stat = c("ll"), 
          dep.var.labels = "Moral Foundation",
          column.labels = c("Care","Fairness","Loyalty","Authority","Purity","Liberty"))
rm(unicare,unifair,uniloyal,uniauth,unipure,unilib)
##Get Person-level Multivariate logistic regression table
stargazer(personcare,personfair,personloyal,personauth,personpure,personlib,
          type = "html", title = "Fig. 2 Person-level multivariate Models", align = TRUE, out = "model2.html", omit.stat = c("ll"),
          dep.var.labels = "Moral Foundation",
          column.labels = c("Care","Fairness","Loyalty","Authority","Purity","Liberty"))
rm(personcare,personfair,personloyal,personauth,personpure,personlib)
##Get Context-level Multivariate logistic regression table
stargazer(contextcare,contextfair,contextloyal,contextauth,contextpure,contextlib,
          type = "html", title = "Fig. 3 Context-level multivariate Models", align = TRUE, out = "model3.html", omit.stat = c("ll"),
          dep.var.labels = "Moral Foundation",
          column.labels = c("Care","Fairness","Loyalty","Authority","Purity","Liberty"))
##Make exponent-coefficient tables
stargazer(contextcare,contextfair,contextloyal,contextauth,contextpure,contextlib,
          type = "html", title = "Fig. 4 Final model coefficient exponents", align = TRUE, out = "table4.html", 
          omit.stat = c("ll","aic"), dep.var.labels = "Moral Foundation", apply.coef = function(x) exp(x), report = "vc", 
          omit.table.layout = "sn", column.labels = c("Care","Fairness","Loyalty","Authority","Purity","Liberty"), omit = "Constant")
rm(contextcare,contextfair,contextloyal,contextauth,contextpure,contextlib)
##Compare Leave.EU and Vote Leave
leavecamps<-filter(alltext, speaker == "Vote Leave"|speaker == "Leave.EU")
leavecamps$leaveeu<-ifelse(leavecamps$speaker == "Leave.EU",TRUE,FALSE)
leavecampsmodel<-LogReg(leavecamps$leaveeu, model = "leavecamps")
stargazer(leavecampsmodel,
          type = "html", title = "Fig. 5 Leave camps differentiated", align = TRUE, out = "model4.html", 
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
          LinReg(allTwitter$zrts, model = "twitterall"), type = "html", title = "Fig. 6 Multivariate Linear Models", align = TRUE, out = "model5.html",
          table.placement = "h", column.labels = c("Favourites","Favourites","Retweets","Retweets"))
