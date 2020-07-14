from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.common.exceptions import NoSuchElementException, StaleElementReferenceException
from time import sleep
import json
import datetime
import os
os.chdir(r"C:\Users\jaack\OneDrive - University of Cambridge\Summer Political Psychology\Brexit Twitter\Data")


# Search Terms
user = 'andrealeadsom'
start = datetime.datetime(2016, 4, 15)  # year, month, day
end = datetime.datetime(2016, 6, 23)  # year, month, day

# only edit these if you're having problems
delay = 5  # time to wait on each page load before reading the page
driver = webdriver.Chrome(r"C:\Users\jaack\AppData\Local\Programs\Python\Python38\Scripts\chromedriver.exe")  # options are Chrome() Firefox() Safari()

# don't mess with this stuff
days = (end - start).days + 1
tweet_selector = 'div.r-1d09ksm > a'
user = user.lower()
ids = []
thisid = ""

def format_day(date):
    day = '0' + str(date.day) if len(str(date.day)) == 1 else str(date.day)
    month = '0' + str(date.month) if len(str(date.month)) == 1 else str(date.month)
    year = str(date.year)
    return '-'.join([year, month, day])

def form_url(since, until):
    p1 = 'https://twitter.com/search?f=tweets&vertical=default&q=from%3A'
    p2 =  user + '%20since%3A' + since + '%20until%3A' + until + 'include%3Aretweets&src=typd'
    return p1 + p2

def increment_day(date, i):
    return date + datetime.timedelta(days=i)

for day in range(days):
    d1 = format_day(increment_day(start, 0))
    d2 = format_day(increment_day(start, 1))
    url = form_url(d1, d2)
    print(url)
    print(d1)
    driver.get(url)
    sleep(delay)
    start = increment_day(start, 1)
    
#    if day % 14 == 13:
#        driver.close()
#        sleep(2)
#        driver = webdriver.Chrome(r"C:\Users\jaack\AppData\Local\Programs\Python\Python38\Scripts\chromedriver.exe")  # options are Chrome() Firefox() Safari()
#        driver.get(url)
# 
    try:
        scheight = .1
        while scheight < 9.9:
            driver.execute_script("window.scrollTo(0, document.body.scrollHeight/%s);" % scheight)
            scheight += .1
            sleep(0.1)
            found_tweets = driver.find_elements_by_css_selector(tweet_selector)
        
            for tweet in found_tweets:
                try:
                    thisid = tweet.get_attribute('href').split('/')[-1]
                    if thisid.isdigit() == True:
                        ids.append(thisid)
                    else: 
                        print(thisid, " is not a valid id.")
                        
                except StaleElementReferenceException as e:
                    print('lost element reference', tweet)
            
            ids = list(set(ids))
        print(day, " cumulative ids: ", len(ids))
    except NoSuchElementException as e:
        print('no tweets on this day')

print("Total scraped ids after exclusions: ", len(ids))

try:
    with open('all_ids.json') as f:
        all_ids = ids + json.load(f)
        data_to_write = list(set(all_ids))
        print('tweets found on this scrape: ', len(ids))
        print('total tweet count: ', len(data_to_write))
except FileNotFoundError:
    with open('all_ids.json', 'w') as f:
        all_ids = ids
        data_to_write = list(set(all_ids))
        print('tweets found on this scrape: ', len(ids))
        print('total tweet count: ', len(data_to_write))
    
with open('all_ids.json', 'w') as outfile:
    json.dump(data_to_write, outfile)

print('all done here')
driver.close()
