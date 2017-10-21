##**********Steps to Set up authorization to connect and extract tweets********

library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(sentiment)
consumerKey="y0p7r2CZ5NJKoDYlz7NSphMLz"
consumerSecret="vaCgJCpKzSQAYhTPoURKaEAEE8AbuNEGSsplmd4hZES3lk0ScO"
accesstoken="868377331898826752-wBX6bXzsC9WUVSchpzPk4HwNKtapViQ"
accesssecret="SLVTXCWNW2MAO77JItPzGSvUFiapQW1I9pRBKS5QNrpJ3"


setup_twitter_oauth(consumerKey,consumerSecret,accesstoken,accesssecret)
setup_twitter_oauth(consumer_key ="y0p7r2CZ5NJKoDYlz7NSphMLz",consumer_secret = "868377331898826752-wBX6bXzsC9WUVSchpzPk4HwNKtapViQ",access_token ="868377331898826752-wBX6bXzsC9WUVSchpzPk4HwNKtapViQ",access_secret ="SLVTXCWNW2MAO77JItPzGSvUFiapQW1I9pRBKS5QNrpJ3")   )

installs_url

oauth_endpoint(authorize = "https://api.twitter.com/oauth",
               access = "https://api.twitter.com/oauth/access_token")

#connect to API
download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'

### Twitter Application
consumerKey="As Set in User Account"
consumerSecret="As Set in User Account"
accesstoken="As Set in User Account"
accesssecret="As Set in User Account"

Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=reqURL,
                         accessURL=accessURL,
                         authURL=authURL)
Cred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')) #There is URL in Console. You need to go to it, get code and enter it on Console

##### Authorization PIN -DYNAMIC

save(Cred, file='twitter authentication.Rdata')

load('twitter authentication.Rdata') 
#Once you launch the code first time, you can start from this line in the future (libraries should be connected)


getTwitterOAuth(consumer_key=consumerKey, consumer_secret=consumerSecret)
setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret)

##****************Step 3: Perform tweets extraction and data cleaning****************

# Harvest some tweets
some_tweets = searchTwitter("Nestle", n=2000, lang="en")

(n.some_tweets <- length(some_tweets))


# get the text
some_txt = sapply(some_tweets, function(x) x$getText())


# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)


# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

