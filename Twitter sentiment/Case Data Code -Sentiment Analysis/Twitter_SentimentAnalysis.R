##****************Set the Working directory***********

setwd("D:/case studies/Twitter sentiment/Case Data Code -Sentiment Analysis")

##****************Install and Invoke Required Packagesfor Sentiment Analysis***********

library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(sentiment)
library(httr)
library(devtools)
library(twitteR)
library(base64enc)


#https://apps.twitter.com/
consumerKey="2JSSQpbn5FTGP7NV65K1dDusd"
consumerSecret="XKyzCAWqOpXiFzljDiPKgEXJpYyXqWuylqIeL7vUwvckxyLEzZ"
accesstoken="4018044100-KJjseiqiFKlbtHRg5NB3dGB1WWIJnRPttdzSXfR"
accesssecret="qiiymvl4eY3eVSDZjUxw08lPtAVmLaTruPDbp07pp9IFG"

setup_twitter_oauth(consumerKey , consumerSecret, accesstoken, accesssecret)

##****************Use Sample data to Perform Sentiment Analysis***********

some_txt <-read.csv("NestleTweetsSample.csv")

print(some_txt)


#some_txt = sapply(some_txt, function(x) x$getText())

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", verbose=TRUE)
head(class_emo)

class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)

head(class_emo)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
head(class_pol)
# get polarity best fit
polarity = class_pol[,4]
head(class_pol)
## *********Create data frame with the results and obtain some general statistics******
# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

sent_df1 = within(sent_df,
                  polarity <- factor(polarity, levels=names(sort(table(polarity), decreasing=TRUE))))


##*************Plot the obtained results*****************
emo<-table(sent_df$emotion)
emo
barplot(emo)


# plot distribution of Emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets", title="classification based on emotion") 


## plot distribution of Polarity
ggplot(sent_df1, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="Dark2")+labs(x="polarity categories", y="number of tweets",title="classification based on polarity")

##### Cloud comparision
# First, separate the words according to emotions
Nestle_emos = levels(factor(sent_df$emotion))
Nestle_emos

n_Nestle_emos = length(Nestle_emos)
n_Nestle_emos
Nestle_emos_docs = rep(" ", n_Nestle_emos)
for (i in 1:n_Nestle_emos)
{
  tmp = some_txt[emotion == Nestle_emos[i],2]
  Nestle_emos_docs[i] = paste(tmp, collapse=" ")
}


###### Remove stopwords- Data Cleaning Step*************** 

Nestle_emos_docs = removeWords(Nestle_emos_docs, stopwords(kind = "en"))

###### Vector creation to represent on A CORPORA- PACKAGE -tm
library(corpora)
Nestle.corpus = Corpus(VectorSource(Nestle_emos_docs))
Nestle.tdm = TermDocumentMatrix(Nestle.corpus)
Nestle.tdm = as.matrix(Nestle.tdm)
colnames(Nestle.tdm) = Nestle_emos
Nestle_emos
# creating, comparing and plotting the words on the cloud
comparison.cloud(Nestle.tdm, colors = brewer.pal(n_Nestle_emos, "Dark2"),scale = c(4,.5), random.order = TRUE, title.size = 1.5)
