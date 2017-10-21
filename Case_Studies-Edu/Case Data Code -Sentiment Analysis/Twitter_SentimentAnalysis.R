##****************Set the Working directory***********
setwd("c:/edu")
setwd("E:/Path/Path")

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


# ref for installing sentiment -https://www.r-bloggers.com/intro-to-text-analysis-with-r/
require(devtools)
#install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
#install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
#install.packages("plyr")
#install.packages("ggplot2")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("tm")
#install.packages("SnowballC")

##****************Use Sample data to Perform Sentiment Analysis***********

some_txt <-read.csv("NestleTweetsSample - Copy.csv")

print(some_txt)


#..........................................................................
documents <- c("I am very happy, excited, and optimistic.",
               "I am very scared, annoyed, and irritated.",
               "Iraq's political crisis entered its second week one step closer to the potential 
               dissolution of the government, with a call for elections by a vital coalition partner 
               and a suicide attack that extended the spate of violence that has followed the withdrawal 
               of U.S. troops.")

# CLASSIFY EMOTIONS
classify_emotion(documents,algorithm="bayes",verbose=TRUE)
#..............................................................................

#some_txt = sapply(some_txt, function(x) x$getText())

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)

# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
head(class_pol)
# get polarity best fit
polarity = class_pol[,4]
head(polarity)

## *********Create data frame with the results and obtain some general statistics******
# data frame with vector emotion, polarity and text is created 
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

sent_df1 = within(sent_df,
                  polarity <- factor(polarity, levels=names(sort(table(polarity), decreasing=TRUE))))


##*************Plot the obtained results*****************
## can be done as follows too
#emo<-table(sent_df$emotion)
#emo
#barplot(emo)

# plot distribution of Emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets", title="classification based on emotion") 


## plot distribution of Polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="Dark2")+labs(x="polarity categories", y="number of tweets",title="classification based on polarity")

##### Cloud comparision
# First, separate the words according to emotions ----  seperate each tweet according to one of the 5 emotion
Nestle_emos = levels(factor(sent_df$emotion))
Nestle_emos
n_Nestle_emos = length(Nestle_emos)
n_Nestle_emos
Nestle_emos_docs = rep(" ", n_Nestle_emos)
Nestle_emos_docs
for (i in 1:n_Nestle_emos)
{
  tmp = some_txt[emotion == Nestle_emos[i],2] # filter according to emotion for col2 which has tweet and save in tmp--tmp has tweet mapped to each emotion
  Nestle_emos_docs[i] = paste(tmp, collapse=" ")
}
#example Nestle_emos_docs[2] has emotion joy and so forth --u have five emotions-- index 1 has unknown emotion



###### Remove stopwords- Data Cleaning Step***************
#The who, that what, and

Nestle_emos_docs = removeWords(Nestle_emos_docs, stopwords(kind = "en"))

###### Vector creation to represent on A CORPORA- PACKAGE -tm

Nestle.corpus = Corpus(VectorSource(Nestle_emos_docs))
Nestle.tdm = TermDocumentMatrix(Nestle.corpus)
Nestle.tdm = as.matrix(Nestle.tdm)
colnames(Nestle.tdm) = Nestle_emos

# creating, comparing and plotting the words on the cloud
comparison.cloud(Nestle.tdm, colors = brewer.pal(n_Nestle_emos, "Dark2"),scale = c(4,.5), random.order = TRUE, title.size = 1.5)
