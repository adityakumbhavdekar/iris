
library(httr)
library(SnowballC)
library(RColorBrewer)
library(tm)
library(wordcloud)

setwd("c:/edu")
mach_text  <-read.csv("NestleTweetsSample.csv")

mach_corpus = Corpus(VectorSource(mach_text))
mach_corpus

mach_text = sapply(mach_tweets, function(x) x$getText())
head(text)
mycorpus=Corpus(VectorSource(mydata$x))
inspect(mycorpus)

# creat plain text
mycorpus=tm_map(mycorpus,PlainTextDocument)

# Remove punctuation - remove comma, .
mycorpus=tm_map(mycorpus, removePunctuation)

# do stemming- running converted to run

mycorpus=tm_map(mycorpus, stemDocument)

# Remove stopwords - remove a ,and etc
mycorpus=tm_map(mycorpus,removeWords,stopwords (kind = "en"))

# do word cloud

pal= brewer.pal(8, "Dark2")
wordcloud(mycorpus, min.freq = 3, max.words = Inf,width=1000, height=100, random.order = FALSE, color=pal)



docs <- Corpus(VectorSource(text))
inspect(docs)


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)



dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)


wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))




dtm <- DocumentTermMatrix(some_txt)
