setwd("C:/Documents and Settings/Superman!/Desktop/Aditya/November/Nov 30")

# Read file
apple <- read.csv(file.choose(), header = T)
str(apple)

# Build corpus
library(tm)
corpus <- iconv(apple$Worst.nd.asking.to.buy)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean text
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, removeWords, c('app'))


cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

# Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]
dim(tdm)
# Bar plot
w <- rowSums(tdm)
w <- subset(w, w>=15)
barplot(w,
        las = 2,
        col = rainbow(50))

# Word cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(6, 0.7),
          rot.per = 0.3)

library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.9,
           shape = 'star',
           rotateRatio = 0.3,
           minSize = 1)

letterCloud(w,
            word = "Kidlo",
            size=4)

# Sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read file
apple <- read.csv(file.choose(), header = T)
tweets <- iconv(apple$Worst.nd.asking.to.buy)

# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)
tweets[200]
get_nrc_sentiment('superb')
s$anger[204]
tweets[204]
get_nrc_sentiment("It didn't work with my phone, it seemed like you couldn't touch the icons to get then to play.")
# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(15),
        ylab = 'Count',
        xlab = "Emotion",
        main = 'Sentimental Scores for Kidloland GP 1 2 and 3 star Reviews')