setwd("C:/Documents and Settings/Superman!/Desktop/Aditya/November/Nov 30")

# Read file
apple <- read.csv(file.choose(), header = T)
str(apple)

# Build corpus
library(tm)
corpus <- iconv(apple$x)
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
          scale = c(4, 0.7),
          rot.per = 0.3)

library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.4,
           shape = 'triangle',
           rotateRatio = 0.3,
           minSize = 1)

letterCloud(w,
            word = "sad",colors = "random-light",
            backgroundColor= "black",
            size=4)


# Change the shape using your image
wordcloud2(w, figPath = "kidloland.png", 
           size = 0.7, color = "skyblue", 
           backgroundColor="black")

wordcloud2(w, figPath = "star1.png", 
           size = 0.7, color = "random-light", 
           backgroundColor="black")
# Change the shape using your image
wordcloud2(w, 
           size = 1.5, color = "random-light", 
           backgroundColor="black")