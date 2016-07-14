# word clouds visualisations
tweets <- read.csv("tweets.csv", stringsAsFactors = F)

library(tm)
library(SnowballC)

# preprocessing
TweetCorpus <- Corpus(VectorSource(tweets$Tweet))
corpus_lower <- tm_map(TweetCorpus, content_transformer(tolower)) 
corpus_plaintext = tm_map(corpus_lower, PlainTextDocument)
corpus_nopunct <- tm_map(corpus_plaintext, removePunctuation)
corpus_stop <- tm_map(corpus_nopunct, removeWords, c(stopwords("english")))
frequencies <-DocumentTermMatrix(corpus_stop)
allTweets <- as.data.frame(as.matrix(frequencies))
colnames(allTweets) <- make.names(colnames(allTweets))


# How many unique words are there across all the documents?
ncol(allTweets)
library(wordcloud)

# Which function can we apply to allTweets to get a vector of the words in our dataset, which we'll pass as the first argument to wordcloud()?
colnames(allTweets)
# Which function should we apply to allTweets to obtain the frequency of each word across all tweets?
colSums(allTweets)

?wordcloud

tweetcloud2 <- wordcloud(colnames(allTweets), colSums(allTweets), scale=c(4, 0.5), min.freq = 15)

corpus_stop2 <- tm_map(corpus_nopunct, removeWords, c("apple", stopwords("english")))
frequencies2 <-DocumentTermMatrix(corpus_stop2)
allTweets2 <- as.data.frame(as.matrix(frequencies2))
colnames(allTweets2) <- make.names(colnames(allTweets2))

# What is the most common word in this new corpus (the largest word in the outputted word cloud)?
tweetcloud2 <- wordcloud(colnames(allTweets2), colSums(allTweets2), scale=c(3, 0.5), min.freq = 20)
#iphone

# Creating more visually appealing wordclouds
negativeTweets = subset(allTweets2, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets))

# brewer palette library is part of wordcloud
wordcloud(colnames(allTweets2), colSums(allTweets2), scale=c(3, 0.5), min.freq = 15,  
          random.order = F, colors= brewer.pal(9, "Blues")[c(-1, -2, -3, -4)], random.color = F)

display.brewer.all() 
