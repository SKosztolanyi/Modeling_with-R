# Wikipedia vandalism on Language page

wiki <- read.csv("wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)

# How many cases of vandalism were detected in the history of this page?
table(wiki$Vandal)

str(wiki)

library(tm)
library(SnowballC)

corpusAdded <- Corpus(VectorSource(wiki$Added))
corpusAdded[[1]]$content

corpus_stop <- tm_map(corpusAdded, removeWords, c(stopwords("english")))
corpus_stop[[1]]$content

corpusAdded <- tm_map(corpus_stop, stemDocument)
corpusAdded[[1]]$content

dtmAdded <- DocumentTermMatrix(corpusAdded)
length(stopwords("english")) 
dtmAdded
# Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded
# Convert sparseAdded to a data frame called wordsAdded
wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) <- paste("A", colnames(wordsAdded))

# Now repeat all of the steps we've done so far to create a Removed bag-of-words dataframe
corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved[[3]]$content

corpusRemoved <- tm_map(corpusRemoved, removeWords, c(stopwords("english")))
corpusRemoved[[3]]$content

corpusRemoved <- tm_map(corpusRemoved, stemDocument)
dtmRemoved <- DocumentTermMatrix(corpusRemoved)

sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) <- paste("R", colnames(wordsRemoved))

# Combine the two data frames into a data frame called wikiWords
wikiWords <- cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal

library(caTools)
set.seed(123)
split <- sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain <- subset(wikiWords, split == TRUE)
wikiTest <- subset(wikiWords, split == FALSE)

# What is the accuracy on the test set of a baseline method that always predicts "not vandalism" (the most frequent outcome)?
table(test$Vandal)
618/nrow(test)
# 0.5313844

# What is the accuracy of the model on the test set, using a threshold of 0.5?
library(ROCR)
library(rpart)
library(rpart.plot)

wikiCART <- rpart(Vandal ~ ., data = wikiTrain, method = "class")
predictVandal <- predict(wikiCART, newdata = wikiTest, type = "class")
table(wikiTest$Vandal, predictVandal)

acc <- (618+12)/nrow(wikiTest)
acc

rpart.plot(wikiCART)

# There is no reason to think there was anything wrong with the split. 
# CART did not overfit, which you can check by computing the accuracy of the model on the training set.
# Over-sparsification is plausible but unlikely, since we selected a very high sparsity parameter. 
# The only conclusion left is simply that bag of words didn't work very well in this case.

# Problem-specific Knowledge
wikiWords2 <- wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

# Based on this new column, how many revisions added a link?
table(wikiWords2$HTTP )
# 217

# create a new CART model using this new variable as one of the independent variables.
wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

wikiCART2 <- rpart(Vandal ~ ., data = wikiTrain2, method = "class")
# What is the new accuracy of the CART model on the test set, using a threshold of 0.5?
predictVandal2 <- predict(wikiCART2, newdata = wikiTest2, type = "class")

table(wikiTest2$Vandal, predictVandal2)
acc2 <- (609+57)/nrow(wikiTest2)
acc2
# 0.5726569

library(tm)
wikiWords2$NumWordsAdded <- rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved <- rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)
# 4.050052

#  make new training and testing sets with wikiWords2
wTrain3 <- subset(wikiWords2, split == TRUE)
wTest3 <- subset(wikiWords2, split==FALSE)
wCART3 <- rpart(Vandal ~., data = wTrain3, method = "class")
predictVandal3 <- predict(wCART3, newdata = wTest3, type = "class")
# What is the new accuracy of the CART model on the test set?
table(wTest3$Vandal, predictVandal3)
acc3 <- (514+248)/nrow(wTest3)
acc3
# 0.6552021

# We have two pieces of "metadata" (data about data) that we haven't yet used.

wikiWords3 <- wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wTrain4 <- subset(wikiWords3, split == TRUE)
wTest4 <- subset(wikiWords3, split==FALSE)
wCART4 <- rpart(Vandal ~., data = wTrain4, method = "class")
predictVandal4 <- predict(wCART4, newdata = wTest4, type = "class")

# What is the new accuracy of the CART model on the test set?
table(wTest4$Vandal, predictVandal4)
acc4 <- (595+241)/nrow(wTest4)
acc4
# 0.7188306

# Plot the CART tree. How many splits are there in the tree?
rpart.plot(wCART4)
# 3