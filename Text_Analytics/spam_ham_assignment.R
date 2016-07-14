emails<- read.csv("emails.csv", stringsAsFactors=FALSE)
str(emails)
table(emails$spam)

# How many characters are in the longest email in the dataset
max(nchar(emails$text))

# Which row contains the shortest email in the dataset?
which.min(nchar((emails$text)))

library(tm)
corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus,content_transformer(tolower))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c(stopwords("english")))
corpus <- tm_map(corpus, stemDocument)
DTM <- DocumentTermMatrix(corpus)
DTM

spdtm <- removeSparseTerms(DTM, 0.95)
spdtm

emailsSparse <- as.data.frame(as.matrix(spdtm), row.names = FALSE)

# What is the word stem that shows up most frequently across all the emails in the dataset? 
which.max(colSums(emailsSparse))
sort(colSums(emailsSparse))

emailsSparse$spam <- emails$spam

onlyHam <- subset(emailsSparse, emailsSparse$spam == FALSE)
onlySpam<- subset(emailsSparse, emailsSparse$spam == TRUE)

# How many word stems appear at least 5000 times in the ham emails in the dataset?
table(colSums(onlyHam) > 5000)
sort(colSums(subset(emailsSparse, spam == 0)))

# How many word stems appear at least 1000 times in the spam emails in the dataset?
sort(colSums(subset(emailsSparse, spam == 1)))
# 4 - 1(word "spam") = 3

# Building machine learning models
emailsSparse$spam = as.factor(emailsSparse$spam)
# Making sure, column name is in correct format
colnames(emailsSparse) <- make.names(colnames(emailsSparse))

set.seed(123)
library(caTools)

split <- sample.split(emailsSparse$spam, SplitRatio = 0.7)
trainSparse <- subset(emailsSparse, split == TRUE)
testSparse <- subset(emailsSparse, split == FALSE)

# if there is a number in name, we will change it to string name

spamLog <- glm(spam ~ ., data = trainSparse, family = binomial)

library(rpart)
library(rpart.plot)
spamCART <- rpart(spam ~ ., data  = trainSparse, method = "class")

library(randomForest)
spamRF <- randomForest(spam ~ ., data = trainSparse)

# For each model, obtain the predicted spam probabilities for the training set.
predictLog <- predict(spamLog, type = "response", newdata = trainSparse)
summary(predictLog)

# How many of the training set predicted probabilities from spamLog are less than 0.00001?
table(predictLog < 0.00001)
# 3046
#How many of the training set predicted probabilities from spamLog are more than 0.99999?
table(predictLog > 0.99999)
# 954
table(predictLog < 0.99999 &  predictLog > 0.00001)
# 10

# How many variables are labeled as significant (at the p=0.05 level) in the logistic regression summary output?
summary(spamLog)

# From summary(spamLog), we see that none of the variables are labeled as significant 
# (a symptom of the logistic regression algorithm not converging).


predCART <- predict(spamCART)[,2]
summary(predCART)

predRF <- predict(spamRF, type = "prob")[,2]
summary(predRF)

# How many of the word stems "enron", "hou", "vinc", and "kaminski" appear in the CART tree? 
rpart.plot(spamCART)
# 2

# What is the training set accuracy of spamLog, using a threshold of 0.5 for predictions?
table(trainSparse$spam, predictLog > 0.5)
(3052+954)/nrow(trainSparse)
# 0.9990025

# What is the training set AUC of spamLog?
library(ROCR)
ROCRpredLog <- prediction(predictLog, trainSparse$spam)
aucLog = as.numeric(performance(ROCRpredLog, "auc")@y.values)
aucLog
# 0.9999959

# What is the training set accuracy of spamCART, using a threshold of 0.5 for predictions? 
table(trainSparse$spam, predCART > 0.5)
CARTacc <- (2885+894)/nrow(trainSparse)
CARTacc
# 0.942394

# What is the training set AUC of spamCART?
ROCRpredCART <- prediction(predCART, trainSparse$spam)
AucCART <- as.numeric(performance(ROCRpredCART, "auc")@y.values)
AucCART
# 0.9696044

# What is the training set accuracy of spamRF, using a threshold of 0.5 for predictions?
table(trainSparse$spam, predRF > 0.5)
RFacc <- (3013+917)/nrow(trainSparse)
RFacc
# 0.9800499

ROCRpredRF <- prediction(predRF, trainSparse$spam)
aucRF <- as.numeric(performance(ROCRpredRF, "auc")@y.values)
aucRF
# 0.997696

# TEST SET
# What is the testing set accuracy of spamLog, using a threshold of 0.5 for predictions?
predTestLog <- predict(spamLog, type = "response", newdata = testSparse)
table(testSparse$spam, predTestLog > 0.5)
TestAccLog <- (1257+376)/nrow(testSparse)
TestAccLog
# 0.9505239

# What is the testing set AUC of spamLog?
ROCRtestLog <- prediction(predTestLog, testSparse$spam)
AucTestLog <- as.numeric(performance(ROCRtestLog, "auc")@y.values)
AucTestLog
# 0.9627517

# What is the testing set accuracy of spamCART, using a threshold of 0.5 for predictions?
predTestCART <- predict(spamCART, newdata = testSparse)[,2]
table(testSparse$spam, predTestCART > 0.5)
TestAccCART <- (1228+386)/nrow(testSparse)
TestAccCART
# 0.9394645

ROCRtestCART <- prediction(predTestCART, testSparse$spam)
AucTestCART <- as.numeric(performance(ROCRtestCART, "auc")@y.values)
AucTestCART
# 0.963176

# What is the testing set accuracy of spamRF, using a threshold of 0.5 for predictions?
predTestRF <- predict(spamRF, newdata = testSparse, type = "prob")[,2]
table(testSparse$spam, predTestRF > 0.5)
TestAccRF <- (1289+387)/nrow(testSparse)
TestAccRF
# 0.975553

ROCRtestRF <- prediction(predTestRF, testSparse$spam)
AucTestRF <- as.numeric(performance(ROCRtestRF, "auc")@y.values)
AucTestRF
# 0.99752