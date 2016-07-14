# Pubmed research articles
trials <- read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
# need to read it as a string and not a factor
summary(trials)
str(trials)

# How many characters are there in the longest abstract?
max(nchar(trials$abstract))
# How many search results provided no abstract?
table(nchar(trials$abstract)==0)
#1 112

# Find the observation with the minimum number of characters in the title out of all of the observations. 
# What is the text of the title of this article?
which.min(nchar(trials$title))
# 1258
trials$title[which.min(nchar(trials$title))]
# "A decade of letrozole: FACE."

library(tm)
corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))

corpusTitle <- tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract <- tm_map(corpusAbstract, content_transformer(tolower))

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, c(stopwords("english")))
corpusAbstract = tm_map(corpusAbstract, removeWords, c(stopwords("english")))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)

dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)

dtmTitle <- as.data.frame(as.matrix(dtmTitle), row.names=FALSE)
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract), row.names=FALSE)

dtmTitle
dtmAbstract
length(stopwords("english"))
#How many terms remain in dtmAbstract?
# 335
#How many terms remain in dtmTitle after removing sparse terms?
# 31

# What is the most frequent word stem across all the abstracts?
which.max(colSums(dtmAbstract))
# patient

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

str(dtmTitle)
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial <- trials$trial
str(dtm)

set.seed(144)
library(caTools)
split <- sample.split(dtm$trial, SplitRatio = 0.7)
train_trials <- subset(dtm, split == TRUE)
test_trials <- subset(dtm, split == FALSE)

# What is the accuracy of the baseline model on the training set?
table(train_trials$trial)
730/nrow(train_trials)
# 0.5606759

trialCART <- rpart(trial ~ ., data = train_trials, method = "class")
rpart.plot(trialCART)

# What is the name of the first variable the model split on?

trialCART_2 <- rpart(trial ~ ., data = train_trials)
str(trialCART_2)
# Obtain the training set predictions for the model
predict_train <- predict(trialCART)[,2]
summary(predict_train)

# Because the CART tree assigns the same predicted probability to each leaf node and there are
# a small number of leaf nodes compared to data points, 
# we expect exactly the same maximum predicted probability for testing set.

# What is the training set accuracy of the CART model?
table(train_trials$trial, predict_train > 0.5)
acc_train <- (631+441)/nrow(train_trials)
acc_train
# 0.8233487

# What is the training set sensitivity of the CART model?
441/(131+441)
# 0.770979 true positive

# What is the training set specificity of the CART model?
631/(631+99)
# 0.8643836 true negative

# this involves not setting a type argument, and keeping only the second column of the predict output
predict_test <- predict(trialCART, newdata = test_trials)[,2]
# What is the testing set accuracy, assuming a probability threshold of 0.5 for predicting that a result is a clinical trial?
table(test_trials$trial, predict_test > 0.5)
str(predict_test)

#     FALSE TRUE
# 0   261   52
# 1    83  162

acc_test <- (261+162)/nrow(test_trials)
acc_test
# 0.7580645

# Using the ROCR package, what is the testing set AUC of the prediction model?
library(ROCR)

ROCRpredict = prediction(predict_test,test_trials$trial)
auc_trial = as.numeric(performance(ROCRpredict, "auc")@y.values)
auc_trial

table(test_trials$trial, predict_test > 0.8)
str(predict_test)
