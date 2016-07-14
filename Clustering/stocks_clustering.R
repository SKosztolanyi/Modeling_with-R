stocks <- read.csv("StocksCluster.csv")
str(stocks)

# What proportion of the observations have positive returns in December?
table(stocks$PositiveDec)
6324/nrow(stocks)
# 0.546114

# What is the maximum correlation between any two return variables in the dataset?
cor(stocks)
# 0.19167279

# Which month (from January through November) has the largest mean return across all observations in the dataset?
summary(stocks)

set.seed(144)
library(caTools)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

# What is the overall accuracy of logistic regression on the training set, using a threshold of 0.5?

stocksModel <- glm(PositiveDec ~ ., family = binomial, data = stocksTrain)
LogPred <- predict(stocksModel, type = "response", newdata = stocksTrain)
summary(LogPred)

table(stocksTrain$PositiveDec, LogPred >0.5)
(990+3640)/nrow(stocksTrain)
# 0.5711818

# Now obtain test set predictions from StocksModel

LogPredTest <- predict(stocksModel, type = "response", newdata = stocksTest)
table(stocksTest$PositiveDec, LogPredTest > 0.5)
(1553+417)/nrow(stocksTest)
# 0.5670697

# What is the accuracy on the test set of a baseline model that always predicts the most common outcome (PositiveDec = 1)?
mean(stocksTest$PositiveDec)

# Now, let's cluster the stocks. 
# The first step in this process is to remove the dependent variable using the following commands:
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

# What is the mean of the ReturnJan variable in normTrain?
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

set.seed(144)
km <- kmeans(normTrain, centers = 3)
# Which cluster has the largest number of observations?
summary(km)
str(km)
table(km$cluster)

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
# How many test-set observations were assigned to Cluster 2?
table(clusterTest)

# Building data frames based on clusters
TrainCluster1 <- subset(stocksTrain, clusterTrain == 1)
TrainCluster2 <- subset(stocksTrain, clusterTrain == 2)
TrainCluster3 <- subset(stocksTrain, clusterTrain == 3)

TestCluster1 <- subset(stocksTest, clusterTest == 1)
TestCluster2 <- subset(stocksTest, clusterTest == 2)
TestCluster3 <- subset(stocksTest, clusterTest == 3)

# Which training set data frame has the highest average value of the dependent variable?
mean(TrainCluster1$PositiveDec)
mean(TrainCluster2$PositiveDec)
mean(TrainCluster3$PositiveDec)

StocksModel1 <- glm(PositiveDec ~ ., family = "binomial", data = TrainCluster1)
StocksModel2 <- glm(PositiveDec ~ ., family = "binomial", data = TrainCluster2)
StocksModel3 <- glm(PositiveDec ~ ., family = "binomial", data = TrainCluster3)

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

# What is the overall accuracy of StocksModel1 on the test set stocksTest1, using a threshold of 0.5?
predictCluster1 <- predict(StocksModel1, type = "response", newdata= TestCluster1)
predictCluster2 <- predict(StocksModel2, type = "response", newdata= TestCluster2)
predictCluster3 <- predict(StocksModel3, type = "response", newdata= TestCluster3)

table(TestCluster1$PositiveDec, predictCluster1 > 0.5)
(774+30)/nrow(TestCluster1)
# 0.6194145

table(TestCluster2$PositiveDec, predictCluster2 > 0.5)
(388+757)/nrow(TestCluster2)
# 0.5504808

table(TestCluster3$PositiveDec, predictCluster3 > 0.5)
(49+13)/nrow(TestCluster3)
# 0.6458333

# To compute the overall test-set accuracy of the cluster-then-predict approach,
# we can combine all the test-set predictions into a single vector and all the true outcomes into a single vector:

AllPredictions = c(predictCluster1, predictCluster2, predictCluster3)
AllOutcomes = c(TestCluster1$PositiveDec, TestCluster2$PositiveDec, TestCluster3$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5)
(1544+467)/nrow(stocksTest)
# 0.5788716