# KAGGLE COMPETITION - GETTING STARTED

# This script file is intended to help you get started on the Kaggle platform, and to show you how to make a submission to the competition.


# Let's start by reading the data into R
# Make sure you have downloaded these files from the Kaggle website, and have navigated to the directory where you saved the files on your computer

train = read.csv("train2016.csv")

test = read.csv("test2016.csv")

# We will just create a simple logistic regression model, to predict Party using all other variables in the dataset, except for the user ID:

SimpleMod = glm(Party ~ . -USER_ID, data=train, family=binomial)

str(SimpleMod)

# And then make predictions on the test set:

PredTrain = predict(SimpleMod, newdata = train, type = "response")
str(PredTrain)
PredTest = predict(SimpleMod, newdata=test, type="response")

threshold = 0.51

table(train$Party, PredTrain > 0.49)
# simple model Train prediction: 
#           FALSE TRUE
#Democrat    1982  795
#Republican   931 1527

(1982+1527)/nrow(train)
# 0.6302083

## Democrat is below 0.5, Republican is above 0.5.

#accuracy on train model:
      (1915+1586)/nrow(train)
# 0.6287716

summary(PredTrain)
table(train$Party)

PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "SubmissionSimpleLog2.csv", row.names=FALSE)
# threshold 0.51 and filling NAs with Democrat is accuracy of 0.63649

# fill na with default
MySubmission[is.na(MySubmission)] <- "Democrat"

summary(MySubmission)

summary(train$Party)

# Missing values:
# There are 333 Na's which need to be inserted, otherwise the prediction is Na. If there is some pattern
# in NA's I will benefit in inserting them. Maybe cluster them. Otherwise I will have to insert them the first default value - "Democrat"

# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle website to use this as a submission to the competition

# This model was just designed to help you get started - to do well in the competition, you will need to build better models!

str(train)
####### Model without questions - just the profile
train_sub <- train[,c("USER_ID", "YOB", "Gender", "Income", "HouseholdStatus", "EducationLevel", "Party", "Q98578", "Q98869",
                      "Q101596", "Q109244", "Q108950", "Q112478", "Q115899", "Q118232", "Q113181", "Q116197", "Q120194", 
                      "Q121699")]
summary(SimpleMod)
str(train_sub)

sub_mod <- glm(Party ~ . -USER_ID, data = train_sub, family = binomial)
summary(sub_mod)

summary(train_sub)

## Imputation of missing values:
library(mice)
tempData <- mice(train_sub,m=5,maxit=50,meth='pmm',seed=500)
completedTrain <- complete(tempData,1)



summary(completedTrain)
sub_mod <- glm(Party ~ . -USER_ID, data = completedTrain, family = binomial)

PredTrain_sub = predict(sub_mod, newdata = completedTrain, type = "response")

PredTest_sub = predict(sub_mod, newdata=test, type="response")

threshold = 0.5

table(train$Party, PredTrain_sub > 0.49)

(1892+1411)/nrow(train)
# 0.5932112

summary(PredTrain)
table(train$Party)

PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

## This approach was not the best and manually selecting fields is not so great.

# What if we selected only background info without questions?
train_sub2 <- train[,c("USER_ID", "YOB", "Gender", "Income", "HouseholdStatus", "EducationLevel", "Party")]
sub_mod2 <- glm(Party ~ . -USER_ID, data = train_sub2, family = binomial)
PredTrain_sub2 <- predict(sub_mod2, newdata = train_sub2,  type = "response")
table(train$Party, PredTrain_sub2 > 0.48)


for(i in 1:ncol(train_sub2)){
      train_sub2[is.na(train_sub2[,i]), i] <- mean(train_sub2[,i], na.rm = TRUE)
}
summary(train_sub2)
str(train_sub2)
completedTrain2 <- complete(tempData,1)

# I need to change char variables into factors for the model to work
completedTrain2$EducationLevel <- as.factor(completedTrain2$EducationLevel)
completedTrain2$Party <- as.factor(completedTrain2$Party)
completedTrain2$HouseholdStatus <- as.factor(completedTrain2$HouseholdStatus)
completedTrain2$Income <- as.factor(completedTrain2$Income)
completedTrain2$Gender <- as.factor(completedTrain2$Gender)

str(completedTrain2)
sub_mod2 <- glm(Party ~ . -USER_ID, data = completedTrain2, family = binomial)
summary(sub_mod2)
PredTrain_sub2 <- predict(sub_mod2, newdata = completedTrain2,  type = "response")
table(train$Party, PredTrain_sub > 0.49)

summary(completedTrain2)
str(completedTrain2)


## Cahnging to only background info didn't help us.
# What about clustering now?
# normalizing data
library(caret)
preproc <- preProcess(train)
preprocTest <- preProcess(test)
votingNormal <- predict(preproc, train)
votingTest <- predict(preprocTest, test)
summary(votingNormal)
# wait a minute, let's put the NA values of YOB away: (maybe next iteration)

voteDist <- dist(votingNormal, method="euclidean")
voteDistTest <- dist(votingTest, method="euclidean")
voteClust <- hclust(voteDist, method="ward.D2") # looks like 5 clusters here
voteClustTest <- hclust(voteDistTest, method="ward.D2")
plot(voteClust)
mean(train$YOB, na.rm = T)
train_woNA <- train
test_woNA <- test
train_woNA$YOB <- as.numeric(ifelse(is.na(train_woNA$YOB), mean(train$YOB, na.rm = T), train_woNA$YOB))
test_woNA$YOB <- as.numeric(ifelse(is.na(test_woNA$YOB), mean(test$YOB, na.rm = T), test_woNA$YOB))

summary(train_woNA)
table(train_woNA$YOB)

# Let's try kmeans and elbow method for choosing number of clusters:
SumWithinss = sapply(2:10, function(x) sum(kmeans(train_woNA, centers=x, iter.max=1000)$withinss))
SumWithinss
NumClusters = seq(2,10,1)
plot(NumClusters, SumWithinss, type="b")
# doesnt work because of some null values

k <- 5
voteKMC <- kmeans(votingNormal, centers = k, iter.max = 1000)
str(voteKMC)
# also doesnt work

#Hclust cut tree?
clusterGroups <- cutree(voteClust, k = 5)
clusterGroupsTest <- cutree(voteClustTest, k=5)
table(clusterGroups)
table(clusterGroupsTest)

# Looking at some patterns in between groups:
tapply(train_woNA$YOB, clusterGroups, mean)
tapply(train_woNA$Gender=="", clusterGroups, sum)
tapply(train_woNA$Income=="", clusterGroups, sum)
tapply(train_woNA$HouseholdStatus=="", clusterGroups, sum)
tapply(train_woNA$EducationLevel=="", clusterGroups, sum)
tapply(train_woNA$Party=="Republican", clusterGroups, sum)

str(train)

trainCluster1 <- subset(train_woNA, clusterGroups == 1)
cluster1model <- glm(Party ~ . -USER_ID, family = "binomial", data = trainCluster1)
predictCluster1 <- predict(cluster1model, newdata = trainCluster1, type = "response")
table(trainCluster1$Party, predictCluster1 > 0.5)
# acc of Model_of_cluster1:
(534+469)/nrow(trainCluster1)
#0.73

trainCluster2 <- subset(train_woNA, clusterGroups == 2)
cluster2model <- glm(Party ~ . -USER_ID, family = "binomial", data = trainCluster2)
predictCluster2 <- predict(cluster2model, newdata = trainCluster2, type = "response")
table(trainCluster2$Party, predictCluster2 > 0.5)
# acc of model_of_Cluster2:
(501+357)/nrow(trainCluster2)
# 0.74

trainCluster3 <- subset(train_woNA, clusterGroups == 3)
cluster3model <- glm(Party ~ . -USER_ID, family = "binomial", data = trainCluster3)
predictCluster3 <- predict(cluster3model, newdata = trainCluster3, type = "response")
table(trainCluster3$Party, predictCluster3 > 0.5)
# acc of model_of_Cluster3:
(460+370)/nrow(trainCluster3)
# 0.76

trainCluster4 <- subset(train_woNA, clusterGroups == 4)
cluster4model <- glm(Party ~ . -USER_ID, family = "binomial", data = trainCluster4)
predictCluster4 <- predict(cluster4model, newdata = trainCluster4, type = "response")
table(trainCluster4$Party, predictCluster4 > 0.5)
# acc of model_of_Cluster4:
(416+372)/nrow(trainCluster4)
# 0.75

trainCluster5 <- subset(train_woNA, clusterGroups == 5)
cluster5model <- glm(Party ~ . -USER_ID, family = "binomial", data = trainCluster5)
predictCluster5 <- predict(cluster5model, newdata = trainCluster5, type = "response")
table(trainCluster5$Party, predictCluster5 > 0.5)
# acc of model_of_Cluster5:
(333+323)/nrow(trainCluster5)

testCluster1 <- subset(test_woNA, clusterGroupsTest == 1)
testCluster2 <- subset(test_woNA, clusterGroupsTest == 2)
testCluster3 <- subset(test_woNA, clusterGroupsTest == 3)
testCluster4 <- subset(test_woNA, clusterGroupsTest == 4)
testCluster5 <- subset(test_woNA, clusterGroupsTest == 5)

predictTest1 <- predict(cluster1model, newdata = testCluster1, type = "response")
predictTest2 <- predict(cluster1model, newdata = testCluster2, type = "response")
predictTest3 <- predict(cluster1model, newdata = testCluster3, type = "response")
predictTest4 <- predict(cluster1model, newdata = testCluster4, type = "response")
predictTest5 <- predict(cluster1model, newdata = testCluster5, type = "response")

summary(predictTest1)


PredTestLabels1 = as.factor(ifelse(predictTest1<0.5, "Democrat", "Republican"))
PredTestLabels2 = as.factor(ifelse(predictTest2<0.5, "Democrat", "Republican"))
PredTestLabels3 = as.factor(ifelse(predictTest3<0.5, "Democrat", "Republican"))
PredTestLabels4 = as.factor(ifelse(predictTest4<0.5, "Democrat", "Republican"))
PredTestLabels5 = as.factor(ifelse(predictTest5<0.5, "Democrat", "Republican"))

MySubmission1 = data.frame(USER_ID = testCluster1$USER_ID, Predictions = PredTestLabels1)
MySubmission2 = data.frame(USER_ID = testCluster2$USER_ID, Predictions = PredTestLabels2)
MySubmission3 = data.frame(USER_ID = testCluster3$USER_ID, Predictions = PredTestLabels3)
MySubmission4 = data.frame(USER_ID = testCluster4$USER_ID, Predictions = PredTestLabels4)
MySubmission5 = data.frame(USER_ID = testCluster5$USER_ID, Predictions = PredTestLabels5)

rbindsubmission <- rbind.data.frame(MySubmission1, MySubmission2, MySubmission3, MySubmission4, MySubmission5)

str(rbindsubmission)

table(rbindsubmission$Predictions)

write.csv(rbindsubmission, "Hclust5.csv", row.names=FALSE)
# This method scored 0.58764, which was not an improvement

## Let's try Kmeans:
library(caret)
preproc <- preProcess(train)
preprocTest <- preProcess(test)
votingNormal <- predict(preproc, train)
votingTest <- predict(preprocTest, test)
summary(votingNormal)

library(flexclust)
set.seed(144)
km <- kmeans(votingNormal, centers = 5)

km.kcca = as.kcca(km, train_woNA)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
# How many test-set observations were assigned to Cluster 2?
table(clusterTest)
