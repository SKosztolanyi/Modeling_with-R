stevens <- read.csv("stevens.csv")
str(stevens)

library(caTools)
set.seed(3000)

spl <- sample.split(stevens$Reverse, SplitRatio = 0.7)
train <- subset(stevens, spl == TRUE)
test <- subset(stevens, spl == FALSE)

library(rpart)
library(rpart.plot)

# method class is classification instead of regression
StevensTree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 25)

prp(StevensTree)

predictCART <- predict(StevensTree, newdata = test, type = "class")

table(test$Reverse, predictCART)

accuracy <- 112/(112+58)
accuracy

library(ROCR)

predictROCR <- predict(StevensTree, newdata = test)
predictROCR
pred <- prediction(predictROCR[,2], test$Reverse)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

AUC <- as.numeric(performance(pred, "auc")@y.values)
AUC

StevensTree_2 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 5)
prp(StevensTree_2)

StevensTree_3 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 100)
prp(StevensTree_3)

library(randomForest)

set.seed(200)
# the dependent variable need to be a factor to build a logistic forest, otherwise it build a regression model
StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent+ LowerCourt + Unconst, data = train, nodesize = 25, ntree = 200)

train$Reverse <- as.factor(train$Reverse)
test$Reverse <- as.factor(test$Reverse)

predictForest <- predict(StevensForest, newdata = test)
table(test$Reverse, predictForest)

forest_accuracy <- (44+76)/(44+76+33+17)
forest_accuracy


# Cross-validating
library(caret)
library(e1071)

numfolds<- trainControl(method="cv", number=10)
cpGrid<- expand.grid(.cp=seq(0.01, 0.5, 0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "rpart", trControl =numfolds, tuneGrid = cpGrid)

StevensTreeCv <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", cp = 0.18)
predictCV <- predict(StevensTreeCv, newdata = test, type = "class")
table(test$Reverse, predictCV)

accuracyCV <- (59+64)/(29+64+18+59)
accuracyCV

prp(StevensTreeCv)
