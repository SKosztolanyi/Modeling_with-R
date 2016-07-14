census <- read.csv("census.csv")
set.seed(2000)
split <- sample.split(census$over50k, SplitRatio = 0.6)
train <- subset(census, split == TRUE)
test <- subset(census, split == FALSE)

library(caTools)
library(ROCR)
log_modelis <- glm(over50k ~ .,family = "binomial", data = train)
summary(log_modelis)

str(train)

# What is the accuracy of the model on the testing set? Use a threshold of 0.5.
modelis_predict <- predict(log_modelis, newdata = test, type="response")
table(test$over50k, modelis_predict > 0.5)
accuracy_log <- (9051+1888)/nrow(test)
accuracy_log

# What is the baseline accuracy for the testing set?
baseline_acc <- (9051+662)/nrow(test)
baseline_acc

# What is the area-under-the-curve (AUC) for this model on the test set?
library('ROCR')
ROCRpredict = prediction(modelis_predict, test$over50k)
auc_census = as.numeric(performance(ROCRpredict, "auc")@y.values)
auc_census
# 0.9061598

## CART
Class_tree <- rpart(over50k ~ .,data = train , method = "class")
prp(Class_tree)
