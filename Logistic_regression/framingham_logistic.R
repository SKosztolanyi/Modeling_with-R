framingham <- read.csv("framingham.csv")
str(framingham)
library(caTools)
set.seed(1000)
split <- sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

train<- subset(framingham, split==TRUE)
test <- subset(framingham, split==FALSE)

framinghamLog <- glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)

predictTest <- predict(framinghamLog, type = "response", newdata = test)
summary(predictTest)

table(test$TenYearCHD, predictTest > 0.5)

#               (predicted)FALSE    (predicted)TRUE
# 0(realFalse)  1069(TNP)           6(FalsePositivePrediction)
# 1(realTrue)   187(FNP)            11(TruePositivePrediction)

accuracy_of_0.5 <- (1069+11)/(1080+193)
accuracy_of_0.5
# 0.8483896

baseline <- 1075/(1075+198)
baseline
# 0.8444619

library(ROCR)
ROCRpred <- prediction(predictTest, test$TenYearCHD)
auc <- as.numeric(performance(ROCRpred, "auc")@y.values)
auc

#  0.7421095
senzitivity <- 11/198
senzitivity
# 0.05555556

specificity <- 1069/1075
specificity
# 0.9944186