# Logistic regression training with health care dataset
quality <- read.csv("quality.csv")
str(quality)
table(quality$PoorCare)

# Baseline model prediction is predicting everybody has the value that is most frequent
98/131
# baseline model accuracy is 0.7480916, everything more is better model and less is worse model.

# As a next step I load a package for splitting dataset into training and test
library(caTools)

set.seed(88)
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)

qualityTrain <- subset(quality, split == TRUE)
qualityTest <- subset(quality, split == FALSE)
nrow(qualityTrain)

QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, family = binomial, data = qualityTrain)
summary(QualityLog)

# making predictions with model
predictTrain <- predict(QualityLog, type = "response")
summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean)

# different model:
QualityLog2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(QualityLog2)

# Finding a threshold value for predicting 1/0
table(qualityTrain$PoorCare, predictTrain > 0.5)

                                    # FALSE TRUE
## TrueNegative, FalseNegative     0    70    4
## FalsePositive, TruePositive     1    15   10

# Senzitivity = TruePositive / FalsePositive + TruePositive
senzitivity <- 10/25
senzitivity # 0.4

# Specificity = TrueNegative / FalseNegative + TrueNegative
specificity <- 70/74
specificity # 0.95

# another threshold
table(qualityTrain$PoorCare, predictTrain > 0.7)
#FALSE TRUE
#0    73    1
#1    17    8

# By increasing threshold, our sensitivity went down and our specificity went up.
# It is still not the best balance and we want to find a better threshold that will be a good tradeoff

# Decreasing the threshold:
table(qualityTrain$PoorCare, predictTrain > 0.35)

# This looks like the best model as it calculates the least number of false prediction > not accurate
# I need to calculate accuracy of every model and compare it to the baseline model.
# To compare visually different thresholds, I can use ROC(Receiver Operator Characteristic) curve.

library('ROCR')

# This prediction uses the same data as we used when doing table.
# ROC predict calculates values for all possible thresholds.
ROCRpred <- prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2, 1.7))

# Given this ROC curve, which threshold would you pick if you wanted to correctly identify a small group of patients 
# who are receiving the worst care with high confidence?
# 0.7: FALSE TRUE
#0    73    1
#1    17    8

#A: 
# The threshold 0.7 is best to identify a small group of patients who are receiving the worst care 
# with high confidence, since at this threshold we make very few false positive mistakes, 
# and identify about 35% of the true positives. 
# The threshold t = 0.8 is not a good choice, since it makes about the same number of false positives,
# but only identifies 10% of the true positives. 
#The thresholds 0.2 and 0.3 both identify more of the true positives, 
#but they make more false positive mistakes, so our confidence decreases.

# Another important aspect is AUC - Area Under the ROC Curve. It is calculated as a portion of data/1.
# It is absolute measure of quality - accuracy of a model. 
# Maximum is 1, minimum is 0.5(pure guessing).

# Calculating overall accuracy:
# TP + TN / N
(67+13)/99
#0.81 compared to baseline 0.75 is slight improvement.

# Interpreting AUC:
# The AUC of a model has the following nice interpretation: 
# given a random patient from the dataset who actually received poor care, 
# and a random patient from the dataset who actually received good care, 
# the AUC is the perecentage of time that our model will classify which is which correctly.

predictTest = predict(QualityLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc


