fedFunds <- read.csv("federalFundsRate.csv", stringsAsFactors = F)
str(fedFunds)

# What proportion of months did the Fed raise the interest rate?
table(fedFunds$RaisedFedFunds)
294/nrow(fedFunds)
# 0.5025641
table(fedFunds$Chairman)

fedFunds$Chairman <- as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres <- as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds <- as.factor(fedFunds$RaisedFedFunds)

# Which of the following methods requires the dependent variable be stored 
# as a factor variable when training a model for classification?
# Ans: RandomForest

set.seed(201)
library(caTools)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)

trainFed <- subset(fedFunds, spl == T)
testFed <- subset(fedFunds, spl == F)

# Why do we use the sample.split() function to split into a training and testing set?
# Ans:  It balances the dependent variable between the training and testing sets

fedLR <- glm(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = trainFed, family = binomial)
summary(fedLR)

# The observation has PreviousRate=1.7, Streak=-3, Unemployment=5.1, DemocraticPres=0, MonthsUntilElection=18
9.121012 + 1.7*(-0.003427) - 3* 0.157658 + 5.1*(-0.047449) + 65.3*(-0.136451) + 0*0.347829 + 18*(-0.006931)
# -0.6347861

# plug this into the logistic response function to get the predicted probability
# this is prediction on log odd scale, so I need to convert it back to odds
exp(-0.6347861)
#   0.5300489

# And now from odds to probability
0.5300489/(  0.5300489+1)
# The predicted probability for raised interest rate next month is 0.3464261

# What is the meaning of the coefficient labeled "DemocraticPres1" in the logistic regression summary output?
exp(0.347829)
1.41599/(1.41599+1)
# 0.586091

testpred <- predict(fedLR, newdata = testFed, type = "response")

# What is the number of test set observations where the prediction from the logistic regression model 
# is different than the prediction from the baseline model?
table(testFed$RaisedFedFunds, testpred > 0.5)

table(testFed$RaisedFedFunds)
# 88 T, so all T is 175

# Other is where predicsiton is false so 91.

# What is the test-set AUC of the logistic regression model?

# In statistics, a receiver operating characteristic (ROC), or ROC curve, is a graphical plot that illustrates 
# the performance of a binary classifier system as its discrimination threshold is varied. 
# The curve is created by plotting the true positive rate (TPR) against the false positive rate (FPR) at various threshold settings.
library(ROCR)
ROCRpred <- prediction(testpred, testFed$RaisedFedFunds)
auc <- as.numeric(performance(ROCRpred, "auc")@y.values)
# When using normalized units, the area under the curve (often referred to as simply the AUC, or AUROC) 
# is equal to the probability that a classifier will rank a randomly chosen positive instance higher 
# than a randomly chosen negative one (assuming 'positive' ranks higher than 'negative').

auc
# 0.704023

# Which logistic regression threshold is associated with the upper-right corner of the ROC plot (true positive rate 1 and false positive rate 1)?
table(testFed$RaisedFedFunds, testpred > 0)

# A model with threshold 0 predicts 1 for all observations, yielding a 100% true positive rate and a 100% false positive rate.

# Plot the colorized ROC curve for the logistic regression model's performance on the test set.
# The aim is to maximaze True Positive rate but minimize False positive rate.
perf <- performance(ROCRpred, "tpr", "fpr")
plot(perf, colorize = T)

# Which of the following best describes how 10-fold cross-validation works when selecting between 2 different parameter values?
# Ans: In 10-fold cross validation, the model with each parameter setting will be trained on 10 90% subsets of the training set. 
# Hence, a total of 20 models will be trained. 
# The models are evaluated in each case on the last 10% of the training set (not on the testing set).

set.seed(201)
library(caret)

# What cp value maximizes the cross-validation accuracy?
numfolds<- trainControl(method="cv", number=10)
cpGrid<- expand.grid(.cp=seq(0.001, 0.05, 0.001))
train(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = trainFed, method = "rpart", trControl =numfolds, tuneGrid = cpGrid)

fedTreeCv <- rpart(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = trainFed, method = "class", cp = 0.016)
predictCV <- predict(fedTreeCv, newdata = testFed, type = "class")

# What variable is used as the first (upper-most) split in the tree?
library(rpart)
library(rpart.plot)
prp(fedTreeCv)
rpart.plot(fedTreeCv)
# Streak

# What is the accuracy of your CART model?
table(testFed$RaisedFedFunds, predictCV)
(48+64)/nrow(testFed)
# 0.64