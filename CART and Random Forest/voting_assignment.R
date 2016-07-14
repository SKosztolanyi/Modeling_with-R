gerber <- read.csv("gerber.csv")
summary(gerber)
table(gerber$voting)

108696/(108696+235388)
str(gerber)

voting_1 <- subset(gerber, voting==1)
table(gerber)
summary(gerber)
nrow(gerber$hawthorne, gerber$voting == 1)

# Which of the four "treatment groups" had the largest percentage of people who actually voted (voting = 1)?
table(gerber$hawthorne, gerber$voting)
12316/(12316+25888)
# 0.3223746
table(gerber$civicduty, gerber$voting)
12021/(12021+26197)
# 0.3145377
table(gerber$neighbors, gerber$voting)
14438/(14438+23763)
# 0.3779482
table(gerber$self, gerber$voting)
13191/(13191+25027)
# 0.3451515

# or:
tapply(gerber$voting, gerber$civicduty, mean)

library(caTools)
voting_lg <- glm(voting ~ civicduty + hawthorne + self + neighbors, data =gerber)
summary(voting_lg)

# Using a threshold of 0.3, what is the accuracy of the logistic regression model?
predict_voting <- predict(voting_lg, type = "response")
table(gerber$voting, predict_voting > 0.3)

accuracy <- (51966 + 134513) / (134513 + 100875 +  56730 + 51966)
accuracy

# Using a threshold of 0.5, what is the accuracy of the logistic regression model?
table(gerber$voting, predict_voting > 0.5)

accuracy2 <- (235388) / (108696+235388)
accuracy2
#  0.6841004
summary(predict_voting)

# Compare your previous two answers to the percentage of people who did not vote (the baseline accuracy)
# and compute the AUC of the model. What is happening here?
library('ROCR')
ROCRpredict = prediction(predict_voting, gerber$voting)
auc_voting = as.numeric(performance(ROCRpredict, "auc")@y.values)
auc_voting

# Even though all of our variables are significant, our model does not improve over the baseline model of just predicting 
# that someone will not vote, and the AUC is low. 
# So while the treatment groups do make a difference, this is a weak predictive model.

## TREES
# Plot the tree. What happens, and if relevant, why?
library(rpart)
library(rpart.plot)
CARTmodel_voting = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel_voting)

# just see one leaf! There are no splits in the tree, 
# because none of the variables make a big enough effect to be split on.

CARTmodel2_voting = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2_voting)
CART2_predict <- predict(CARTmodel2_voting, newdata = gerber)

CARTmodel3_voting = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
# In the control group, which gender is more likely to vote?
table(gerber$sex, gerber$voting)
# male
# In the "Civic Duty" group, which gender is more likely to vote?
table(gerber$civicduty, gerber$voting, gerber$sex)

prp(CARTmodel3_voting)

# Let's just focus on the "Control" treatment group.
CARTmodel4_control <- rpart(voting ~ control, data=gerber, cp=0.0)
CARTmodel5_sex <- rpart(voting ~ control + sex, data=gerber, cp=0.0)

CART4_control_predict <- predict(CARTmodel4_control, newdata=gerber)
# In the "control" only tree, what is the absolute value of the difference in the predicted 
# probability of voting between being in the control group versus being in a different group? 
summary(CART4_control_predict)
summary(CART2_predict)

prp(CARTmodel4_control, digits = 6)
# 0.296638, 0.34
abs_difference <- abs(0.296638-0.34)
abs_difference

prp(CARTmodel5_sex, digits = 6)
#The first split says that if control = 1, go left.
#Then, if sex = 1 (female) predict 0.290456, and if sex = 0 (male) predict 0.302795.
#On the other side of the tree, where control = 0, if sex = 1 (female) predict 0.334176, and if sex = 0 (male) predict 0.345818. 
#So for women, not being in the control group increases the fraction voting by 0.04372. 
#For men, not being in the control group increases the fraction voting by 0.04302. 
#So men and women are affected about the same.

log_sex_control <- glm(voting ~ sex + control, data = gerber, family = 'binomial')
summary(log_sex_control)
# Coefficient is negative, reflecting that women are less likely to vote Coefficient is negative, reflecting that women are less likely to vote

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(log_sex_control, newdata=Possibilities, type="response")

# What is the absolute difference between the tree and the logistic regression for the (Woman, Control) case?
abs_woman_control_diff <- abs( 0.290456 - 0.2908065)
abs_woman_control_diff
# 0.0003505

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

#  If a person is a woman and in the control group, the chance that she voted goes down. If a person is a woman and in the control group, the chance that she voted goes down.
# This coefficient is negative, so that means that a value of 1 in this variable decreases the chance of voting. 
# This variable will have variable 1 if the person is a woman and in the control group.

predict(LogModel2, newdata=Possibilities, type="response")
abs_woman_control_diff2 <- abs( 0.290456 - 0.2904558)
abs_woman_control_diff2
