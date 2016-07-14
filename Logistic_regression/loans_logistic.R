loans <- read.csv("loans.csv")

#What proportion of the loans in the dataset were not paid in full? 
table(loans$not.fully.paid)
1533/(1533+8045)

# Which of the following variables has at least one missing observation?
summary(loans)

na_pub_rec <- subset(loans, is.na(loans$pub.rec))
summary(na_pub_rec)

# Which of the following is the best reason to fill in the missing values for these variables instead of removing observations with missing data?
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))

#  We want to be able to predict risk for all borrowers, instead of just the ones with all data reported. We want to be able to predict risk for all borrowers, 
# instead of just the ones with all data reported.
#  to predict risk for loans with missing data we need to fill in the missing values instead of removing the observations.
install.packages("mice")
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

control_loans <- read.csv("loans_imputed.csv")

summary(control_loans) == summary(loans)
#  We predicted missing variable values using the available independent variables for each observation.

set.seed(144)
library(caTools)
split <- sample.split(control_loans$not.fully.paid , SplitRatio = 0.7)
train <- subset(control_loans, split == TRUE)
test <- subset(control_loans, split == FALSE)

LoanLogReg <- glm(not.fully.paid~., data = train, family="binomial")
summary(LoanLogReg)

# Let Logit(A) be the log odds of loan A not being paid back in full, according to our logistic regression model, 
# and define Logit(B) similarly for loan B. What is the value of Logit(A) - Logit(B)?
fico_A <- 700
fico_B <- 710

logit_A <- -(10*-9.317e-03)
logit_A
odds <- exp(logit_A)
odds

predicted.risk = predict(LoanLogReg, type="response", newdata=test)
summary(predicted.risk)

test$predicted.risk <- predicted.risk

# Compute the confusion matrix using a threshold of 0.5.
table(test$not.fully.paid, predictTest > 0.5)

accuracy <- 2403/(2403 + 470)
accuracy

baseline_accuracy <- 2413/(2413+460)
baseline_accuracy

# Use the ROCR package to compute the test set AUC
library(ROCR)
ROCRpred <- prediction(predicted.risk, test$not.fully.paid)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

# A "Smart Baseline"
biv_model <- glm(not.fully.paid~int.rate, data = train, family = "binomial")
summary(biv_model)

biv_predict <- predict(biv_model, type = "response", newdata = test)
summary(biv_predict)

table(test$not.fully.paid, biv_predict > 0.5)

# According to the summary function, the maximum predicted probability of the loan not being paid back is 0.4266, 
# which means no loans would be flagged at a logistic regression cutoff of 0.5.

ROCR_biv_pred <- prediction(biv_predict, test$not.fully.paid)
aus <- as.numeric(performance(ROCR_biv_pred, "auc")@y.values)
aus

# How much does a $10 investment with an annual interest rate of 6% pay back after 3 years, using continuous compounding of interest?
interest_rate <- 0.06
investment <- 10
years <- 3

payback <- investment * exp(interest_rate * years)
payback

# In order to evaluate the quality of an investment strategy,
# we need to compute this profit for each loan in the test set
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

# What is the maximum profit of a $10 investment in any loan in the testing set 
summary(test$profit)

test$profit[which.max(test$profit)] * 10

# we will analyze an investment strategy in which the investor only purchases loans with a high interest rate 
# (a rate of at least 15%), but amongst these loans selects the ones with the lowest predicted risk of not being paid back in full. 
# We will model an investor who invests $1 in each of the most promising 100 loans.

HighInterest <- subset(test, test$int.rate >= 0.15)
summary(HighInterest)
str(HighInterest)

# What is the average profit of a $1 investment in one of these high-interest loans?
summary(HighInterest) # mean

# What proportion of the high-interest loans were not paid back in full?
table(HighInterest$not.fully.paid)
110/(327+110)

# Next, we will determine the 100th smallest predicted probability of not paying in full by sorting the predicted risks 
# in increasing order and selecting the 100th element of this sorted list. 
cutoff = sort(HighInterest$predicted.risk, decreasing=FALSE)[100]
cutoff

selectedLoans <- subset(HighInterest, HighInterest$predicted.risk <= cutoff)
summary(selectedLoans)

# How many of 100 selected loans were not paid back in full?
table(selectedLoans$not.fully.paid)

# What is the profit of the investor, who invested $1 in each of these 100 loans?
sum(selectedLoans$profit)

