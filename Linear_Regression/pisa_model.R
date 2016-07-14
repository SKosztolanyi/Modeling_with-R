pisa_train <- read.csv("pisa2009train.csv")
pisa_test <- read.csv("pisa2009test.csv")
str("pisa_train")

# Using tapply() on pisaTrain, what is the average reading test score of males?
tapply(pisa_train$readingScore , pisa_train$male==1, mean)

# Which variables are missing data in at least one observation in the training set?
summary(pisa_train)

# Linear regression discards observations with missing data, so we will remove all such observations from the training and testing sets.
pisa_train <- na.omit(pisa_train)
pisa_test <- na.omit(pisa_test)
summary(pisa_train)
str(pisa_train)
str(pisa_test)

# by default R selects the first level alphabetically ("American Indian/Alaska Native") as the reference level of our factor instead of the most common level ("White")
pisa_train$raceeth <- relevel(pisa_train$raceeth, "White")
pisa_test$raceeth <- relevel(pisa_test$raceeth, "White")

# It would be time-consuming to type all the variables, but R provides the shorthand notation
pisa_train_model <- lm(readingScore ~ ., data = pisa_train)
summary(pisa_train_model)

# What is the training-set root-mean squared error (RMSE) of pisa_train_model?
SSE_train = sum(pisa_train_model$residuals^2)
SSE_train
RMSE_train1 = sqrt(SSE_train / nrow(pisa_train))
RMSE_train1
RMSE_pisa_train <- sqrt(mean(pisa_train_model$residuals^2))
RMSE_pisa_train

# What is the range between the maximum and minimum predicted reading score on the test set?
predTest <- predict(pisa_train_model, newdata = pisa_test)
summary(predTest)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 353.2   482.0   524.0   516.7   555.7   637.7 

# What is the sum of squared errors (SSE) of pisa_train_model on the testing set?
SSE_test = sum((predTest - pisa_test$readingScore)^2)
SSE_test

# What is the root-mean squared error (RMSE) of pisa_train_model on the testing set?
MSE_test <- mean((predTest - pisa_test$readingScore)^2)
MSE_test

RMSE_test <- sqrt(MSE_test)
RMSE_test

# What is the predicted test score used in the baseline model?
baseline <- mean(predTest)

# What is the sum of squared errors of the baseline model on the testing set?
Baseline_SSE_is_SST <- sum((baseline - pisa_test$readingScore)^2)
Baseline_SSE_is_SST

# What is the test-set R-squared value of lmScore?
R2_test <- 1 - SSE_test / Baseline_SSE_is_SST
R2_test

## The test-set R^2 is defined as 1-SSE/SST, where SSE is the sum of squared errors of the model on the test set 
## and SST is the sum of squared errors of the baseline model. 
## For this model, the R^2 is then computed to be 1-5762082/7802354.