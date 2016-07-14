letters <- read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")

set.seed(1000)
library(caTools)
split <- sample.split(letters$isB, SplitRatio = 0.5)
train <- subset(letters, split == TRUE)
test <- subset(letters, split == FALSE)

#  What is the accuracy of this baseline method on the test set?
table(test$isB)
1175/(1175+383)

#Now build a classification tree to predict whether a letter is a B or not, using the training set to build your model. 
# Remember to remove the variable "letter" out of the model, as this is related to what we are trying to predict! 
CARTb = rpart(isB ~ . - letter, data=train, method="class")

predict_CART <- predict(CARTb, newdata = test, type = "class")
table(test$isB, predict_CART)

accuracy <- (1118+340)/nrow(test)
accuracy
#  0.9358151

library(randomForest)
set.seed(1000)
isB_Forest <- randomForest(isB ~ . - letter, data=train)
predict_isB_forest <- predict(isB_Forest, newdata = test)
summary(predict_isB_forest)

table(test$isB, predict_isB_forest)
accuracy_forest <- (1165+374)/nrow(test)
accuracy_forest
# 0.9878049

# Let us now move on to the problem that we were originally interested in, 
# which is to predict whether or not a letter is one of the four letters A, B, P or R.
letters$letter = as.factor( letters$letter )

set.seed(2000)
split_4 <- sample.split(letters$letter, SplitRatio = 0.5)
train_4 <- subset(letters, split_4==TRUE)
test_4 <- subset(letters, split_4==FALSE)

table(test_4$letter)
baseline_accuracy <- 401/nrow(test)
baseline_accuracy
#  0.2573813
CART_4 = rpart(letter ~ . - isB, data=train_4, method="class")

predict_CART_4 <- predict(CART_4, newdata = test_4, type = "class")
table(test_4$letter, predict_CART_4)
CART_4_accuracy = (348+318+363+340)/nrow(test)
CART_4_accuracy
# 0.8786906

set.seed(1000)
Forest_4 <- randomForest(letter ~ . - isB, data=train_4)
predict_forest_4 <- predict(Forest_4, newdata = test_4)
table(test_4$letter, predict_forest_4)
forest_accuracy <- (390+380+393+364)/nrow(test_4)
forest_accuracy
# 0.9801027