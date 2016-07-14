## Clustering
female_feminist <- subset(trainsub7, Gender == "Female" & Q109244 == "Yes")
table(female_feminist$Gender, female_feminist$Q109244 == "Yes")
trainsub8<- trainsub7

model_train <- glm(Party ~ Income +HouseholdStatus+EducationLevel+Q113181+Q115611+Q115195+Q120012+Q108342+Q98869+Q115899  , data = female_feminist, family = binomial)
predict_train <- predict(model_train, newdata = female_feminist, type = "response")

summary(predict_train)

table(female_feminist$Party, predict_train > 0.38)
(1962+1407)/nrow(trainsub)
# 0.6050647

summary(female_feminist)

trainsub8$gender_feminist <- as.numeric(trainsub8$Gender)*as.numeric(trainsub8$Q109244)
table(trainsub8$gender_feminist)

model_train <- glm(Party ~ . -USER_ID, data = trainsub8, family = binomial)
predict_train <- predict(model_train, newdata = trainsub8, type = "response")

table(trainsub8$Party, predict_train > 0.51)
(2040+1533)/nrow(trainsub8)
# 0.6417026

trainsub9<- trainsub7
trainsub9$Female_Feminist <- ifelse(trainsub9$Gender=="Female" & trainsub9$Q109244 == "Yes", 10, 0)
trainsub9$Female_Not_Feminist <- ifelse(trainsub9$Gender=="Female" & trainsub9$Q109244 == "No", 10, 0)
# trainsub9$Male_Feminist <- ifelse(trainsub9$Gender=="Male" & trainsub9$Q109244 == "Yes", 10, 0)
table(trainsub9$Female_Feminist)

model_train <- glm(Party ~ . -USER_ID, data = trainsub9, family = binomial)
predict_train <- predict(model_train, newdata = trainsub9, type = "response")

table(trainsub9$Party, predict_train > 0.5)
(2004+1572)/nrow(trainsub9)
# 0.6422414

#added female not a feminist
table(trainsub9$Party, predict_train > 0.51) # as well as 0.5
(2036+1542)/nrow(trainsub9)
(1964+1610)/nrow(trainsub9)
# 0.6426006

summary(model_train)

table(trainsub9$Party, trainsub9$Q113181)

library(rpart)
library(rpart.plot)
cart_model <- rpart(Party ~ . -USER_ID, data = trainsub9, cp=0.005)
cart_predict <- predict(cart_model, newdata = trainsub9, type = "class")
rpart.plot(cart_model)
table(trainsub9$Party, cart_model_predict)
#weak model

str(trainsub9)
table(trainsub9$Q109244*trainsub9$Gender)

library(caret)
library(e1071)
tc <- trainControl("cv", 10, savePredictions=T) 
fit <- train(Party ~ . - USER_ID, data=trainsub9, method="glm", family="binomial", trControl = tc)
summary(fit)$coef
summary(fit)
fitpred <- fit$finalModel$fitted.values
fitpredt <- function(t) ifelse(fitpred > t , 1,0)
str(fitpredt)
confusionMatrix(fitpredt(0.5),trainsub9$Party)

table(trainsub9$Party, fitpred>0.509)
fit

numfolds<- trainControl(method="cv", number=10)
cpGrid<- expand.grid(.cp=seq(0.01, 0.5, 0.01))
fit2 <- train(Party~.-USER_ID, data = trainsub9, method ="glm", family=binomial, trControl =numfolds)
fitpred2 <- fit2$finalModel$fitted.values
fitpredt2 <- function(t) ifelse(fitpred2 > t , 1,0)

testsub9 <- testsub7
testsub9$Female_Feminist <- ifelse(testsub9$Gender=="Female" & testsub9$Q109244 == "Yes", 10, 0)
testsub9$Female_Not_Feminist <- ifelse(testsub9$Gender=="Female" & testsub9$Q109244 == "No", 10, 0)
testsub9$Male_Feminist <- ifelse(testsub9$Gender=="Male" & testsub9$Q109244 == "Yes", 10, 0)

predict_test9 <- predict(model_train, newdata = testsub9, type = "response")

mylabelsF <-  as.factor(ifelse(predict_test9<0.505, "Democrat", "Republican"))
MySubmission = data.frame(USER_ID = testsub9$USER_ID, Predictions = mylabelsF)
MySubmission[is.na(MySubmission)] <- "Democrat"
summary(MySubmission)
table(mylabelsF)
write.csv(MySubmission, "Upgraded_GLM4.csv", row.names=FALSE)
## The best so far 0.64080 - 0.5 threshold

trainsub10 <- trainsub9[ ,c("Party", "Income", "Gender", "HouseholdStatus", "EducationLevel", "Q109244", "Q115611", 
                           "Q113181", "Female_Feminist", "Female_Not_Feminist", "Male_Feminist")]
library(randomForest)
set.seed(100)
# the dependent variable need to be a factor to build a logistic forest, otherwise it build a regression model

votingForest <- randomForest(Party ~ ., data = trainsub10)
## , nodesize = 25, ntree = 500
predictForest <- predict(votingForest, newdata = trainsub10)
table(trainsub10$Party, predictForest)

testsub10 <- testsub9[ ,c("Income", "Gender", "HouseholdStatus", "EducationLevel", "Q109244", "Q115611", 
                            "Q113181", "Female_Feminist", "Female_Not_Feminist", "Male_Feminist")]
predictTestForest <- predict(votingForest, newdata = testsub10)

table(predictTestForest)
mylabelsF <- as.factor(predictTestForest)
MySubmissionForest = data.frame(USER_ID = test$USER_ID, Predictions = mylabelsF)
write.csv(MySubmissionForest, "FifthRF.csv", row.names=FALSE)
#### It was the best so far, with 0.64224 accuracy
