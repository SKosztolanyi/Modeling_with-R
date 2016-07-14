parole <- read.csv("parole.csv")
str(parole)

# How many of the parolees in the dataset violated the terms of their parole?
table(parole$violator)

# Which variables in this dataset are unordered factors with at least three levels?
summary(parole)

parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)

# How does the output of summary() change for a factor variable as compared to a numerical variable?
summary(parole)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

TrainModel <- glm(violator ~ ., family="binomial", data = train)
summary(TrainModel)

# Consider a parolee who is male, of white race, aged 50 years at prison release, from the state of Maryland, 
# served 3 months, had a maximum sentence of 12 months, did not commit multiple offenses, and committed a larceny. 
# Answer the following questions based on the model's predictions for this individual.

step <- -4.2411574 + 1*0.3869904 + 1*0.8867192 + 50*-0.0001756 +3*-0.1238867 + 12*0.0802954 + 0.6837143
step

probability_violator <- 1.0/(1.0+ exp(-step))
probability_violator
probability_non_violator <- 1 - probability_violator
probability_non_violator

odds_violator <- exp(step)
odds_violator

# From the logistic regression equation, we have log(odds) = 
# -4.2411574 + 0.3869904*male + 0.8867192*race - 0.0001756*age + 0.4433007*state2 + 0.8349797*state3 
# - 3.3967878*state4 - 0.1238867*time.served + 0.0802954*max.sentence + 1.6119919*multiple.offenses 
# + 0.6837143*crime2 - 0.2781054*crime3 - 0.0117627*crime4. 
# This parolee has male=1, race=1, age=50, state2=0, state3=0, state4=0, time.served=3, 
# max.sentence=12, multiple.offenses=0, crime2=1, crime3=0, crime4=0. 

# We conclude that log(odds) = -1.700629.

# What is the maximum predicted probability of a violation?
predictTest = predict(TrainModel, type="response", newdata=test)
summary(predictTest)

# In the following questions, evaluate the model's predictions on the test set using a threshold of 0.5.
table(test$violator, predictTest > 0.5)

sensitivity <- 12/(12+11)
sensitivity

specificity <- 167/(167+12)
specificity

accuracy <- 179/(179+23)
accuracy

# What is the accuracy of a simple model that predicts that every parolee is a non-violator?
baseline_accuracy <- 179/202
baseline_accuracy

# The job of a parole board is to make sure that a prisoner is ready to be released into free society, 
# and therefore parole boards tend to be particularily concerned about releasing prisoners who will violate their parole.
table(test$violator, predictTest > 0.2)

#  The board assigns more cost to a false negative than a false positive, and should therefore 
# use a logistic regression cutoff less than 0.5.

# In plain: It is riskier to set possible violator free than to keep non-violator in prison longer.
# Only the safest cases are set free and those cases in which it is unclear are kept prisoners.

# Using the ROCR package, what is the AUC value for the model?
library('ROCR')
ROCRpred <- prediction(predictTest, test$violator)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

# The AUC deals with differentiating between a randomly selected positive and negative example. 
# It is independent of the regression cutoff selected.


