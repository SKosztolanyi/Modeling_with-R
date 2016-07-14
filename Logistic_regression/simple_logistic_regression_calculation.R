# This is a simple calculation of odds and probabilities for logistic regression:
B0 <- -1.5
B1 <- 3
B2 <- -0.5
x1 <- 1
x2 <- 5

step<- B0 + B1*x1 + B2*x2
step

P_y_1 <- 1.0/(1.0+ exp(-step))
P_y_1
P_y_0 <- 1 - P_y_1
P_y_0
ODDS <- P_y_1/P_y_0
ODDS

logit <- log(ODDS)
logit

logit2 = step
logit2

ODDS2 <- exp(logit2)
ODDS2
