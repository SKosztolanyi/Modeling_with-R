## Introduction:
# We would like to estimate influenza-like illness (ILI) activity using Google web search logs.
# Google Search Queries - Google Trends allows public retrieval of weekly counts for every query searched by users around the world. 
# The csv file FluTrain.csv aggregates this data from January 1, 2004 until December 31, 2011

# "Week" - The range of dates represented by this observation, in year/month/day format.

# ILI" - This column lists the percentage of ILI-related physician visits for the corresponding week.

# "Queries" - This column lists the fraction of queries that are ILI-related for the corresponding week, 
# adjusted to be between 0 and 1 (higher values correspond to more ILI-related search queries).

flu_train <- read.csv("FluTrain.csv")
summary(flu_train)

# which week corresponds to the highest percentage of ILI-related physician visits?
flu_train[which.max(flu_train$ILI), ]

# Which week corresponds to the highest percentage of ILI-related query fraction?
flu_train[which.max(flu_train$Queries), ]

# Plot the histogram of the dependent variable, ILI. 
# What best describes the distribution of values of ILI?
hist(flu_train$ILI)

# Answer:
# Most of the ILI values are small, with a relatively small number of much larger values (in statistics, this sort of data is called "skew right"). 
#Most of the ILI values are small, with a relatively small number of much larger values (in statistics, this sort of data is called "skew right").

# Plot the natural logarithm of ILI versus Queries. What does the plot suggest?
plot(log(flu_train$ILI), flu_train$Queries)

# Based on our understanding of the data from the previous subproblem, which model best describes our estimation problem?
# Answer:
# log(ILI) = intercept + coefficient x Queries, where the coefficient is positive 

# What is the training set R-squared value for FluTrend1 model?
FluTrend1 <- lm(log(ILI) ~ Queries, data = flu_train)
summary(FluTrend1)

# What is the relationship we infer from our problem? 
flu_cor <- cor(log(flu_train$ILI), flu_train$Queries)
flu_cor
flu_cor^2

# What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012?
FluTest <- read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
PredTest1
FluTest$Week

which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[11]

# What is the relative error betweeen the estimate (our prediction) and the observed value for the week of March 11, 2012?
(FluTest$ILI[11]-PredTest1[11])/FluTest$ILI[11]

# 0.04623827

# What is the Root Mean Square Error (RMSE) between our estimates and the actual observations
# for the percentage of ILI-related physician visits, on the test set?

RMSE_flu <- sqrt(mean((PredTest1 - FluTest$ILI)^2))
RMSE_flu 

#  0.7490645