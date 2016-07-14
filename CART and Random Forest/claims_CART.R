claims <- read.csv("ClaimsData.csv")
str(claims)

table(claims$bucket2009)/nrow(claims)

library(caTools)

set.seed(88)
spl <- sample.split(claims$bucket2009, SplitRatio = 0.6)
claimsTrain <- subset(claims, spl == TRUE)
claimsTest <- subset(claims, spl == FALSE)

summary(claimsTrain)
table(claims$diabetes)

174254/(174254+283751)
