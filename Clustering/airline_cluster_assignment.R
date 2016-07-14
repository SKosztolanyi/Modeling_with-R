airlines <- read.csv("airlinesCluster.csv")
summary(airlines)

# normalizing variables with caret:
library(caret)
preproc <- preProcess(airlines)
airlinesNorm <- predict(preproc, airlines)
summary(airlinesNorm)
sd(airlinesNorm$Balance) # all have sd 1

# calculating distance
airDist = dist(airlinesNorm, method="euclidean")
airClust = hclust(airDist, method="ward.D")
plot(airClust)

clusterGroups <- cutree(airClust, k = 5)
table(clusterGroups)

tapply(airlines$Balance, clusterGroups, mean)
tapply(airlines$QualMiles, clusterGroups, mean)
tapply(airlines$BonusMiles, clusterGroups, mean)
tapply(airlines$BonusTrans, clusterGroups, mean)
tapply(airlines$FlightMiles, clusterGroups, mean)
tapply(airlines$FlightTrans, clusterGroups, mean)
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)

# k-means
set.seed(88)
k <- 5
airKMC <- kmeans(airlinesNorm, centers = k, iter.max = 1000)
str(airKMC)
# How many clusters have more than 1,000 observations?
table(airKMC$cluster)

table(airKMC$centers)
