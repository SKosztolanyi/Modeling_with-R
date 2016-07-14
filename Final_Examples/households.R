households <- read.csv("Households.csv")
str(households)
summary(households)

# How many households have logged transactions at the retailer only in the morning?
table(households$MorningPct)
# 4

table(households$AfternoonPct)
# 13

# Of the households that spend more than $150 per transaction on average, 
# what is the minimum average discount per transaction?
avg150 <- subset(households, AvgSalesValue >= 150)
avg150
# 15.64607

# Of the households who have an average discount per transaction greater than 25%, 
# what is the minimum average sales value per transaction?
disc25 <- subset(households, AvgDiscount >= 25)
min(disc25$AvgSalesValue)
# 50.1175

# In the dataset, what proportion of households visited the retailer at least 300 times?
vis300 <- subset(households, NumVisits >= 300)
nrow(vis300)/nrow(households)
# 0.0592

# If you clustered this dataset without normalizing, 
# which variable would you expect to dominate in the distance calculations?
summary(households)
# NumVisits

library(caret)

preproc <- preProcess(households)
HouseholdsNorm <- predict(preproc, households)

# for each variable, the normalization process subtracts the mean 
# and divides by the standard deviation

# What is the maximum value of NumVisits in the normalized dataset?
summary(HouseholdsNorm)
# 10.2828

# What is the minimum value of AfternoonPct in the normalized dataset?
# -3.22843

set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)

# Based on the dendrogram, how many clusters do you think would be appropriate for this problem?
# 2, 3 or 5

set.seed(200)

KMC <- kmeans(HouseholdsNorm, centers = 10, iter.max = 1000)
str(KMC)
KMC

householdClusters <- KMC$cluster
summary(householdClusters)

# How many observations are in the smallest cluster?
table(householdClusters)
# 51
# How many observations are in the largest cluster?
# 504

# Which cluster best fits the description "morning shoppers stopping in to make a quick purchase"?
KMeansCluster1 <- subset(households, householdClusters == 1)
summary(KMeansCluster1)
KMC$centers
# cluster 4

# Which cluster best fits the description "shoppers with high average product count and high average value per visit"?
# cluster 2

# Which cluster best fits the description "frequent shoppers with low value per visit"?
# cluster 9

# If we ran hierarchical clustering a second time without making any additional calls to set.seed, we would expect:
# Ans:  Identical results to the first hierarchical clustering 

# If we ran k-means clustering a second time without making any additional calls to set.seed, we would expect:
# Ans:  Different results from the first k-means clustering 

# If we ran k-means clustering a second time, running the command set.seed(100) right before doing the clustering, we would expect:
# Ans:  Different results from the first k-means clustering

# To get more general clusters, the number of clusters should be decreased. 
# To get more specific clusters, the number of clusters should increase.

set.seed(5000)
KMC2 <- kmeans(HouseholdsNorm, centers = 5, iter.max = 1000)
household5Clusters <- KMC2$cluster

# How many observations are in the smallest cluster?
table(household5Clusters)
# 172

# How many observations are in the largest cluster?
# 994

# Using the cluster assignments from k-means clustering with 5 clusters, 
# which cluster best fits the description "frequent shoppers with low value per visit"?
kmc2centers <- KMC2$centers
kmc2centers
# cluster 4

# The cluster centroid shows average behavior in a single cluster - 
# it does not describe every single observation in that cluster or tell us how the cluster compares to other clusters.

# A box plot of NumVisits shows the distribution of the number of visits of the households, 
# and we want to subdivide by cluster. 
# Alternatively, ggplot with y as the cluster and x as the number of visits plots the data,
# but only geom_point is appropriate to show the distribution of the data.

ggplot(data = kmc2centers, geom_point(kmc2centers$NumVisits))
kmc2centers
