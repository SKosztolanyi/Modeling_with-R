dailykos <- read.csv("dailykos.csv")
str(dailykos)

kosDist = dist(dailykos, method="euclidean")
kosHierClust = hclust(kosDist, method="ward.D")
# If I use ward.D2 I get different result than expected

plot(kosHierClust)

# Let's pick 7 clusters. This number is reasonable according to the dendrogram, and also seems reasonable for the application. 
# Use the cutree function to split your data into 7 clusters.

rect.hclust(kosHierClust, k = 7, border = "red")
clusterGroups <- cutree(kosHierClust, k = 7)
table(clusterGroups)

HierCluster1 <- subset(dailykos, clusterGroups == 1)
HierCluster2 <- subset(dailykos, clusterGroups == 2)
HierCluster3 <- subset(dailykos, clusterGroups == 3)
HierCluster4 <- subset(dailykos, clusterGroups == 4)
HierCluster5 <- subset(dailykos, clusterGroups == 5)
HierCluster6 <- subset(dailykos, clusterGroups == 6)
HierCluster7 <- subset(dailykos, clusterGroups == 7)

# advanced approach:
HierCluster <- split(dailykos, clusterGroups)
# selecting subset:
HierCluster[[2]]

# look at top 6 words of a cluster:
tail(sort(colMeans(HierCluster1)))

tail(sort(colMeans(HierCluster2)))
tail(sort(colMeans(HierCluster3)))
tail(sort(colMeans(HierCluster4)))
# Which cluster could best be described as the cluster related to the Iraq war?
tail(sort(colMeans(HierCluster5)))
#       american       presided administration            war           iraq           bush 
#       1.090909       1.120393       1.230958       1.776413       2.427518       3.941032 
tail(sort(colMeans(HierCluster6)))
tail(sort(colMeans(HierCluster7)))

# Kmeans
set.seed(1000)
k <- 7
kosKMC <- kmeans(dailykos, centers = k)
str(kosKMC)

kosClusters <- kosKMC$cluster
summary(kosClusters)
table(kosClusters)

kosClusters
Kcluster1 <- subset(dailykos, kosClusters ==1)
Kcluster2 <- subset(dailykos, kosClusters ==2)
Kcluster3 <- subset(dailykos, kosClusters ==3)
Kcluster4 <- subset(dailykos, kosClusters ==4)
Kcluster5 <- subset(dailykos, kosClusters ==5)
Kcluster6 <- subset(dailykos, kosClusters ==6)
Kcluster7 <- subset(dailykos, kosClusters ==7)

tail(sort(colMeans(Kcluster1)))
tail(sort(colMeans(Kcluster2)))
tail(sort(colMeans(Kcluster3)))
tail(sort(colMeans(Kcluster4)))
tail(sort(colMeans(Kcluster5)))
tail(sort(colMeans(Kcluster6)))
tail(sort(colMeans(Kcluster7)))

# Use the table function to compare the cluster assignment of hierarchical clustering to the cluster assignment of k-means clustering.
table(clusterGroups, kosClusters)
#             kosClusters
#clusterGroups    
#     1    2    3    4    5    6    7
#1    3   11   64 1045   32    0  111
#2    0    0    0    0    0  320    1
#3   85   10   42   79  126    8   24
#4   10    5    0    0    1    0  123
#5   48    0  171  145    3    1   39
#6    0    2    0  712    0    0    0
#7    0  116    0   82    1    0   10