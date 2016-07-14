# Image pixel clustering
flower <- read.csv("flower.csv", header = FALSE)
# flower is a simple example dataset
str(flower)
flowerMatrix <- as.matrix(flower)
str(flowerMatrix)

flowerVector <- as.vector(flowerMatrix)
str(flowerVector)

distanceMatrix <- dist(flowerVector, method = "euclidean")
str(distanceMatrix)

clusterIntensity <- hclust(distanceMatrix, method = "ward.D2")
# I need to use D2, otherwise plotting will fail the R session with fatal error.
plot(clusterIntensity)

# make 3 clusters of hclust
rect.hclust(clusterIntensity, k = 3, border = "red")
flowerClusters <- cutree(clusterIntensity, k=3)
flowerClusters

tapply(flowerVector, flowerClusters, mean)
dim(flowerClusters) <- c(50, 50)
#image according to cluster
image(flowerClusters, axes = F)

#original image
image(flowerMatrix, axes = F, col = grey(seq(0,1,length = 256)))


# new file:
healthy <- read.csv("healthy.csv", header = F)
healthyMartix <- as.matrix(healthy)
str(healthyMartix)
image(healthyMartix, axes = F, col=grey(seq(0,1, length = 256))) # 0 is black, 1 is white

# capturing clusters in image data
healthyVector <- as.vector(healthyMartix)
distance <- dist(healthyVector, method = "euclidean")
# too big vector
str(healthyVector)
n = 365536
dist_calc <- n*(n-1)/2 # number of calculated distances
dist_calc
# 66808100880 - 67 billion
# This means, we cannot use hierarchical clustering and we will use k-means clustering instead
k <- 5
set.seed(1)
KMC <- kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

healthyClusters <- KMC$cluster
summary(healthyClusters)

dim(healthyClusters) <- c(nrow(healthyMartix), ncol(healthyMartix))
# rainbow palette for image output
image(healthyClusters, axes = F, col = rainbow(k))


# SCREE PLOT
# One common way to select the number of clusters is by using a scree plot, which works for any clustering algorithm.
# A standard scree plot has the number of clusters on the x-axis, and the sum of the within-cluster sum of squares on the y-axis.

KMC2 <- kmeans(healthyVector, centers = 2, iter.max = 1000)
KMC2$withinss
SumWithinss = sapply(2:10, function(x) sum(kmeans(healthyVector, centers=x, iter.max=1000)$withinss))
# sapply is like lambda (anonymous function) in python?
SumWithinss
NumClusters = seq(2,10,1)
plot(NumClusters, SumWithinss, type="b")
# To determine the best number of clusters using this plot, we want to look for a bend, or elbow, in the plot.

# about apply functions: http://www.r-bloggers.com/using-apply-sapply-lapply-in-r/

# Detecting Tumors
tumor <- read.csv("tumor.csv", header = F)
tumorMatrix <- as.matrix(tumor)
tumorVector <- as.vector(tumorMatrix)

# We will apply our prepared clustering algorithm from healthy brain (tumor is a testing set)
library(flexclust)
KMC.kcca <- as.kcca(KMC, healthyVector)
tumorClusters <- predict(KMC.kcca, newdata = tumorVector)
dim(tumorClusters) <- c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = F, col = rainbow(k))

image(t(tumorClusters)[, nrow(tumorClusters):1], axes = FALSE, col = rainbow(k))
