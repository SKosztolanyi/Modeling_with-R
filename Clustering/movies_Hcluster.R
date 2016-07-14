movies <- read.table("movies.csv", header=FALSE, sep = "|", quote = "\"")
str(movies)

colnames(movies) <- c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Advanture", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror","Musical", "Mystery", "Romance", "SciFi", "Thriller","War", "Western")
str(movies)

# remove redundant columns
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
movies <- unique(movies)

str(movies)
# How many movies are classified as romance AND drama?
str(subset(movies, movies$Romance == 1 & movies$Drama))
table(movies$Romance, movies$Drama)
# 97

distances <- dist(movies[2:20], method = "euclidean")
clusterMovies <- hclust(distances, method= "ward.D")
plot(clusterMovies)

# we create 10 clusters manually
clusterGroups <- cutree(clusterMovies, k = 10)

# Calculate mean of a variable in a cluster groups:
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

spl = split(movies[2:20], clusterGroups)

genres_in_clusters <- lapply(spl, colMeans)
# The lapply function runs the second argument (colMeans) on each element of the first argument (each cluster subset in spl). 
# So instead of using 19 tapply commands, or 10 colMeans commands,
# we can output our centroids with just two commands: one to define spl, and then the lapply command.

genres_in_clusters

cluster2 <- subset(movies, clusterGroups == 2)
cluster2$Title[1:10]

colSums(movies[2:20])
