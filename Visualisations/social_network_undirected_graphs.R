# For this assignment, we will visualize social networking data using anonymized data from Facebook
users <- read.csv("users.csv")
edges <- read.csv("edges.csv")
str(edges)
str(users)

# In our dataset, what is the average number of friends per user?
# number of edges times two divided by number of users
146*2/59

# Out of all the students who listed a school, what was the most common locale?
table(users$locale, users$school)

# Is it possible that either school A or B is an all-girls or all-boys school?
table(users$gender, users$school)

library(igraph)

# Which of the following commands will create a graph g describing our social network, 
# with the attributes of each user correctly loaded?
g = graph.data.frame(edges, directed = FALSE, vertices = users)
plot(g, vertex.size=5, vertex.label=NA)

# In this graph, there are a number of groups of nodes where all the nodes in each group are connected but the groups are disjoint from one another, forming "islands" in the graph.
# Such groups are called "connected components," or "components" for short.
# How many connected components with at least 2 nodes are there in the graph?
4

# How many users are friends with 10 or more other Facebook users in this network?
degree(g)
table(degree(g) >= 10)

# To visually draw attention to these nodes, we will change the size of the vertices so the vertices with high degrees are larger.
V(g)$size = degree(g)/2+2

plot(g, vertex.label=NA)

# We can update the colors by setting the color to black for all vertices, 
# then setting it to red for the vertices with gender A and setting it to gray for the vertices with gender B
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"

# What is the gender of the users with the highest degree in the graph?
plot(g, vertex.label=NA)
# gender B

# Now, color the vertices based on the school that each user in our network attended.
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "orange"
V(g)$color[V(g)$school == "AB"] = "green"

# Are the two users who attended both schools A and B Facebook friends with each other?
plot(g, vertex.label=NA)

# Now, color the vertices based on the locale of the user.
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "blue"
V(g)$color[V(g)$locale == "B"] = "yellow"

# The large connected component is most associated with which locale?
plot(g, vertex.label=NA)

?igraph.plotting

library(rgl)
rglplot(g, vertex.label=NA)

# What parameter to the plot() function would we use to change the edge width when plotting g?
edge.width
