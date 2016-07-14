songs <- read.csv("songs.csv")
summary(songs)

# How many observations (songs) are from the year 2010?
table(songs$year)

# How many songs does the dataset include for which the artist name is "Michael Jackson"?
table(songs$artistname)

MJ<- subset(songs,songs$artistname=="Michael Jackson")

# Which songs by Michael Jackson made it to the Top 10? 
MJ_top <- subset(MJ, Top10 == 1)
MJ_top

MJ[c("songtitle", "Top10")]

# What are the values of timesignature that occur in our dataset?
table(songs$timesignature)

# Out of all of the songs in our dataset, the song with the highest tempo is one of the following songs. Which one is it?
which.max(songs$tempo)
songs$songtitle[which.max(songs$tempo)]

# How many observations (songs) are in the training set?
train <- subset(songs, songs$year < 2010)
str(train)

test <- subset(songs, songs$year > 2009)
str(test)

nrow(train) + nrow(test) == nrow(songs)
# [1] TRUE

# I want to include in the model only numeric variables
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain <- train[, !(names(train)%in% nonvars)]
str(SongsTrain)
SongsTest <- test[, !(names(train)%in% nonvars)]

SongsLog1 <- glm(Top10 ~ ., data = SongsTrain, family = binomial)
summary(Model1)

# What is the correlation between the variables "loudness" and "energy" in the training set?
cor(songs$loudness, songs$energy)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)

# Look at the summary of SongsLog2, and inspect the coefficient of the variable "energy". What do you observe?
summary(SongsLog2)
# Energy is no logner negative, but positive.
summary(SongsLog3)

predictTest <- predict(SongsLog3, type = "response", newdata = SongsTest)
summary(predictTest)

# What is the accuracy of Model 3 on the test set, using a threshold of 0.45?
table(SongsTest$Top10, predictTest > 0.45)
accuracy_of_0.45 <- (309+19)/(309+5+40+19)
accuracy_of_0.45
#  0.8793566

#    FALSE  TRUE
#0   309    5
#1    40    19

baseline_songs <- (309+5)/(349+24)
baseline_songs

# How many non-hit songs does Model 3 predict will be Top 10 hits (again, looking at the test set), using a threshold of 0.45?
5

# How many songs does Model 3 correctly predict as Top 10 hits in 2010 (remember that all songs in 2010 went into our test set), 
#using a threshold of 0.45?
19
senzitivity <- 19/59
senzitivity

specificity <- 309/314
specificity
