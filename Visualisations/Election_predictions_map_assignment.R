statesMap = map_data("state")
str(statesMap)
table(statesMap$group)

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

polling <- read.csv("pollingImputed.csv")

Train <- subset(polling, Year == 2008 | Year == 2004)
Test <- subset(polling, Year == 2012)

str(Train)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
# probabilities
TestPrediction = predict(mod2, newdata=Test, type="response")

# binary
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
str(predictionDataFrame)

# For how many states is our binary prediction 1 (for 2012), corresponding to Republican?
table(TestPredictionBinary)

# What is the average predicted probability of our model (on the Test set, for 2012)?
summary(predictionDataFrame)

# Now, we need to merge "predictionDataFrame" with the map data "statesMap"
predictionDataFrame$region <- tolower(predictionDataFrame$Test.State)
predictionMap <- merge(statesMap, predictionDataFrame, by = "region")
predictionMap <- predictionMap[order(predictionMap$order),]

str(predictionMap)
str(statesMap)
?merge

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
      geom_polygon(color = "black")

# We see that the legend displays a blue gradient for outcomes between 0 and 1. 
# However, when plotting the binary predictions there are only two possible outcomes: 0 or 1.
# plotting binary
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + 
      scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

# plotting probability
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black") + 
      scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")

# What was our predicted probability for the state of Florida?
table( predictionMap$TestPrediction, predictionMap$region)
predictionDataFrame

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", alpha=0.3) + 
      scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")

?geom_polygon

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black", alpha=0.3) + 
      scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
