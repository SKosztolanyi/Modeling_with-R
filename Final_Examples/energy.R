energy <- read.csv("energy.csv")
str(energy)

# Which state in the United States seems to have the highest total generation of energy from renewable sources?
which.max( energy$GenTotalRenewable )
energy[169, ]
# ID = Idaho

# Which year did the above state produce the highest energy generation from renewable resources?
# 2000

# What is the average CO2 emissions from all sources of energy for:
# - states during years in which they voted republican?
republican <- subset(energy, presidential.results == 0)
mean(republican$AllSourcesCO2, na.rm = T)
#  16.44296

# - states during years in which they voted democrat?
democrat <- subset(energy, presidential.results == 1)
mean(democrat$AllSourcesCO2, na.rm = T)
# 5.783781

# The same in one line:
tapply(energy$AllSourcesCO2, as.factor(energy$presidential.results), mean, na.rm = T)

# Do states that voted democrat have on average higher NOx emissions than states that voted republican across all years?
mean(democrat$AllSourcesNOx, na.rm = T) > mean(republican$AllSourcesNOx, na.rm = T)

# What is the correlation between overall CO2 emissions and energy sales made to industrial facilities?
cor(energy$EsalesIndustrial, energy$AllSourcesCO2, use="complete" )
# 0.5385867

cor(energy[9:22], use = "complete")

# Create a boxplot of the total energy price (EPriceTotal) by State across the data, 
# and a table summarizing the mean of EPriceTotal by State.

?boxplot
boxplot(EPriceTotal ~ STATE, data = energy, col = "orange")
tapply(energy$EPriceTotal, energy$STATE, mean)

# Which state has the lowest average energy price of all?
min(tapply(energy$EPriceTotal, energy$STATE, mean))

# Is this state associated with the highest mean total energy generation (GenTotal)?
max(tapply(energy$GenTotal, energy$STATE, mean))

set.seed(144)
spl = sample(1:nrow(energy), size = 0.7*nrow(energy))
train = energy[spl,]
test = energy[-spl,]

mod <- glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data = train, family = "binomial")
summary(mod)

# Compute the predictions on the test set. 
# Using a threshold of 0.5, what is the accuracy of our model on the test set?

predTest <- predict(mod, newdata = test, type = "response")

table(test$GenSolarBinary, predTest > 0.5)
(154+18)/nrow(test)
# Acc is 0.8190476

testRep <- subset(test, presidential.results == 0)
testDem <- subset(test, presidential.results == 1)

predTestDem <- predict(mod, newdata = testDem, type = "response")
table(testDem$GenSolarBinary, predTestDem > 0.5)
(16+64)/nrow(testDem)
# Acc dem 0.8

predTestRep <- predict(mod, newdata = testRep, type = "response")
table(testRep$GenSolarBinary, predTestRep > 0.5)
92/110
# Acc rep 0.8363636

# Let us create a train.limited and test.limited datasets, where we only 
#keep the variables CumlRegulatory, CumlFinancial, presidential.results, Total.salary, and Import.
train.limited <- train[ ,c("CumlRegulatory", "CumlFinancial", "presidential.results", "Total.salary",  "Import")]
str(train.limited)
test.limited <- test[ ,c("CumlRegulatory", "CumlFinancial", "presidential.results", "Total.salary",  "Import")]


preproc <- preProcess(train.limited)
train.norm <- predict(preproc, train.limited)

preproc <- preProcess(test.limited)
test.norm <- predict(preproc, test.limited)
summary(train.norm)

library(flexclust)
set.seed(144)
km = kmeans(train.norm, centers = 2, iter.max = 1000)
km.kcca = as.kcca(km, train.norm)
cluster.train = predict(km.kcca)

set.seed(144)
km = kmeans(test.norm, centers = 2, iter.max = 1000)
km.kcca = as.kcca(km, test.norm)
cluster.test = predict(km.kcca)

# apply the clustering criteria on original dataset
train1 <- subset(train, cluster.train == 1)
train2 <- subset(train, cluster.train == 2)

test1 <- subset(test, cluster.test == 1)
test2 <- subset(test, cluster.test == 2)

table(train1$presidential.results)
table(train2$presidential.results)
# 0 is Republican

tapply(train1$AllSourcesCO2, train1$STATE, mean)
tapply(train2$AllSourcesCO2, train2$STATE, mean)

mod1 <- glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import,data = train1, family = "binomial")
mod2 <- glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import,data = train2, family = "binomial")
summary(mod1)
summary(mod2)

# What is the accuracy on test1, the subset of test corresponding to the first cluster?
predTest1 <- predict(mod1, newdata = test1, type = "response")
table(test1$GenSolarBinary, predTest1 > 0.5)
(114+4)/nrow(test1)
# 0.9076923

predTest1.mod <- predict(mod, newdata = test1, type = "response")
table(test1$GenSolarBinary, predTest1.mod > 0.5)
115/130
# Acc is 0.8846154

# Using the threshold of 0.5, what is the accuracy on test2,
# the subset of test corresponding to the second cluster?
predTest2 <- predict(mod2, newdata = test2, type = "response")
table(test2$GenSolarBinary, predTest2 > 0.5)
60/80
# 0.75

predTest2.mod <- predict(mod, newdata = test2, type = "response")
table(test2$GenSolarBinary, predTest2.mod > 0.5)
57/80
# 0.7125

# To compute the overall test-set accuracy of the cluster-the-predict approach, 
# we can combine all the test-set predictions into a single vector "AllPredictions" 
# and all the true outcomes into a single vector "AllOutcomes".

AllPredictions <- c(predTest1, predTest2)
AllOutcomes <- c(test1$GenSolarBinary, test2$GenSolarBinary)      

# What is the overall accuracy on the test set, using the cluster-then-predict approach, 
# again using a threshold of 0.5?

table(AllOutcomes, AllPredictions > 0.5)
(154+24)/(154+24+25+7)
# 0.847619