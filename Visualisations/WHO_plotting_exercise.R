WHO <- read.csv("WHO.csv")
str(WHO)

plot(WHO$GNI, WHO$FertilityRate)

# same plot with ggplot2
library(ggplot2)

# data, aesthetic mapping, geometric object
scatter <- ggplot(WHO, aes(x = GNI, y = FertilityRate))
scatter + geom_point()
scatter + geom_line()
# not a good representation

scatter + geom_point(color = "blue", size = 3, shape = 17)
scatter + geom_point(color = "darkred", size = 3, shape = 8) + ggtitle("Fertility rate VS GNI")

# print a plot into a pdf file
fertilityGNIplot <- scatter + geom_point(color = "darkred", size = 3, shape = 8) + ggtitle("Fertility rate VS GNI")
pdf("TestPlot.pdf")
print(fertilityGNIplot)
# close connection to pdf
dev.off()

ggplot(WHO, aes(x= GNI, y= FertilityRate, color = Region)) + geom_point()

ggplot(WHO, aes(x= GNI, y= FertilityRate, color = LifeExpectancy)) + geom_point()

ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()
# to simulate linear relationship, we log the fertility rate
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point()

model = lm(Under15 ~ log(FertilityRate), data = WHO)
summary(model)

# adding regression line with 99% confidence interval
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() +stat_smooth(method = "lm", level = 0.99)

ggplot(WHO, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point()


