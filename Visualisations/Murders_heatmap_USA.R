mvt <- read.csv("mvt.csv", stringsAsFactors = F)
str(mvt)

#format date
mvt$Date <- strptime(mvt$Date, format = "%m/%d/%y %H:%M")

#days of week
mvt$Weekday <- weekdays(mvt$Date)
mvt$Hour <- mvt$Date$hour
str(mvt)

WeekdayCounts <- as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)

library(ggplot2)

ggplot(data=WeekdayCounts, aes(x=Var1, y = Freq)) + geom_line(aes(group=1))
#days of week are out of order - it is alphabetical order not chronological

WeekdayCounts$Var1 <- factor(WeekdayCounts$Var1, ordered = T, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ggplot(data=WeekdayCounts, aes(x=Var1, y = Freq)) + geom_line(aes(group=1)) +xlab("Day of Week") + ylab("Total MotorVehicleTheft")

table(mvt$Weekday, mvt$Hour)
DayHourCounts <- as.data.frame(table(mvt$Weekday, mvt$Hour))
str(DayHourCounts)

DayHourCounts$Hour <- as.numeric(as.character(DayHourCounts$Var2))
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1), size = 1) 
# 7 lines is a lot, so we will visualize this info in a heatmap

DayHourCounts$Var1 <- factor(DayHourCounts$Var1, ordered = T, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#Removing y label and adding the label to legend, changing color of heatmap
ggplot(DayHourCounts, aes(x=Hour, y=Var1)) +geom_tile(aes(fill=Freq)) +scale_fill_gradient(name="Total MV thefts", low = "white", high = "red") + theme(axis.title.y = element_blank())

library(ggmap)
library(maps)

chicago <- get_map(location = "chicago", zoom = 11)
ggmap(chicago)
ggmap(chicago) + geom_point(data=mvt[1:100,], aes(x=Longitude, y = Latitude))

#making Lat Lon groups
LatLonCounts <- as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude, 2)))
str(LatLonCounts)
# changing to number
LatLonCounts$Long <- as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat <- as.numeric(as.character(LatLonCounts$Var2))

# low is yellow, high is red
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long, y = Lat, color = Freq, size = Freq)) + scale_color_gradient(low = "yellow", high = "red")

# geographical heatmap, a map like people use for predictive policing
ggmap(chicago) + geom_tile(data=LatLonCounts, aes(x=Long, y = Lat, alpha = Freq, size = Freq), fill = "red")

# removing points with 0 frequency
LatLonCounts2 <- subset(LatLonCounts, LatLonCounts$Freq > 0)

ggmap(chicago) + geom_point(data=LatLonCounts2, aes(x=Long, y = Lat, color = Freq, size = Freq)) + scale_color_gradient(low = "yellow", high = "red")

ggmap(chicago) + geom_tile(data=LatLonCounts2, aes(x=Long, y = Lat, alpha = Freq, size = Freq), fill = "red")

#other csv file for the whole USA
murders<- read.csv("murders.csv")
str(murders)

statesMap <- map_data("state")
str(statesMap)
ggplot(statesMap, aes(x=long, y=lat, group = group)) + geom_polygon(fill="white", color="black")

#comparing names of states: Are they the same?
murders$region <- tolower( murders$State)

#merging two tables on field with the same name:
murdersMap <- merge(statesMap, murders, by="region")

str(murdersMap)
ggplot(murdersMap, aes(x=long, y=lat, group = group, fill=Murders)) + geom_polygon(color="black") +scale_fill_gradient(low="black", high = "red", guide="legend")

# checking population
ggplot(murdersMap, aes(x=long, y=lat, group = group, fill=Population)) + geom_polygon(color="black") +scale_fill_gradient(low="black", high = "red", guide="legend")

# the murder map was basically a population map. We need to plot murder rate instead.
murdersMap$MurderRate <- murdersMap$Murders/murdersMap$Population*100000

ggplot(murdersMap, aes(x=long, y=lat, group = group, fill=MurderRate)) + geom_polygon(color="black") +scale_fill_gradient(low="black", high = "red", guide="legend")

# removing outlier - washingtonDC
ggplot(murdersMap, aes(x=long, y=lat, group = group, fill=MurderRate)) + 
      geom_polygon(color="black") +scale_fill_gradient(low="black", high = "red", guide="legend", limits = c(0,10))

# gun_ownership
ggplot(murdersMap, aes(x=long, y=lat, group = group, fill=GunOwnership)) + geom_polygon(color="black") +scale_fill_gradient(low="black", high = "red", guide="legend") +
      theme(text("dog"))
