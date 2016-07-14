intl <- read.csv("intl.csv")
str(intl)

# basic bar plot
ggplot(intl, aes(x=Region, y=PercentOfIntl)) +geom_bar(stat="identity") + geom_text(aes(label=PercentOfIntl))

# reorder in descending order
intl <- transform(intl, Region = reorder(x = Region, -PercentOfIntl))

str(intl)
intl$PercentOfIntl <- intl$PercentOfIntl /100

ggplot(intl, aes(x=Region, y=PercentOfIntl)) + 
      geom_bar(stat="identity",fill = "dark blue") + geom_text(aes(label=PercentOfIntl), vjust = -0.4) + 
      ylab("Percent of International Students")   +theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

intlall <- read.csv("intlall.csv", stringsAsFactors = F)
head(intlall)
intlall[is.na(intlall)] = 0

world_map <- map_data("world")
world_map <- world_map[order(world_map$group),]
str(world_map)
world_map <- merge(world_map, intlall, by.x = "region", by.y = "Citizenship")

str(world_map)
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
      geom_polygon(fill="white", color = "black") + coord_map(projection = "mercator", xlim=c(-180,180), ylim=c(-60, 90))

# this map is a list of lat long pairs that define a border of country
# merge function reordered the dataset - we need to reorder the data

world_map <- world_map[order(world_map$group, world_map$order),]
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
      geom_polygon(fill="white", color = "black") + coord_map(projection = "mercator", xlim=c(-180,180), ylim=c(-60, 90))

# the map looks better, but there are some of countries that are missing for dirrerent reasons:
# USA doesnt have foreign students in its own country from USA
# Some countries of Africa don't send students to MIT
# China has different names in two datasets, so it didn't match up

table(intlall$Citizenship)
intlall$Citizenship[intlall$Citizenship == "China (People's Republic Of)"] = "China"

# we need to merge again:
world_map <- merge(map_data("world"), intlall, by.x = "region", by.y = "Citizenship")
world_map <- world_map[order(world_map$group, world_map$order),]
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
      geom_polygon(aes(fill=Total), color = "black") + coord_map(projection = "mercator", xlim=c(-180,180), ylim=c(-60, 90))

# another plotting technique - respahe data frame and plot:
households <- read.csv("households.csv")
str(households)

# ggplot needs data in form:
# year / group / fraction
# we therefore we need to use melt function from reshape2 package
library(reshape2)
households[,1:2]
head(melt(households, id="Year"))
melt(households, id="Year")[1:12,]
# Every value has a new row in melted data frame

ggplot(melt(households, id = "Year"), aes(x=Year, y = value, color = variable)) +
      geom_line(size=1) + geom_point(size=3) + ylab("Percentage of Households")
