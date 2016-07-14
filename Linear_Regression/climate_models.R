setwd("C:/Users/stefan/Google Drive OldZZZ/Data Science/Coursera + Edx/Analytics Edge/Week_2")

climate<- read.csv("climate_change.csv")

training_climate <- subset(climate, climate$Year <= 2006)

str(training_climate)

summary(training_climate)

test_climate <- subset(climate, climate$Year > 2006)

str(test_climate)

climate_model1 <- lm(Temp~MEI +  CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=training_climate)

summary(climate_model1)

cor(training_climate)

climate_model2 <- lm(Temp~MEI+TSI+Aerosols+N2O, data = training_climate)

summary(climate_model2)

stepped_model <- step(climate_model1)

summary(stepped_model)

temp_prediction <- predict(stepped_model, newdata = test_climate)

climate_SSE <- sum((temp_prediction - test_climate$Temp)^2)
climate_SSE

climate_SST <- sum((mean(training_climate$Temp) - test_climate$Temp)^2)
climate_SST

climate_R2 <- 1 - climate_SSE/climate_SST
climate_R2
