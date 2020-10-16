#Xtern Work Assessment for Data Science
library(ggplot2)
library(dplyr)
library(tidyverse)

FoodieX = read.csv("2020-XTern-DS.csv")

BaseRestaurants = FoodieX[which(FoodieX$Rating!="NEW" & FoodieX$Rating!="-" & FoodieX$Reviews>=1),]

lmRating = as.numeric(BaseRestaurants$Rating)
lmTime = as.numeric(gsub(" minutes", "", BaseRestaurants$Cook_Time))
summary(lmRating)
summary(lmTime)

cor.test(lmRating, y=lmTime)

linearModel = lm(lmTime ~ lmRating, data=BaseRestaurants)
summary(linearModel)

ratingTimePlot = ggplot(BaseRestaurants, aes(x=lmRating, y=lmTime)) + geom_point() + geom_smooth(method="lm", se=TRUE, level=0.99)
ratingTimePlot

signlessCost = gsub("[$ ]", "", BaseRestaurants$Average_Cost)
lmCost = as.numeric(signlessCost)
summary(lmCost)

cor.test(lmRating, y=lmCost, na.rm = TRUE)

linearModel = lm(lmCost ~ lmRating, data=BaseRestaurants)
summary(linearModel)

ratingCostPlot = ggplot(BaseRestaurants, aes(x=lmRating, y=lmCost)) + geom_point() + geom_smooth(method="lm", se=TRUE, level=0.99)
ratingCostPlot

lmLong = as.numeric(BaseRestaurants$Longitude)
lmLat = as.numeric(BaseRestaurants$Latitude)

summary(lmLong)
summary(lmLat)

cor.test(lmRating, y=lmLong)
cor.test(lmRating, y=lmLat)

linearModelLong = lm(lmLong ~ lmRating, data=BaseRestaurants)
linearModelLat = lm(lmLat ~ lmRating, data=BaseRestaurants)
summary(linearModelLong)
summary(linearModelLat)

ratingLongPlot = ggplot(BaseRestaurants, aes(x=lmRating, y=lmLong)) + geom_point() + geom_smooth(method="lm", se=TRUE, level=0.99)
ratingLongPlot
ratingLatPlot = ggplot(BaseRestaurants, aes(x=lmRating, y=lmLat)) + geom_point() + geom_smooth(method="lm", se=TRUE, level=0.99)
ratingLatPlot
ratingMapPlot = ggplot(BaseRestaurants, aes(x=lmLat, y=lmLong)) + geom_point() + geom_smooth(method="lm", se=TRUE, level=0.99)
ratingMapPlot

lmVote = as.numeric(BaseRestaurants$Votes)
summary(lmVote)

cor.test(lmRating, y=lmVote, na.rm = TRUE)

linearModel = lm(lmVote ~ lmRating, data=BaseRestaurants)
summary(linearModel)

ratingVotesPlot = ggplot(BaseRestaurants, aes(x=lmRating, y=lmVote)) + geom_point() + geom_smooth(method="lm", se=TRUE, level=0.99)
ratingVotesPlot