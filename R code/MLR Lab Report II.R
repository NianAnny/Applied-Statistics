################################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Oct. 20, 2022
#
#Goal: Find a variable to predict the sale price of a house in King County.
#
#Dataset is from the collection of the house sold between May 2014 and May 2015
#in King County, Washington.
#
################################################################################
#
#Import the dataset
houseData<-read.csv(file.choose(), header=TRUE)
head(houseData)
str(houseData)
#
#Plots each variable
lattice::densityplot(houseData$price)

lattice::densityplot(houseData$sqft_living)
lattice::densityplot(houseData$condition)
lattice::densityplot(houseData$grade)
#It seems all the variables display a kind of skewness, which means they need
#transformation. I want to do log transform to the data but try to avoid some
#zero values because log(0) provides no meaning. Also, I do not want to remove
#any variables if I can. Therefore, I decide to use squared footage of the 
#apartments interior living space, which represents the space the buyers can
#really use for living. The condition of comments to see how physical goodness
#of the house. The grade to see how good the building construction and design 
#of the house is.
#
#We need do transformation on price and squared footage of living space
houseData<-transform(houseData, 
                     logPrice=log(price), 
                     logSqft_living=log(sqft_living))
lattice::densityplot(houseData$logPrice)
lattice::densityplot(houseData$logSqft_living)
#
#Construct a multiple linear regression model
houseModel<-lm(logPrice~logSqft_living+condition+grade, data=houseData)
summary(houseModel)
#The dataset has 21613 observation so that the coefficient of determination 
#(R^2) will be hard to be close to 1, explaining all the variation in price.
#56.17% is not bad.
exp(houseModel$coefficients)
#
#Check the linearity
plot(houseModel$fitted.values, houseModel$residuals)
abline(h=0)
qqnorm(houseModel$residuals)
qqline(houseModel$residuals)
lattice::densityplot(houseModel$residuals)
#
#find the variance inflation factor
car::vif(houseModel)
#The values are small, not too high to remove (all values are smaller than 5)
#
#prediction interval
house<-data.frame(logSqft_living=log(2000), condition=3, grade=7)
housePrice<-predict(houseModel, house, int="prediction")
housePrice
exp(housePrice)
