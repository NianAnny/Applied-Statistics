################################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Oct. 4, 2022
#
#Title: Car Prices
# 
#Data comes from Age and Price of Mazda Cars
#from the website "http://www.statsci.org/data/oz/mazdas.html"
#
#The two columns of the data are the prices and year purchased for 124 Mazda 
#cars, as taken from the classified section of the Melbourne Age during the 
#course of 1991. Hence the age of the car at the time can be calculated and 
#used to model car price.
#
################################################################################
#
#Import the dataset and add a new column called Age (91-Year)
carData<-read.table("http://www.statsci.org/data/oz/mazdas.txt", header=TRUE)
carData$Age<-91-carData$Year
head(carData)
str(carData)
#
#Check to see if any variable has the skewness
plot(carData$Age, carData$Price, xlab="Age in years", ylab="Price in dollars",
     main="Price v.s. Age")

lattice::densityplot(carData$Year)
lattice::densityplot(carData$Price)
lattice::densityplot(carData$Age)
#The data of Price is clearly left skewed, so we need to do log transformation
#
#Log transform the data of Price and check it again
carData<-transform(carData, logPrice=log(carData$Price))
#
#Plot the logPrice changes corresponding to the age of the car
plot(carData$logPrice, carData$Age, xlab="Age in years", ylab="logPrice",
     main="logPrice v.s. Age")
#
#Make a linear regression model for logPrice and check for its inference
carModel<-lm(logPrice~Age, data=carData)
carModel
summary(carModel)
abline(carModel)
anova(carModel)

#Fit for "true" linear equation of Price~Age
carModel$coefficients
exp(10.1877511)
exp(0.1646909)

carModel2<-26575.66 - 1.179029*carData$Age
plot(carData$Age, carModel2, xlab="Age", ylab="Estimated Price", 
     main="True Model")

mean(carData$Price)
mean(carModel2)

median(carData$Price)
median(carModel2)

#Check for the value of R
cor(carData$logPrice, carData$Age)
cor(carData$Price, carData$Age)
cor(carModel2, carData$Age)
#
#
#Check for the linearity of the model
hist(carModel$residuals)
plot(carModel$fitted.values, carModel$residuals)
qqnorm(carModel$residuals)
qqline(carModel$residuals)
lattice::densityplot(carModel$residuals)
#
#Use the model to estimate a 95% confidence interval for the coefficient of Age
confint(carModel)
exp(0.1797199)
exp(0.1496619)
#
#Estimate a 95% confidence interval and prediction
year85=data.frame(Age=91-85)
con85<-predict(carModel, year85, int="confidence")
pre85<-predict(carModel, year85, int="prediction")
exp(con85)
exp(pre85)
