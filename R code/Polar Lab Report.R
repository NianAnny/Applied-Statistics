################################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Sept. 19, 2022
#
#Title: Polar Lab Report
#
#Datasets come from an ice core and a trek across the antarctic.
#
################################################################################
#
#imoport the ice core data and check for it
iceCore<-read.csv(file.choose(),header=TRUE)
head(iceCore)
str(iceCore)
attach(iceCore)
#
#import the trek data and check for it
trek<-read.csv(file.choose(),header=TRUE)
head(trek)
str(trek)
attach(trek)
#
#Plot the temperature over delta18O based on the trek data to see the linearity
trekPlot<-plot(trek$delta18O,trek$temperature_corrected,
              xlab="delta18O Depletion", ylab="Temperature",
              main="Graph of delta18O v.s. Temperature (Trek)")
#The graph seems have a good linearity of an increasing trendline, even if there
#is a little bit bent between the range of -40 to -30, it will not influence the 
#overall linear function too much. Using the column of temperature_corrected to
#exclude the effect of the one outlier.
#
#Construct a model for temperature with delta18O as the explanatory variable 
trekModel<-lm(temperature_corrected~delta18O, data=trek)
trekModel
summary(trekModel)
abline(trekModel)
#Then, we get a function of the model which is 
#Temp = 1.17530 delta18O + 10.44790
#
#Check for the model
plot(trekModel$fitted.values,trekModel$residuals)
hist(trekModel$residuals)
qqnorm(trekModel$residuals)
qqline(trekModel$residuals)
#
#
#Apply the linear model to get the temperature based on given d18O in ice core.
#Create a new data frame including delta18O, temperature, and age values 
#for ice core
newIce<-data.frame(delta18O=c(iceCore$d18O),
                   Age_decimal=c(iceCore$Age_decimal))
iceTemp<-predict(object=trekModel, 
                 newdata = newIce)
newIce<-transform(newIce, iceTemp=iceTemp)
head(newIce)
str(newIce)
#
#Plot the temperature changing over time for the new data frame newIce
icePlot<-plot(newIce$Age_decimal, newIce$iceTemp,
              xlab="Age (years)", ylab="Temperature", 
              main="Graph of Temperature over Time")
#
################################################################################
#Second way to apply the linear model for ice core data 
#Using a normal method for checking the previous one
#Predict the temperature in ice core dataset with the linear regression model
#Create a data frame of d18O from ice core
#
#iceDelta18O<-as.data.frame(iceCore$d18O)
#newIceCore<-transform(iceDelta18O, 
#                      temp=10.448+1.175*iceCore$d18O, 
#                      age=iceCore$Age_decimal)
#head(newIceCore)
#str(newIceCore)
#View(newIceCore)
#plot(newIceCore$iceCore.d18O, newIceCore$temp)
#plot(newIceCore$age, newIceCore$temp)
################################################################################

