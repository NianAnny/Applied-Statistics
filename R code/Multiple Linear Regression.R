################################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Sept. 29, 2022
#
#Title: Warm-Up: Multiple Linear Regression
#Data from the csv file "snowPetrel" collected by Prof. Hodum
#
################################################################################
#
#Import the data file
petrelData<-read.csv(file.choose(), header=TRUE)
head(petrelData)
str(petrelData)
#
#We can try to construct a model with the original data to see the difference
original<-lm(mass~day+tarsus+wing+culmen, data=petrelData)
summary(original)
#
#See the relationship between mass and other variable individually
plot(petrelData$day, petrelData$mass)
plot(petrelData$tarsus, petrelData$mass)
plot(petrelData$wing, petrelData$mass)
plot(petrelData$culmen, petrelData$mass)
#
#We need to remove the outlier in tarsus data and use the new data frame to
#create a model equation with keeping the data lengths are equal
newData<-subset(petrelData, tarsus<300)
head(newData)
str(newData)
plot(newData$day, newData$mass)
plot(newData$tarsus, newData$mass)
plot(newData$wing, newData$mass)
plot(newData$culmen, newData$mass)
#
lattice::densityplot(newData$mass)
lattice::densityplot(newData$day)
lattice::densityplot(newData$tarsus)
lattice::densityplot(newData$wing)
lattice::densityplot(newData$culmen)
#No clear skewness so do not do any transformation of the data
#
#Construct a linear model
massModel<-lm(mass~day+tarsus+wing+culmen, data=newData)
summary(massModel)
anova(massModel)
#
#Create plots and check the conditions of inference
plot(massModel$fitted.values, massModel$residuals)
hist(massModel$residuals)
lattice::densityplot(massModel$residuals)
qqnorm(massModel$residuals)
#The model displays the linearity 





