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
#wing skewed left
lattice::densityplot(newData$culmen)
#culmen skewed right
#
# plot(log(newData$culmen), newData$mass) # does not help
#
plot(log(newData$wing), newData$mass) # no curve now
#
newData<-transform(newData, logWing=log(wing))
head(newData)
#
#Construct a linear model
massModel<-lm(mass~day+tarsus+logWing+culmen, data=newData)
#
#Create plots and check the conditions of inference
plot(massModel$fitted.values, massModel$residuals)
hist(massModel$residuals)
lattice::densityplot(massModel$residuals)
qqnorm(massModel$residuals)
#The model displays the linearity 
#
#Inference
summary(massModel)
anova(massModel)
#F-statistics: better than constant model
#p-value suggest rejecting null hypothesis

newBird<-data.frame(day=3, logWing=2, tarsus=23, culmen=12)
predict(massModel, newdata = newBird, int="confidence")
predict(massModel, newdata = newBird, int="prediction")

