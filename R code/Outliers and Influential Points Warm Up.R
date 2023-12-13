################################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Sept. 15, 2022
#
#Title: Warm-Up: Outliers and Influential Points
#
#Data collected by Peter Hodum (Department of Biology) in 1995-1996 on HopIsland
#
################################################################################
#
#Import data from a csv file downloaded on PC and quickly check them
snowPetrel<-read.csv(file.choose(),header=TRUE)
head(snowPetrel)
str(snowPetrel)
attach(snowPetrel)
#
#Graph of wing length against leg length
lengthPlot<-plot(tarsus, wing, data=snowPetrel, xlab="tarsus (mm)",
             ylab="wing (mm)", main="Graph of Wing Length against Leg Length")
#
#Remove the influential point and create a new plot
petrel<-subset(snowPetrel,tarsus<300)
head(petrel)
str(petrel)
remove(snowPetrel)
attach(petrel)
plot2<-plot(tarsus, wing, data=petrel, xlab="tarsus (mm)",
                 ylab="wing (mm)", main="New Graph")
#
#Check the distribution of the new graph
hist(wing)
lattice::densityplot(wing)
#
#Transform the data and construct a model
newPetrel<-transform(petrel,logWing=log(wing))
plot(newPetrel$tarsus,newPetrel$logWing)
wingModel<-lm(logWing~tarsus,data=newPetrel)
summary(wingModel)
abline(wingModel)
#
#Check for the model
plot(wingModel$fitted.values,wingModel$residuals)
hist(wingModel$residuals)
qqnorm(wingModel$residuals)

exp(wingModel$coefficients)
