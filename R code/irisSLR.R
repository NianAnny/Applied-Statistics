####################################################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Sept. 13, 2022
#
#Title: Simple Linear Regression
#
#Data: Build-in R iris data
#
####################################################################################################
#
#Load the iris data and reduce it to the Virginica species
irisData<-iris
virginicaData<-droplevels(subset(irisData, Species=="virginica"))
head(virginicaData)
str(virginicaData)
#
#Create a graph of sepal lengths against petal length for virginica
#
plot(virginicaData$Petal.Length,virginicaData$Sepal.Length,
     xlab="Length of Petal (ft)",ylab="Length of Sepal (ft)")
#
#Construct a linear model 
#
irisModel<-lm(Sepal.Length~Petal.Length, data=virginicaData)
summary(irisModel)
#
#Create a histogram of the residuals
#
hist(irisModel$residuals, xlab="Residuals",main="Histogram of the Residuals")
#
#Plot residuals against the fitted values
#
plot(irisModel$fitted.values,irisModel$residuals, 
     xlab="Fitted Values", ylab="Residuals",main="Plot of Residuals against Fitted Values")
#
#Construct a normal quantile-quantile plot
#
qqnorm(irisModel$residuals,main="Normal Quantile Plot of Residuals")
#
#execute a t-test
#
t.test(virginicaData$Petal.Length,virginicaData$Sepal.Length)

