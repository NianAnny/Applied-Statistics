################################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Sept. 28, 2022
#
#Title: Simple Linear Regression Lab Report
#Data comes form Cancer Mortality near Hanford Reactor
#http://www.statsci.org/data/general/hanford.html
#
################################################################################
#
#Import the dataset and see the information
exposureData<-read.table("http://www.statsci.org/data/general/hanford.txt",
                         header=TRUE)
head(exposureData)
str(exposureData)
#
#Take a look at the data scatterplot
exposurePlot<-plot(exposureData$Exposure, exposureData$Mortality, xlab="Exposure",
     ylab="Mortality", main="Graph of Exposure v.s. Mortality")
lattice::densityplot(exposureData$Exposure)
lattice::densityplot(exposureData$Mortality)
hist(exposureData$Exposure)
hist(exposureData$Mortality)
#None of the variables seems have a skewness so we don't have to do a transform.
#
#Construct a linear model
exposureModel<-lm(Mortality~Exposure, data=exposureData)
abline(exposureModel)
summary(exposureModel)
anova(exposureModel)
#
#Construct a confidence interval for the slope of the regression line
confint(exposureModel, 2)
#
#Construct mean response confidence intervals and prediction intervals for 
#"low," "medium," and "high" exposure communities (you can decide what these 
#three things are)
#
#Assign counties to low, medium, and high communities
lowCom<-subset(exposureData, Exposure< 4)
mediumCom<-subset(exposureData, (Exposure> 4)&(Exposure< 8))
highCom<-subset(exposureData, Exposure>8)

rbind(lowCom, mediumCom, highCom)
#
#Do confidence and prediction intervals for each community mean resposne
lowCI<-predict(exposureModel, lowCom, int="confidence")
lowPI<-predict(exposureModel, lowCom, int="prediction")

mediumCI<-predict(exposureModel, mediumCom, int="confidence")
mediumPI<-predict(exposureModel, mediumCom, int="prediction")

highCI<-predict(exposureModel, highCom, int="confidence")
highPI<-predict(exposureModel, highCom, int="prediction")

rbind(lowCI, lowPI, mediumCI, mediumPI, highCI, highPI)
#
#Calculate the mean value of mortality in each community
rbind(mean(lowCI), mean(mediumCI), mean(highCI))
#
#We can calculate the average value for each community's exposure at first
lowE<-data.frame(Exposure=mean(lowCom$Exposure))
mediumE<-data.frame(Exposure=mean(mediumCom$Exposure))
highE<-data.frame(Exposure=mean(highCom$Exposure))

meanLowCI<-predict(exposureModel, lowE, int="confidence")
meanLowPI<-predict(exposureModel, lowE, int="prediction")

meanMediumCI<-predict(exposureModel, mediumE, int="confidence")
meanMediumPI<-predict(exposureModel, mediumE, int="prediction")

meanHighCI<-predict(exposureModel, highE, int="confidence")
meanHighPI<-predict(exposureModel, highE, int="prediction")

rbind(meanLowCI, meanLowPI, meanMediumCI, meanMediumPI, meanHighCI, meanHighPI)
#Both methods get the same fitted value of mean morality but we prefer to use 
#the second one as it also gives the lower and higher values, which shows the 
#intervals.
