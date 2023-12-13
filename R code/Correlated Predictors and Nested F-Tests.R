################################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Oct. 10, 2022
#
#Goal: Construct a model to predict an animal's lifespan with some given 
#variables
#
#Data comes from an observation of 62 different species of mammals
#
################################################################################
#
#Import the data
mammalData<-read.table("http://www.statsci.org/data/general/sleep.txt",
                       header=TRUE)
head(mammalData)
str(mammalData)
#
#Check for outliers
plot(LifeSpan~BodyWt, data=mammalData)
plot(LifeSpan~NonDreaming, data=mammalData)
plot(LifeSpan~TotalSleep, data=mammalData)
plot(LifeSpan~Gestation, data=mammalData)
#In the graph of Body Weight v.s. Lifespan, we can see three outliers and we
#need to remove these points
mammalData2<-subset(mammalData, (BodyWt<2000)&(LifeSpan<80))
head(mammalData2)
str(mammalData2)
#
#
#Check skewness of each variable
lattice::densityplot(mammalData2$LifeSpan)
lattice::densityplot(mammalData2$BodyWt)
lattice::densityplot(mammalData2$NonDreaming)
lattice::densityplot(mammalData2$TotalSleep)
lattice::densityplot(mammalData2$Gestation)
#LifeSpan, Body weight, and gestation are clearly left skewed and need transform
#
mammalData3<-transform(mammalData2, logLifeSpan=log(LifeSpan), 
                       logBodyWt=log(BodyWt), logGestation=log(Gestation))
head(mammalData3)
str(mammalData3)

lattice::densityplot(mammalData3$logLifeSpan)
lattice::densityplot(mammalData3$logBodyWt)
lattice::densityplot(mammalData3$NonDreaming)
lattice::densityplot(mammalData3$TotalSleep)
lattice::densityplot(mammalData3$logGestation)
#
#Construct a model for lifespan 
mammalModel<-lm(logLifeSpan~logBodyWt+NonDreaming+TotalSleep+logGestation,
                data=mammalData3)

plot(mammalModel$fitted.values, mammalModel$residuals)
lattice::densityplot(mammalModel$residuals)
qqnorm(mammalModel$residuals)
qqline(mammalModel$residuals)

summary(mammalModel)
c(exp(1.46458),exp(0.25849),exp(0.38191),exp(0.26664),exp(0.05904))
anova(mammalModel)

#Find the inflation factor
1/(1-(1-15.2560/(15.2560+0.9617+4.6116+0.0519+13.5579)))
1/(1-(1-0.9617/(15.2560+0.9617+4.6116+0.0519+13.5579)))
1/(1-(1-4.6116/(15.2560+0.9617+4.6116+0.0519+13.5579)))
1/(1-(1-0.0519/(15.2560+0.9617+4.6116+0.0519+13.5579)))

mammalModel2<-lm(logLifeSpan~logBodyWt+NonDreaming+TotalSleep,
                 data=mammalData3)
summary(mammalModel2)
anova(mammalModel2)
