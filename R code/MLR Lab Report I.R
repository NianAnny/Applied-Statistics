################################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Oct. 10, 2022
#
#Goal: Prepare for Multip linear Regression lab and analyze the basic ideas
#
#Data come from the American Football Punters of an investigation studied 
#physical characteristics and ability in 13 football punters.
#
################################################################################
#
#Import the dataset
footballData<-read.table("http://www.statsci.org/data/general/punting.txt",
                         header=TRUE)
head(footballData)
str(footballData)
names(footballData)
#
#Create density plots for each variable
lattice::densityplot(footballData$Distance)
lattice::densityplot(footballData$Hang)
lattice::densityplot(footballData$R_Strength)
lattice::densityplot(footballData$L_Strength)
lattice::densityplot(footballData$R_Flexibility)
lattice::densityplot(footballData$L_Flexibility)
lattice::densityplot(footballData$O_Strength)
#From the density plots of each variable, there exists no plots displaying a 
#clear shewness so we do not have to do the transformation of the data.
#
#
#Intuitively, We can firstly use the hang time and overall leg strength as the 
#explanatory variables and check to see how the model fits the values
punterModel<-lm(Distance~Hang+O_Strength, data=footballData)
summary(punterModel)
#The p-value is 0.001103, it seems good to show there exists a trend between
#the distance and the hang time and the overall leg strength. The R^2 value is
#0.7438 and we can explain 74.38% of the variation in distance. This is not bad,
#but we can try to do more check on this model

#
#Check the linearity of the model
plot(punterModel$fitted.values, punterModel$residuals)
lattice::densityplot(punterModel$residuals)
qqnorm(punterModel$residuals)
qqline(punterModel$residuals)
#
#Construct a 95% prediction interval
footballData$Distance
footballData$Hang
footballData$O_Strength

avgKickerHang<-mean(footballData$Hang)
avgKickerOverallLeg<-mean(footballData$O_Strength)
c(avgKickerHang, avgKickerOverallLeg)

starKickerHang<-5
starKickerOverallLeg<-300

avgKicker<-data.frame(Hang=avgKickerHang, O_Strength=avgKickerOverallLeg)
starKicker<-data.frame(Hang=starKickerHang, O_Strength=starKickerOverallLeg)

averageKicker<-predict(punterModel, avgKicker, int="prediction")
star<-predict(punterModel, starKicker, int="prediction")

averageKicker
star

