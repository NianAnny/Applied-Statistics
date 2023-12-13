######################################
#
#Author: Xinyi (Anny) Cui
#Date: Oct. 26, 2022
#
#Goal: See whether different sectors of the economy have different average 
#sale figures
#
#Data comes from the top 2000 companies as listed by Forbes.com in 2017
#
#Response variable: Sales
#Explanatory variable: Sector
#
#############################
#Import the dataset
companies<-read.csv(file.choose(),header=TRUE)
head(companies)
str(companies)

#Explore the data
boxplot(Sales~Sector, data=companies)
lattice::densityplot(~Sales, group=Sector, data=companies, auto.key=TRUE)

#Assess whether reexpress the data
means<-tapply(companies$Sales, companies$Sector, mean)
SDs<-tapply(companies$Sales, companies$Sector, sd)
cbind(means, SDs)

medians<-tapply(companies$Sales, companies$Sector, median)
n<-tapply(companies$Sales,companies$Sector,length)
cbind(SDs, means, medians, n)

plot(log(means), log(SDs))
checkModel<-lm(log(SDs)~log(means))
abline(checkModel)
summary(checkModel)
#slope is 1.0143 so we need to do log transformation

#Create box plots and do comparisons
boxplot(log(Sales)~Sector, data=companies)
lattice::xyplot(log(Sales)~factor(Sector), data=companies)
lattice::densityplot(~log(Sales), group=Sector, data=companies, auto.key=TRUE)

#Construct the ANOVA model
salesAOV<-aov(log(Sales)~Sector, data=companies)
summary(salesAOV)

#conditions of inference
#Assess the ANOVA model
lattice::densityplot(salesAOV$residuals, group=companies$Sector,auto.key=TRUE)
plot(salesAOV$fitted.values, salesAOV$residuals)
qqnorm(salesAOV$residuals)
lattice::xyplot(salesAOV$residuals~factor(companies$Sector))
boxplot(salesAOV$residuals~companies$Sector)

#Convert coefficients back
salesAOV$coefficients
exp(salesAOV$coefficients)

#Tukey HSD analysis
TukeyHSD(aov(Sales~Sector, data=companies))

