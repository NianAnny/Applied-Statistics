################################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Sept. 124, 2022
#
#Title: Correlation and Prediction
#
#Data comes from the build-in R data frame trees
#
################################################################################
#
#Call for build-in R data frame trees and take a look of it
treesData<-trees
head(treesData)
str(treesData)
#
#Check for the skewness
hist(treesData$Height)
hist(treesData$Girth)
#
#Plot Girth (response variable) v.s. Height (explanatory variables)
#and Construct a linear model for it
heightPlot<-plot(treesData$Girth, treesData$Height, ylab="Height (ft)",
                xlab="Girth at 4'6'' above the ground(inches)",
                main="Graph of Height v.s. Girth of Felled Black Cherry Trees")
heightModel<-lm(Height~Girth, data=treesData)
heightModel
summary(heightModel)
abline(heightModel)
#
#Check for linear model
hist(heightModel$residuals)
plot(heightModel$fitted.values, heightModel$residuals)
qqnorm(heightModel$residuals)
qqline(heightModel$residuals)
#
#Analysis of Variance
anova(heightModel)
#
#Check for the correlation
rSquared<- 1-(889.56/(889.56+328.44))
rSquared

r<-sqrt(rSquared)
r

r2<-cor(treesData$Height, treesData$Girth)
cor.test(treesData$Height, treesData$Girth)

r2^2
#
#Check for confidence intervals and prediction intervals with default confi
#level 0.95
newData10<-data.frame(Girth=10)
newData15<-data.frame(Girth=15)
newData25<-data.frame(Girth=25)

ci10<-predict(heightModel, newData10, int="confidence")
pi10<-predict(heightModel, newData10, int="prediction")

ci15<-predict(heightModel, newData15, int="confidence")
pi15<-predict(heightModel, newData15, int="prediction")

ci25<-predict(heightModel, newData25, int="confidence")
pi25<-predict(heightModel, newData25, int="prediction")

table<-rbind(ci10, pi10, ci15, pi15, ci25, pi25)
table

#Check for the width
table<-as.data.frame(table)
str(tableData)
table$Width<-(table$upr-table$lwr)
table
