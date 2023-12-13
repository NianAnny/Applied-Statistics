################################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Oct. 26, 2022
#
#Title: Multiple Linear Regression for mpg of car models from 1973-74
#Goal: Use the build-in R dataset mtcars to construct a multiple linear 
#regression model for predicting the gas mileage with predictors
#
#
#Data is called by the build-in R dataset mtcars
#Response variable: gas mileage (mpg)
#Other variables:
# cyl: 	Number of cylinders
# disp:	Displacement (cu.in.)
# hp:	  Gross horsepower
# drat:	Rear axle ratio
#	wt:	  Weight (1000 lbs)
#	qsec:	1/4 mile time
#	vs:	  Engine (0 = V-shaped, 1 = straight)
#	am:	  Transmission (0 = automatic, 1 = manual)
#	carb:	Number of carburetors

#Data Description: The data was extracted from the 1974 Motor Trend US magazine
#and comprises fuel consumption and 10 aspects of automobile design and
#performance for 32 automobiles (1973-1974 models)
#
################################################################################
#
#check with the build-in R dataset mtcars (Motor Trend Car Road Tests)
head(mtcars)
str(mtcars)
?mtcars

#Explore the data and check for each variables 
lattice::densityplot(mtcars$mpg) #No skewness

#cylinders
lattice::densityplot(mtcars$cyl)

#displacement
lattice::densityplot(mtcars$disp)

#gross horsepower
lattice::densityplot(mtcars$hp, xlab="Gross Horsepower")#slightly left skewed
mtcars<-transform(mtcars, logHP=log(hp))
lattice::densityplot(mtcars$logHP, xlab="log Gross Horsepower")

#rear axle ratio
lattice::densityplot(mtcars$drat)

#weight
lattice::densityplot(mtcars$wt)

#quarter mile time
lattice::densityplot(mtcars$qsec, xlab="1/4 mile time")#notice the outlier and remove it
mtcars<-subset(mtcars, qsec<22)
lattice::densityplot(mtcars$qsec, xlab="1/4 mile time")

#engine
lattice::densityplot(mtcars$vs)

#transmission
lattice::densityplot(mtcars$am)

#forward gears
lattice::densityplot(mtcars$gear)

#carburetors
lattice::densityplot(mtcars$carb)

#
#See how variables affect the response one
car::scatterplotMatrix(subset(mtcars, select=c("mpg",
                                              "cyl",
                                              "disp",
                                              "logHP",
                                              "drat",
                                              "wt",
                                              "qsec",
                                              "vs",
                                              "am",
                                              "gear",
                                              "carb")))
#Reasonabe relationships between response and potential predictors
#But some potential predictors are highly correlated

########################################################################
#
#Construct the model with all variables for more checking and reducing

#model1 is the completed model with all other variables
model1<-lm(mpg~cyl+disp+logHP+drat+wt+qsec+vs+am+gear+carb, data=mtcars)
summary(model1)
car::vif(model1)
#the variables wt has the highest VIF 19.878458, so we can drop it firstly 

model2<-lm(mpg~cyl+disp+logHP+drat+qsec+vs+am+gear+carb, data=mtcars)
car::vif(model2)
#cyl has the highest VIF 14.394481, so this time drop this variable

model3<-lm(mpg~disp+logHP+drat+qsec+vs+am+gear+carb, data=mtcars)
car::vif(model3)
#logHP has the highest VIF of 12.898424, so drop this variable

model4<-lm(mpg~disp+drat+qsec+vs+am+gear+carb, data=mtcars)
car::vif(model4)
#gear still has the highest VIF 5.651795, so we drop this variable
#
################################################################################
#
#Comparing appropriate models to select the best one

model5<-lm(mpg~disp+drat+qsec+vs+am+carb, data=mtcars)
car::vif(model5)
summary(model5) # R^2 is 0.8384
#qsec still has a little high VIF 4.800542, near to 5, so we can try construct
#a model without this 

#Construct the mpg model
mpgModel<-lm(mpg~disp+drat+vs+am+carb, data=mtcars)
summary(mpgModel) #R^2 is 0.838
car::vif(mpgModel) #disp has a VIF of 4.191773, an acceptable value but higher
#than 4, try to compare to another model without the variable disp

mpgModel2<-lm(mpg~drat+vs+am+carb, data=mtcars)
car::vif(mpgModel2)

#Compare model5 and mpgModel, mpgModel is the reduced version of model5
anova(model5, mpgModel)
#the p-value is 0.8156 which is not statistically significant to show that the 
#full model (model5) is necessarily better. This means we'd better to use the
#reduced version (mpgModel)


#Check fot the inference
summary(mpgModel) #R^2 is 0.838
summary(mpgModel2)#R^2 is 0.8029

#directly compare mpgModel and mpgModel2 (mpgModel2 is the reduced version of
#mpgModel)
anova(mpgModel, mpgModel2)
#the p-value is 0.02838, this is much smaller than 0.05, which means it is
#statistically significant that the full model is better than the reduced one
#Better to select mpgModel 

#Assess the mpgModel
plot(mpgModel$fitted.values, mpgModel$residuals,xlab="Fitted Values",
     ylab="Residuals")
abline(h=0)
lattice::densityplot(mpgModel$residuals, xlab="Residuals")
qqnorm(mpgModel$residuals)
qqline(mpgModel$residuals)
#
#
#confidence interval and prediction interval
#randomly selecting values of predictors for predicting intervals
#lm(mpg~disp+drat+vs+am+carb)
newData<-data.frame(disp=200,
                    drat=3.9,
                    vs=1,
                    am=0,
                    carb=4)
newData

#95% confidence and prediction intervals
predict(mpgModel, newData, int="confidence")
predict(mpgModel, newData, int="prediction")

