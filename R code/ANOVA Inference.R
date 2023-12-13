################################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Oct. 17, 2022
#
#Goal: Using the one-way ANOVA to investigate whether the type of car affects
#the highway MPG.
#
#Dataset comes randomly from 1993 passenger car models that were listed in Consumer
#Reports and the Pace Buying Guide.
# Response: Highway MPG (HMPG)
# Explanatory: Manual (0 = automatic, 1 = manual)
#
################################################################################
#
#Import the data file
cars<-read.csv(file.choose(),header=TRUE)
head(cars)
str(cars)
#
#Construct the boxplot showing highway MPG for each group
boxplot(HMPG~Manual, data=cars, horizontal=TRUE)
lattice::densityplot(~HMPG, groups=Manual, data=cars, auto.key=TRUE)
#The boxplot graphically compares the HMPG between cars based on the manuals
#and the plots and group means make clear that there are substantial differences
#among the two groups. The highway MPG is generally higher in group of manual=1.
#
#Find the grand average and the group effect
avg<-mean(cars$HMPG) # grand average
eff<-tapply(cars$HMPG,cars$Manual,mean)-avg #group effect
avg
eff
#
#Construct an ANOVA model
carsANOVA<-aov(HMPG~Manual,data=cars)
summary(carsANOVA)

qqnorm(carsANOVA$residuals)
qqline(carsANOVA$residuals)
plot(carsANOVA$fitted.values,carsANOVA$residuals)
abline(h=0)
