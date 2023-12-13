###############################################################
#
#Author: 
#Date: Oct. 9, 2022
#
#Title: Two-Way ANOVA Lab
#Question: Whether mother lizards deposit an antifungal microbiome onto 
#         eggshells during oviposition (egg-laying), as eggs pass through the 
#         cloaca, and how this might affect hatchling fitness.
#
#Goal: Test the effect of both the environment and the treatment on the mass 
#       of the hatchling. 
#
#Data description: Data comes from a reseach study conducted by Prof. Stacey 
#                   Weiss.
#
#Response: mass (HatchBmass) in grams
#Blocking: environment (inc.envmt), sterile or fungal-inoculated
#Explanatory: treatment (egg.trmt), 2 levels (laid or dissected)
#
################################################################
#
#Read the dataset
eggs<-read.csv(file.choose(),header=TRUE)
head(eggs)
str(eggs)

#rename the variables
dissected<-subset(eggs, egg.trmt=="Dissected")
induced<-subset(eggs, egg.trmt=="Induced")
#
#visualizing and judging the additive effects
boxplot(HatchBmass~inc.envmt+egg.trmt, data=eggs, xlab="envmt:treat",ylab="mass")
# treatment side-by-side make the additive effects model reasonable
#
boxplot(HatchBmass~egg.trmt+inc.envmt,data=eggs,xlab="treat:envmt",ylab="mass")


#fit the model
grandAvg<-mean(eggs$HatchBmass)
envmtEffect<-tapply(eggs$HatchBmass, eggs$inc.envmt, mean)-grandAvg
treatEffect<-tapply(eggs$HatchBmass, eggs$egg.trmt, mean)-grandAvg

grandAvg
envmtEffect
treatEffect

massAOV<-aov(HatchBmass~inc.envmt+egg.trmt,data=eggs)
summary(massAOV)

#assess the conditions for inference
plot(massAOV$fitted.values,massAOV$residuals)
boxplot(massAOV$residuals~eggs$egg.trmt + eggs$inc.envmt)
boxplot(massAOV$residuals~eggs$inc.envmt + eggs$egg.trmt)
qqnorm(massAOV$residuals)
qqline(massAOV$residuals)
lattice::densityplot(massAOV$residuals)

#95% confidence intervals
TukeyHSD(massAOV)

#effect size for the model
SD = sqrt(deviance(massAOV)/df.residual(massAOV))
blockEffectSize<-envmtEffect/SD
explanatoryEffectSize<-treatEffect/SD

blockEffectSize
explanatoryEffectSize

