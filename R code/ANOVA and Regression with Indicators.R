#####################################################
#
#Author: Xinyi (Anny) Cui
#Date: Nov. 18, 2022
#
#
##############################################
#
#Data
rootData<-read.csv(file.choose(), header=TRUE)
library(palmerpenguins)
data(penguins)

head(rootData)
head(penguins)

str(rootData)
str(penguins)
penguins<-subset(penguins,!is.na(body_mass_g))
str(penguins)

#Deal with rootData
grandAvg<-mean(rootData$Diameter)
treatEffects<-tapply(rootData$Diameter,rootData$Trt,mean) - grandAvg
grandAvg
treatEffects
grandAvg + treatEffects

# fit the model
diameterModel = aov(Diameter~Trt, data = rootData)
summary(diameterModel)

dModel = lm(Diameter~Trt, data = rootData)
summary(dModel)


meanModel = lm(Diameter - mean(Diameter) ~ Trt, data = rootData)
summary(meanModel)

noConstantModel = lm(Diameter - mean(Diameter) ~ Trt - 1, data = rootData)
summary(noConstantModel)

anova(dModel)
anova(meanModel)
anova(noConstantModel)


# penguin 2-way ANOVA
meansTable<-tapply(penguins$body_mass_g,
                    list(penguins$sex,penguins$species),
                    mean)
meansTable

grandAvg<-mean(penguins$body_mass_g,na.rm = TRUE)
grandAvg

speciesEffect<-tapply(penguins$body_mass_g, 
                       penguins$species, 
                       mean) - grandAvg
sexEffect<-tapply(penguins$body_mass_g, 
                   penguins$sex, 
                   mean) - grandAvg

speciesEffect
sexEffect

interaction<-(meansTable - rbind(speciesEffect,speciesEffect)
               - cbind(sexEffect,sexEffect,sexEffect) - grandAvg)
interaction

#linear regression
penguinModel = lm(body_mass_g ~ sex+species+sex:species, data = penguins)
summary(penguinModel)
anova(penguinModel)

