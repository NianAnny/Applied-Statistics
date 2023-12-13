###################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Nov. 8, 2022
#
#Goal: Investigate how the mass of a peguin depends on their species and sex
#
#Data Description: collected and made available by Dr. Kristen Gorman and Palmer
#               Station, Antarctica LTER, a member of the Long Term Ecological
#               Research Network.
#               
#Response: mass (body_mass_g) in grams
#Explanatory: species (3 levels: Adélie, Chinstrap and Gentoo) 
#         and sex (2 levels: female and male)
#
####################################################################
#
#Install the released version of palmerpenguins from CRAN 
install.packages("palmerpenguins")

library(palmerpenguins)
data(package='palmerpenguins')
#
#call the dataset
head(penguins)
head(penguins_raw)

?penguins
?penguins_raw

str(penguins)
str(penguins_raw)

#Use the first dataset penguins, a subset of penguins_raw

#observations for each combination of predictors
nrow(subset(penguins, !is.na(species)))
nrow(subset(penguins, !is.na(sex)))

#means for checking the plot values
speciesMass<-tapply(penguins$body_mass_g, penguins$species, mean, na.rm=TRUE)
speciesMass
sexMass<-tapply(penguins$body_mass_g, penguins$sex, mean, na.rm=TRUE)
sexMass

#plots
interaction.plot(x.factor=penguins$sex,
                 trace.factor = penguins$species,
                 response = penguins$body_mass_g,
                 fun=mean,
                 ylab = "Mass (g)", xlab = "Sex",
                 trace.label = "Species",
                 col=c("blue","green","red")) #mass means based on species v.s. sex

interaction.plot(x.factor=penguins$species,
                 trace.factor = penguins$sex,
                 response = penguins$body_mass_g,
                 fun=mean,
                 ylab = "Mass (g)", xlab = "Species",
                 trace.label = "Sex",
                 col=c("blue","red")) #mass means based on sex v.s. species
#not roughly parallel, proof of interaction
#
#
#grand average and group effects
grandAvg<-mean(penguins$body_mass_g, na.rm=TRUE)
speciesEffect<-speciesMass-grandAvg
sexEffect<-sexMass-grandAvg

grandAvg
speciesEffect
sexEffect

#model with interaction
massAOV<-aov(body_mass_g~species+sex+species:sex, data=penguins)
summary(massAOV)

#condition of inference
plot(massAOV$fitted.values,massAOV$residuals)
qqnorm(massAOV$residuals)
lattice::densityplot(massAOV$residuals)
