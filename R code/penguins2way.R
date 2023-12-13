# Jake Price
# November 9, 2022
# Two-Way ANOVA with Interactions - Palmer's Penguins
#
# Question: Do the different penguin sexes
#           have different average masses? Is
#           that difference the same for each
#           species?
#
# Data: Palmer's Penguin dataset
#          palmerpenguins package
#
# Response: body_mass_g
# Predictors: Sex (male / female)
#             Species (Adelie, Gentoo, Chinstrap)
#
# Model: Mass ~ Sex + Species + (Sex:Species)

# load the data
# install.package("palmerpenguins")
library(palmerpenguins)
data(penguins)
head(penguins)
penguins$species = factor(penguins$species,labels = c("A","C","G"))
penguins = subset(penguins, !is.na(body_mass_g) & !is.na(species)
                   & !is.na(sex))

# data exploration
boxplot(body_mass_g ~ sex + species, data = penguins)
boxplot(body_mass_g ~ species + sex, data = penguins)

meansTable = tapply(penguins$body_mass_g,
                    list(penguins$sex,penguins$species),
                    mean)

plot(meansTable[1,],meansTable[2,])
heuristicModel = lm(meansTable[2,]~meansTable[1,])
abline(heuristicModel)
heuristicModel

# fit the data with an interaction term
grandAvg = mean(penguins$body_mass_g,na.rm = TRUE)
grandAvg

speciesEffect = tapply(penguins$body_mass_g, 
                       penguins$species, 
                       mean) - grandAvg
speciesEffect

sexEffect = tapply(penguins$body_mass_g, 
                       penguins$sex, 
                       mean) - grandAvg
sexEffect

interaction = (meansTable - rbind(speciesEffect,speciesEffect)
                - cbind(sexEffect,sexEffect,sexEffect) - grandAvg)
interaction

model = aov(body_mass_g ~ species + sex + species:sex, data = penguins)
# note, converted mass to kg
summary(model)
# all terms, including interactions, are significant

meansTable = tapply(penguins$body_mass_g,
                    list(penguins$sex,penguins$species),
                    mean)

meansTable

SD = sqrt(deviance(model)/df.residual(model))
SD

(meansTable-grandAvg)/SD

TukeyHSD(model)

# assess the model
boxplot(model$residuals ~ penguins$species + penguins$sex)
qqnorm(model$residuals)
lattice::densityplot(model$residuals)
# wow, basically perfectly normal residuals
# exactly equal variance across groups