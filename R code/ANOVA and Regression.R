# Jake Price
# 11/18/22
# ANOVA and Regression
#
# Exploring the relationship between ANOVA and linear regression

# One-way ANOVA
# Model petal length ~ species
# ANOVA approach
grandAvg = mean(iris$Petal.Length)
speciesEffects = tapply(iris$Petal.Length,iris$Species,mean) - grandAvg
grandAvg
speciesEffects
grandAvg + speciesEffects

# fit the model
aovModel = aov(Petal.Length ~ Species, data = iris)
summary(aovModel)


linModel = lm(Petal.Length ~ Species, data = iris)
summary(linModel)


speciesEffects
meanShiftedModel = lm(Petal.Length - mean(Petal.Length) ~ Species, data = iris)
summary(meanShiftedModel)

speciesEffects
noConstantModel = lm(Petal.Length - mean(Petal.Length) ~ Species - 1, data = iris)
summary(noConstantModel)

anova(noConstantModel)
anova(linModel)



# penguin 2-way ANOVA with interactions
# The ANOVA method:
library(palmerpenguins)
data(penguins)
penguins = subset(penguins,!is.na(body_mass_g))

meansTable = tapply(penguins$body_mass_g,
                    list(penguins$sex,penguins$species),
                    mean)

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

# the regression method:
penguinLM = lm(body_mass_g ~ sex+species+sex:species, data = penguins)
summary(penguinLM)
anova(penguinLM)



# 3-way ANOVA for penguin data
threeWay = aov(body_mass_g ~ sex + species + year, data = penguins)
summary(threeWay)

# 4-way ANOVA
fourWay = aov(body_mass_g ~ sex + species + year + island, data = penguins)
summary(fourWay)
