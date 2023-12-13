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
meansTable

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

model = aov(body_mass_g/1000 ~ species + sex + species:sex, data = penguins)
# note, converted mass to kg
summary(model)
# all terms, including interactions, are significant

# assess the model
boxplot(model$residuals ~ penguins$species + penguins$sex)
qqnorm(model$residuals)
lattice::densityplot(model$residuals)
# wow, basically perfectly normal residuals
# exactly equal variance across groups


# Note: all terms significant, including interaction term
# so all intervals and effect sizes should be species-specific

# get the t* needed for 95% intervals
tStar = qt(0.975,df = model$df.residual)

# get the standard deviation of the residuals
SD = sqrt(deviance(model)/model$df.residual)

# convert means to kg
meansKG = meansTable / 1000

# find sample size of each group
count = table(penguins$sex,
              penguins$species)

count

# 95% interval for Adelie
adelieLeft = ((meansKG[2,1] - meansKG[1,1]) 
  - tStar*SD*sqrt(1/count[2,1] + 1/count[1,1]))
adelieRight = ((meansKG[2,1] - meansKG[1,1]) 
              + tStar*SD*sqrt(1/count[2,1] + 1/count[1,1]))


# 95% interval for Chinstrap
chinstrapLeft = ((meansKG[2,2] - meansKG[1,2]) 
              - tStar*SD*sqrt(1/count[2,2] + 1/count[1,2]))
chinstrapRight = ((meansKG[2,2] - meansKG[1,2]) 
               + tStar*SD*sqrt(1/count[2,2] + 1/count[1,2]))


# 95% interval for Chinstrap
gentooLeft = ((meansKG[2,3] - meansKG[1,3]) 
                 - tStar*SD*sqrt(1/count[2,3] + 1/count[1,3]))
gentooRight = ((meansKG[2,3] - meansKG[1,3]) 
                  + tStar*SD*sqrt(1/count[2,3] + 1/count[1,3]))


#print results
CIs = rbind(c(adelieLeft,adelieRight),
            c(chinstrapLeft,chinstrapRight),
            c(gentooLeft,gentooRight))
rownames(CIs) = c("Adelie","Chinstrap","Gentoo")
colnames(CIs) = c("2.5%","97.5%")
CIs

TukeyHSD(model)

# effect sizes
adelieEffect = (meansKG[2,1] - meansKG[1,1])/SD
chinstrapEffect = (meansKG[2,2] - meansKG[1,2])/SD
gentooEffect = (meansKG[2,3] - meansKG[1,3])/SD

effects = rbind(adelieEffect,chinstrapEffect,gentooEffect)
rownames(effects) = c("Adelie","Chinstrap","Gentoo")
colnames(effects) = "Effect Size"
effects
