# Jake Price
# 10/10/20
# Multiple linear regression of mammal lifespan
#
# Data: http://www.statsci.org/data/general/sleep.txt
# Documentation: https://rdrr.io/cran/openintro/man/mammals.html
#
# Goal: Estimate mammal life span using constitutive variables
#
#Variables:
#
#species: Species of mammals
#body_wt: Total body weight of the mammal (in kg)
#brain_wt: Brain weight of the mammal (in kg)
#non_dreaming: Number of hours of non dreaming sleep
#dreaming: Number of hours of dreaming sleep
#total_sleep: Total number of hours of sleep
#life_span: Life span (in years)
#gestation: Gestation time (in days

# load and check data
mammals = read.table("http://www.statsci.org/data/general/sleep.txt",header = TRUE)
head(mammals)

# Begin by visualizing each quantitative variable and assessing need
# for transformations
lattice::densityplot(~LifeSpan, data = mammals) # skewed right
mammals = transform(mammals, logL = log(LifeSpan))
lattice::densityplot(~logL, data = mammals)

lattice::densityplot(~BodyWt, data = mammals)
# notice huge outliers - elephants! Let's remove them and note
# that we are restricting our scope to non-massive mammals
mammals = subset(mammals, BodyWt < 2000)
lattice::densityplot(~BodyWt, data = mammals)
# now it's easier to read, and it looks super skewed!
mammals = transform(mammals, logBW = log(BodyWt))
lattice::densityplot(~logBW, data = mammals)

# brain weight
lattice::densityplot(~BrainWt, data = mammals)
mammals = transform(mammals, logBR = log(BrainWt))
lattice::densityplot(~logBR, data = mammals)

# gestation
lattice::densityplot(~Gestation, data = mammals)
mammals = transform(mammals, logG = log(Gestation))
lattice::densityplot(~logG, data = mammals)

# sleep ones
lattice::densityplot(~Dreaming, data = mammals)
lattice::densityplot(~NonDreaming, data = mammals)
lattice::densityplot(~TotalSleep, data = mammals)

# look at how all variables impact response
car::scatterplotMatrix(subset(mammals, select = c("logL",
                                                  "logBW",
                                                  "logBR",
                                                  "logG",
                                                  "Dreaming",
                                                  "NonDreaming",
                                                  "TotalSleep")))
# notice reasonable relationships between response 
# and every potential predictor
# However, many predictors are highly correlated!
# Specifically: body weight and brain weight,
# nondreaming sleep and total sleep

# let's make a model with all the variables, except only
# one of body weight / brain weight and only one of 
# nondreaming sleep / total sleep
model1 = lm(logL ~ logBW + logG + Dreaming + NonDreaming, data = mammals)
summary(model1)
# assessment plots
plot(model1$fitted.values,model1$residuals)
qqnorm(model1$residuals)
# print exponentiated coefficients
exp(model1$coefficients)

# try the complete model
completeModel = lm(logL~logBW + logBR + logG + Dreaming + NonDreaming + TotalSleep, data = mammals)
summary(completeModel)
# because TotalSleep = Dreaming + NonDreaming, lm() fails to fit
# a model

# almost complete model
model2 = lm(logL~logBW + logBR + logG + Dreaming + NonDreaming, data = mammals)
summary(model2)
car::vif(model2)
# we see that brain weight and body weight have the highest VIF scores
# and brain weight is more significant than body weight, so let's drop
# body weight

model3 = lm(logL ~ logBR + logG + Dreaming + NonDreaming, data = mammals)
summary(model3)
car::vif(model3)

# directly compare model3 and model 2 (model3 is a reduced version of model2)
anova(model2,model3)
# p-value 0.78 means there is *not* significance evidence that the full
# model is better than the reduced model -> more reason to go with
# the reduced model