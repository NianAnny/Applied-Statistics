# Jake Price
# 10/19/2020
# 
# ANOVA Inference
#
# Goal: Create an ANOVA model of highway MPG
#       for different classes of cars
#
# Data: cars93 dataset Data was gathered on 93 different vehicles from Consumer Reports.
#
# Response: HMPG - miles per gallon on the highway
# Explanatory: Type of car
#    Compact, Large, Midsize, Small, Sporty, Van

# import data and make sure it loaded right
cars = read.csv(file.choose())
head(cars)

# data exploration
# side-by-side boxplot
boxplot(HMPG ~ Type, 
        data = cars)
lattice::xyplot(HMPG ~ factor(Type), 
                data = cars)
lattice::densityplot(~HMPG, 
                     groups = Type, 
                     data = cars, 
                     auto.key = TRUE)
# * the groups seem to have different averages
# * there are some outliers in small
# * the variances don't seem to be exactly the same, but
#      they're not too far off

# find the grand average
mean(cars$HMPG)

# find group effects
tapply(cars$HMPG, cars$Type, mean) - mean(cars$HMPG)

# construct the ANOVA model and inspect it
carAOV = aov(HMPG ~ Type, data = cars)
summary(carAOV)

# p-value from F-test: 3.9 x 10^-15 ~= 0
# There is statistically significant evidence that the different types of cars
# do not all have the same average highway MPG

# conditions of inference
# Assume random samples from each group
# residuals:
boxplot(carAOV$residuals~cars$Type)
qqnorm(carAOV$residuals)
plot(carAOV$fitted.values,carAOV$residuals)
# the variances don't seem to be hugely different
# from one another

