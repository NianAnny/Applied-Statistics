# Jake Price
# 10/14/2022
#
# Introduction to ANOVA by way of two-sample mean comparison
#
# Data: cars93 (from next Wednesday's warm-up)
# Response: Highway MPG (HMPG)
# Explanatory: Manual (0 = automatic, 1 = manual)

# data visualization
cars<-read.csv(file.choose(),header=TRUE)
boxplot(HMPG ~ Manual, data = cars, horizontal = TRUE)
lattice::densityplot(~HMPG, groups = Manual,
                     data = cars,
                     auto.key = TRUE)

# grab the overall mean and the mean by group
mean(cars$HMPG)
tapply(cars$HMPG,cars$Manual,mean)

# analyzing with linear regression
linModel = lm(HMPG ~ Manual, data = cars)
summary(linModel)
plot(linModel$fitted.values,linModel$residuals)
confint(linModel)

# analyzing with t.test
t.test(HMPG~Manual, data = cars) # Welch's t-test which corrects for
                                 # differences in variance between groups
t.test(HMPG~Manual, data = cars, var.equal = TRUE)

# analyzing with t.test
mean(cars$HMPG) #grand average
tapply(cars$HMPG,cars$Manual,mean) - mean(cars$HMPG) # group effects





# analyzing iris data with ANOVA
mean(iris$Petal.Length)
tapply(iris$Petal.Length,iris$Species,mean) - mean(iris$Petal.Length) # group effects
