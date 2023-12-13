################################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Oct. 2, 2022
#
#Title: Warm-Up: Comparing Two Regression Lines
#
#Data comes from the build-in R dataset iris
#
################################################################################
#
#Recall iris dataset
head(iris)
str(iris)
attach(iris)
#
#Produce a scatterplot of petal.length~sepal.length colored by by species
lattice::xyplot(Petal.Length~Sepal.Length, data=iris, groups=Species, 
                auto.key=TRUE,
                xlab="Sepal Length of iris", ylab="Petal Length of iris",
                main="Scatterplot of Petal Length v.s. Sepal Length by Species",
                type=c("p","r"))
#
#Construct the model
lengthModel<-lm(Petal.Length~Sepal.Length + Species + Sepal.Length*Species)
summary(lengthModel)
anova(lengthModel)
#
#Check for the linearity
plot(lengthModel$fitted.values, lengthModel$residuals)
hist(lengthModel$residuals)
qqnorm(lengthModel$residuals)
lattice::densityplot(lengthModel$residuals)
