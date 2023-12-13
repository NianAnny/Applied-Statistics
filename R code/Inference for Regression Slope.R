################################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Sept. 20, 2022
#
#Title: Warm-Up: Inference for Regression Slope
#
#Data comes from build-in R iris data
#
################################################################################
#
#Call for the iris data and subset the setosa species, check for it
irisData<-iris
setosaData<-droplevels(subset(irisData, Species=="setosa"))
head(setosaData)
str(setosaData)
#
#See the graph of sepal length (x) v.s. petal length (y) for setosa data
plot(setosaData$Sepal.Length, setosaData$Petal.Length)
lattice::xyplot(Petal.Length~Sepal.Length, data=setosaData ,type=c("p","r"))
#
#Check for skewness
hist(setosa$Sepal.Length)
hist(setosa$Petal.Length)
#Seems no skewneww
#
#Construct a regression model and see the regression table
setosaModel<-lm(Petal.Length~Sepal.Length,data=setosaData)
abline(setosaModel)
summary(setosaModel)
#
#Construct a 95% confidence interval, default level=0.95
confint(setosaModel)
#
#Confidence interval only for the slope
confint(setosaModel, 2)
