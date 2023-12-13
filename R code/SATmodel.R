##################################
#
#Author: Xinyi (Anny) Cui
#Date: Now. 15, 2022
#
#Goal: Construct a model to predict the average SAT score in a state.
#
#Data comes from the package Sleuth3, the dataset is case1201
#
#Response: SAT scores
#Explanatory:
#
#####################################
#
#Access the data
install.packages("Sleuth3")
library(Sleuth3)
?case1201

satData<-case1201
head(satData)
str(satData)#each SAT score corresponding to the state

#Data exploration
lattice::den

#complete model
fullModel<-lm(SAT~Takers+Income+Years+Public+Expend+Rank, data = satData)
summary(fullModel)

#check for the correlations between each variable
car::scatterplotMatrix(satData)
#Except the variable Public and Expend, other variables are correlated
#to the response SAT score. Also, some explanatory variables seems correlated.

#compare models
model1<-lm(SAT~Takers+Income+Years+Rank, data=satData)
summary(model1)
car::vif(model1)# vif of Takers is 13.28, drop the variable

model2<-lm(SAT~Income+Years+Rank, data=satData)
summary(model2)
car::vif(model2)#all variables' vif smaller than 5, which is good

#Check for AIC
AIC(fullModel, model1, model2)
#seems fullModel has the lowest AIC values, but as the variable public and expend 
# are less correlated to SAT scores, the full model includes unhelpful variables,
# model2 seems fit the data well.

#
#Assess the model2
plot(model2$fitted.values, model2$residuals)#seems zero-mean
abline(h=0)
qqnorm(model2$residuals)#a littler bent
qqline(model2$residuals)
lattice::densityplot(model2$residuals)#a kind of skewness


#stepwise selection
step(fullModel)
stepModel<-lm(formula = SAT ~ Years + Public + Expend + Rank, data = satData)
summary(stepModel)#R^2=0.8771, which is good
car::vif(stepModel)#no vif more than 5

#Assess stepwise model
AIC(fullModel, model1, model2, stepModel)#stepModel has the lowest AIC
plot(stepModel$fitted.values, stepModel$residuals)#seems zero-mean
abline(h=0)
qqnorm(stepModel$residuals)
qqline(stepModel$residuals)#a little bent
lattice::densityplot(stepModel$residuals)
