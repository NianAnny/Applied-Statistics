#########################
#
#Author: Xinyi (Anny) Cui
#Date: Nov. 28, 2022
#
#Title: Heat Load Multiple Linear Regression Model for Buildings
#Data: Energy Efficiency Dataset from Kaggle
#
#Goal: Construct a model for predicting the heating load.
#
#
#Response: HL: Heating Load
#Variables: RC: Relative Compactness (a ratio)
#           SA: Surface Area (m^2)
#           WA: Wall Area (m^2)
#           RA: Roof Area (m^2)
#           OH: Overall Height (m)
#           O: Orientation (Sun's paths)
#           GA: Glazing Area (percentages of floor area)
#           GAD: Glazing Area Distribution (scenarios of glazing area)
#           HL: Heating Load (kW)
#           CL: Cooling Load (kW)
#   Both HL and CL can be response variable, we choose HL testing energy 
#   efficiency, so models should not include CL.(will cause high multilinear
#   correlation)
#
##############################
#
df<-read.csv(file.choose(), header=TRUE)
head(df)
str(df)

#Data exploration
lattice::densityplot(df$HL, xlab="HL: Heating Load")

lattice::densityplot(df$RC, xlab="RC: Relative Compactness")
lattice::densityplot(df$SA, xlab="SA: Surface Area")
lattice::densityplot(df$WA, xlab="WA: Wall Area")
lattice::densityplot(df$RA, xlab="RA: Roof Area")
lattice::densityplot(df$OH, xlab="OH: Overall Height")
lattice::densityplot(df$O, xlab="Orientation")
lattice::densityplot(df$GA, xlab="GA: Glazing Area")
lattice::densityplot(df$GAD, xlab="GAD: Glazing Area Distribution")

lattice::densityplot(df$CL) #the other variable can be used as response, but 
#                  for energy required of cooling a house. 
#                  Not including in models.


#See how variables affect the response one
#
#matrix scatter plot for all related variables
#scatterplotmatrix loses RA variable, try pairs function instead
#plotting
suppressWarnings(pairs(df[c(10,2,3,4,5,6,7,8,9)], 
      lower.panel=panel.smooth))
#Response variable, HL, seems influenced clearly by RC, SA, WA, RA, OH,
#                                       slightly affected by GA. 
#Also, some other variables express multi-linearity, 
#             RC and SA correlated so much (cannot be in the same model)
#             RC, RA
#             RC, OH
#             SA, RA
#             SA, OH
#             RA, OH
round(cor(df[c(10,2,3,4,5,6,7,8,9)]),digits=3)#double check for correlation again

#########################
#
#full model for comparison 
fullModel<-lm(HL~RC+SA+WA+OH+O+GA+GAD,
              data=df) #0.9162
summary(fullModel) #RA has NA coefficient, remove it.
                    # Then, p-value and R^2 (0.9682) is great.
car::vif(fullModel) #SA with 201.5 is too high to keep.

#Construct models for selection
#candidate models
model1<-lm(HL~RC+WA+O+GA+GAD, data=df)
model2<-lm(HL~WA+OH+O+GA+GAD, data=df)
model3<-lm(HL~WA+OH+GA+GAD, data=df)
model4<-lm(HL~SA+WA+GA, data=df)
model5<-lm(HL~WA+OH+GA, data=df)

AIC(model1, model2, model3, model4, model5)
#model3 seems the best, and then model2

#compare variance inflation factor
car::vif(model2)
car::vif(model3)
                #Both seem good with vif

#check for analysis of variance
anova(model2, model3) #model3 seems reduced form of model2, without O
#p-value is 0.8111, larger than 0.05, not necessory to keep model2

#check for R^2
summary(model2) #0.9108
summary(model3) #0.9108
# R^2 has no difference
#### Choose model3: HL=WA+OH+GA+GAD


#model3: relationship between HL and each explanatory variable
car::avPlots(model3)

#assess the selected model
lattice::densityplot(model2$residuals, xlab="Residuals")
qqnorm(model3$residuals)
qqline(model3$residuals)
plot(model3$fitted.values, model2$residuals, xlab="Fitted Values",
     ylab="Residuals")
abline(h=0)

#select model3: HL=WA+OH+GA+GAD
#95% confidence and prediction intervals
newData<-data.frame(WA=318,
                    OH=7,
                    GA=0,
                    GAD=0)
newData

predict(model3, newData, int="confidence")
predict(model3, newData, int="prediction")


