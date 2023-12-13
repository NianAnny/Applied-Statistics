################################################################################
#Xinyi (Anny) Cui
#Oct. 23, 2022
#
#Reexpressions and Multiple Comparisons
#
#Goal: Plot diagnostic graphs and 
#
#Response: Diameter - % of root system comprised with "fine roots" (smaller than 0.6 mm diameter)
#Explanatory: Trt - treatment group C (Control), N (Nitrogen), and P (Phosphorus)
#
#
#Import dataset
rootData<-read.csv(file.choose(), header=TRUE)
head(rootData)
str(rootData)

boxplot(Diameter~Trt, data=rootData)

means = tapply(rootData$Diameter,rootData$Trt, mean)
SDs = tapply(rootData$Diameter,rootData$Trt, sd)
n = tapply(rootData$Diameter, rootData$Trt, length)
cbind(means, SDs, n)

#
diameterAOV<-aov(Diameter~Trt, data=rootData)
summary(diameterAOV)

#Use Fisher's LSD to identify significant difference
MSE<-sum((diameterAOV$residuals)^2)/df.residual(diameterAOV)
tstar<-qt(0.975, df.residual(diameterAOV))
LSD1<-tstar*sqrt(MSE*(1/16 + 1/16))
LSD2<-tstar*sqrt(MSE*(1/16 + 1/15))
cbind(LSD1, LSD2)

differs<-outer(means, t(means),"-")
differs

# Use Tukey's HSD to compute 95% confidence intervals for the difference
TukeyHSD(diameterAOV, conf.level = 0.95)
