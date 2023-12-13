##########
#
#Author: Xinyi (Anny) Cui
#Date: Nov. 21, 2022
#
#Question: If insulation has an effect on gas usage for heating?
#
#Data was collected in the 1960s at a house in south-east England.
#
#Response: Temp (C)
#Explanatory: Insulate (Before, After)
#             Gas (consumption 1000's of cubic feet)
#
#########################################
#
#Call the dataset
heatData<-read.table("http://www.statsci.org/data/general/insulgas.txt")
head(heatData)
str(heatData)

heatData<-subset(heatData, V1!="Insulate")
head(heatData)
str(heatData)

heatData$Insulate<-heatData$V1
heatData$Temp<-as.numeric(heatData$V2)
heatData$Gas<-as.numeric(heatData$V3)

heating1<-subset(heatData, Insulate=="Before")
heating2<-subset(heatData, Insulate=="After")

tempBefore<-heatData$Temp[heatData$Insulate=="Before"]
tempAfter<-heatData$Temp[heatData$Insulate=="After"]
gasBefore<-heatData$Gas[heatData$Insulate=="Before"]
gasAfter<-heatData$Gas[heatData$Insulate=="After"]

#data exploration
plot(Temp~Gas, data=heatData)

#add lines
beforeLine<-lm(Temp~Gas, data=heating1)
afterLine<-lm(Temp~Gas, data=heating2)
abline(beforeLine)
abline(afterLine)

#linear model
before<-lm(tempBefore~gasBefore)
after<-lm(tempAfter~gasAfter)

summary(before)
summary(after)

#model
tempModel<-aov(Temp~Insulate, data=heatData)
summary(tempModel)

gasModel<-aov(Gas~Insulate, data=heatData)
summary(gasModel)

#ANCOVA model
model<-lm(Temp~Gas+Insulate, data=heatData)
anova(model)
summary(model)


#Assess the model
qqnorm(model$residuals)
qqline(model$residuals)
plot(model$fitted.values, model$residuals)
lattice::densityplot(model$residuals)

#confidence for difference
SD = sqrt(deviance(model)/model$df.residual)
tStar = qt(0.975,df = model$df.residual)
str(heating1)
str(heating2)

meansTable<-tapply(heatData$Temp, heatData$Insulate, mean)
meansTable

lwr = ((5.350000 - 2.811111) 
              - tStar*SD*sqrt(1/26+1/18))
upr = ((5.350000 - 2.811111) 
       + tStar*SD*sqrt(1/26+1/18))
lwr
upr
