############################################################
#
#Author: Anny Cui and Jiafeng Gu
#Date: Nov. 13, 2022
#
#Goal: Investigate how the COVID-19 and different business sectot influence
#       MIGA projects'gross issued amount.
#
#Data: Dataset comes from WBG provided by MIGA. Adjust the original dataset for 
#      fitting the model. 
#     The data frame named newData2 is used for the model.
#
#This is an observation with collected real-world dataset, so we cannot assume
#causality in advance.
#
#Response: MIGA.Gross.Issued.Amount (in dollars)
#Predictors: MIGA.Issued.FY (fiscal year for issue the amount, 
#                                                 if influenced by COVID-19)
#                             2 levels (Yes, No) 
#
#            MIGA.Business.Sector (business sector receive the investment)
#                           4 levels (Financial,
#                                     Infrastructure,
#                                     Manufacturing,
#                                     Services)
#
#############################################################
#
#import the dataset and do adjustments
MIGA<-read.csv(file.choose(), header=TRUE)
head(MIGA)
str(MIGA)

#Check for needed variables, same length without NaNs.
nrow(subset(MIGA, !is.na(Gross.Issued.Amount)))#510 obsersations
nrow(subset(MIGA, !is.na(Business.Sector)))#510 obs.
nrow(subset(MIGA, !is.na(Issued.FY)))#510 obs.

#create a new data frame including needed variables
newData<-data.frame(MIGA$Gross.Issued.Amount, 
          MIGA$Issued.FY,
          MIGA$Business.Sector)

head(newData)
str(newData)

factor(newData$MIGA.Business.Sector) #13 levels
tapply(newData$MIGA.Gross.Issued.Amount, newData$MIGA.Business.Sector,length)
    #some sectors are mixed, hard to distinguish where the investment goes,
#   such as, Agribusiness|Manufacturing with 2 issued counts.
#   some sectors have less issued counts like Construction only receive 2 counts


#Remove unhelpful business sector levels, choose 4 levels
newData2<-data.frame(subset(newData, 
                           (MIGA.Business.Sector=="Financial")|
                            (MIGA.Business.Sector=="Infrastructure")|
                            (MIGA.Business.Sector=="Manufacturing")|
                            (MIGA.Business.Sector=="Services")))


suppressWarnings(newData2<-subset(newData2, !is.nan(log(MIGA.Gross.Issued.Amount))))
str(newData2)#427 obs. of  3 variables

#Adjust the data frame, divide years into COVID-19 groups,
#Yes=influenced by COVID-19
#No=not influenced by COVID-19
nrow(subset(newData2, MIGA.Issued.FY>=2019))#163 obs.
nrow(subset(newData2, MIGA.Issued.FY<=2018))#264 obs.

newData2[which(newData2$MIGA.Issued.FY>=2019),]$MIGA.Issued.FY<-"Yes"
newData2[which(newData2$MIGA.Issued.FY<=2018),]$MIGA.Issued.FY<-"No"

#
#####################################################
#
#newData2 used for fitting the model
head(newData2)
str(newData2)

factor(newData2$MIGA.Issued.FY)# 2 levels, Yes, No
factor(newData2$MIGA.Business.Sector)# 4 levels, selected sectors

#Data exploration
boxplot(MIGA.Gross.Issued.Amount~MIGA.Issued.FY+MIGA.Business.Sector,
        data=newData2) #different variance between groups and many outliers

boxplot(log(MIGA.Gross.Issued.Amount)~MIGA.Issued.FY+MIGA.Business.Sector,
        data=newData2) #log transformation, better to see side-by-side boxplot
                      #business sectors show additive effects

#####
boxplot(MIGA.Gross.Issued.Amount~MIGA.Business.Sector+MIGA.Issued.FY,
        data=newData2)

boxplot(log(MIGA.Gross.Issued.Amount)~MIGA.Business.Sector+MIGA.Issued.FY,
        data=newData2)# log graph does better job to show variances.
                      #Overall, COVID-19 influence the issued amount received by
                      #each business sector (more), but Infrastructure not,
                      #consider interaction term.
###
interaction.plot(x.factor=newData2$MIGA.Issued.FY,
                 trace.factor = newData2$MIGA.Business.Sector,
                 response = newData2$MIGA.Gross.Issued.Amount,
                 fun=mean,
                 ylab = "Gross Issued Amount (dollars)", 
                 xlab = "Covid",
                 trace.label = "Business Sector",
                 col=c("red","blue","purple","black")) 
#Infrastructure is not roughly parallel to other three sectors

interaction.plot(x.factor=newData2$MIGA.Business.Sector,
                 trace.factor = newData2$MIGA.Issued.FY,
                 response = newData2$MIGA.Gross.Issued.Amount,
                 fun=mean,
                 ylab = "Gross Issued Amount (dollars)", 
                 xlab = "Business Sector",
                 trace.label = "Covid",
                 col=c("blue","red")) #Double check for interaction term


meansTable = tapply(newData2$MIGA.Gross.Issued.Amount,
                    list(newData2$MIGA.Issued.FY,newData2$MIGA.Business.Sector),
                    mean)
meansTable

plot(meansTable[1,],meansTable[2,])
testModel<-lm(meansTable[2,]~meansTable[1,])
abline(testModel)
testModel #slope is 0.689, indicates interaction

#
######################################################
#
#construct the log model, does better job
logModel<-aov(log(MIGA.Gross.Issued.Amount)~MIGA.Issued.FY+MIGA.Business.Sector
               +MIGA.Business.Sector:MIGA.Issued.FY, data=newData2)
summary(logModel)#business sector and interaction are statistically significant

# Note: not all terms are significant
# Issued.FY is not statistically significant
# so intervals and effect sizes do not have to be Business.Sector-specific

#fit the log model with interaction, grand average, group effects, effect sizes
grandAvg2<-mean(log(newData2$MIGA.Gross.Issued.Amount))
covidEffect2<-tapply(log(newData2$MIGA.Gross.Issued.Amount), 
                    newData2$MIGA.Issued.FY, mean)-grandAvg2
sectorEffect2<-tapply(log(newData2$MIGA.Gross.Issued.Amount), 
                     newData2$MIGA.Business.Sector, mean)-grandAvg2

SD2<-sqrt(deviance(logModel)/logModel$df.residual)
tStar<-qt(0.975,df = df.residual(logModel))
ns<-table(newData2$MIGA.Issued.FY, newData2$MIGA.Business.Sector)

ns #each pair of groups have huge different length

#find effect sizes
logMeans = tapply(log(newData2$MIGA.Gross.Issued.Amount),
                    list(newData2$MIGA.Issued.FY,newData2$MIGA.Business.Sector),
                    mean)
logMeans

financialEffect = (logMeans[2,1] - logMeans[1,1])/SD2
infrastructureEffect = (logMeans[2,2] - logMeans[1,2])/SD2
manufacturingEffect = (logMeans[2,3] - logMeans[1,3])/SD2
servicesEffect = (logMeans[2,4] - logMeans[1,4])/SD2

effects = rbind(financialEffect, 
                infrastructureEffect, 
                manufacturingEffect, 
                servicesEffect)
rownames(effects) = c("Financial",
                      "Infrastructure",
                      "Manufacturing",
                      "Services")
colnames(effects) = "Effect Size"
effects

#calculate interaction
interaction2 = (log(meansTable)
               -cbind(covidEffect2,covidEffect2,covidEffect2,covidEffect2)
               -rbind(sectorEffect2,sectorEffect2) 
               -grandAvg2)
interaction2

#assess the log model, 
plot(logModel$fitted.values, logModel$residuals)
abline(h=0) #seems zero-mean
boxplot(logModel$residuals~newData2$MIGA.Issued.FY+
          newData2$MIGA.Business.Sector)#better distributed
boxplot(logModel$residuals~newData2$MIGA.Business.Sector+
          newData2$MIGA.Issued.FY)#better distribution
qqnorm(logModel$residuals)
qqline(logModel$residuals)#displays a linear trend
lattice::densityplot(logModel$residuals)#seems normal distributed

####
tuk<-TukeyHSD(logModel)
tuk
exp(tuk$MIGA.Issued.FY)
exp(tuk$MIGA.Business.Sector)
exp(tuk$`MIGA.Issued.FY:MIGA.Business.Sector`)

plot(TukeyHSD(logModel, conf.level=.95), las = 2)
