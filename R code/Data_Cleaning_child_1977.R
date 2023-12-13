################################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Sept. 14, 2022
#
#Title: Analysis of the Anthrokids study
#
#Data comes from the Anthrokids study
#
################################################################################
#
#Import the data of children downloaded on PC and see the first six rows
child<-read.csv(file.choose(),header=TRUE)
head(child)
attach(child)
#
#Check the number of variables and that of observations
dim(child)
#
#Check the number of children who do not have a recorded head length
nrow(subset(child,is.na(Head_Length)))
#
#Check the column of Age in Years to see how the data looks like
#and see the sort it to see the data in increasing order
#and check the difference between the largest age and the smallest one
age<-child$Age_in_Years
head(age)
sort(age)
range(age)
#
#Check the column of weight and plot weight against age in years
childWeight<-child$Weight
head(childWeight)
plot(age,childWeight, xlab="Age in 1/1000 Years",ylab="Weight in Newtons",
     main="Original Graph of Age in Years v.s. Weight")
plot(age/1000,childWeight/9.8,xlab="Age (years)",ylab="Weight (kg)",
     main="Converted Graph of Age v.s. Force")
#
#Convert the age in years and weight in newtons
yearAge<-age/1000
head(yearAge)
Force<-childWeight/9.8
head(Force)
#
#Find subsets of Gender equals to one and Gender equals to two
gender1<-subset(child,Gender==1)
nrow(gender1)
gender2<-subset(child,Gender==2)
nrow(gender2)
#
#Graph of Weight against age in the subsets of gender1 and gender2
plot(gender1$Age_in_Years/1000,gender1$Weight/9.8,
     xlab="Age(years)",ylab="Force(N)",main="Gender 1")
plot(gender2$Age_in_Years/1000,gender2$Weight/9.8,
     xlab="Age(years)",ylab="Force(N)",main="Gender 2")
#
#Find subsets of handedness equals to one and it equals to two
handedness1<-subset(child,Handedness==1)
nrow(handedness1)
handedness2<-subset(child,Handedness==2)
nrow(handedness2)
#
#Find children who have handedness listed as a number other than 1 or 2
nrow(subset(child,(Handedness!=1)&(Handedness!=2)))
#
#Check the numbers listed in handedness other than 1 or 2 and how many children
#for each other number
range(Handedness)
nrow(subset(child, Handedness==0))
nrow(subset(child, Handedness==3))
nrow(subset(child, Handedness==4))
nrow(subset(child, Handedness==5))
#
#Check for the column of stature and graph stature against age
head(child$Stature)
plot(age/1000,child$Stature/10,xlab="Age (years)",ylab="Stature (cm)",
     main="Graph of Age in Years against Stature")
#
#Check values in Birth Order column
head(child$Birth_Order)
plot(age/1000,child$Birth_Order,xlab="Age(years)",ylab="Birth_Order",
     main="Graph of Birth Order")
#
#Divide data into groups of males and females
boy<-gender1
girl<-gender2
head(boy)
head(girl)
#
#Create graphs of Stature v.s. age for males and females
boyPlot<-plot(boy$Age_in_Years/1000,boy$Stature/10,xlab="Age (years)",ylab="Height (cm)",
     main="Height v.s. Age (males)")
girlPlot<-plot(girl$Age_in_Years/1000,girl$Stature/10,xlab="Age (years)",ylab="Height (cm)",
     main="Height v.s. Age (females)")
#
#Calculate and compare average and standard deviation of heights for boys and girls
newChild<-droplevels(subset(child,!is.na(Stature)))
attach(newChild)
boyGroup<-subset(newChild,Gender==1)
girlGroup<-subset(newChild,Gender==2)
str(newChild)
n<-tapply(Stature,Gender,length)
average<-tapply(Stature,Gender,mean)
SD<-tapply(Stature,Gender,sd)
cbind(n,average,SD)
#
#Make linear regressions model of height against age for both males and females
plot(boyGroup$Stature~boyGroup$Age_in_Years)
boyModel<-lm(Stature~Age_in_Years,data=boyGroup)
abline(boyModel)
plot(girlGroup$Stature~girlGroup$Age_in_Years)
girlModel<-lm(Stature~Age_in_Years,data=girlGroup)
abline(girlModel)
#
#See summary of models' information
summary(boyModel)
summary(girlModel)
#
#Check values of upper arm circumferences and plot them against age
head(child$Upper_Arm_Circumference)
UAC<-plot(child$Age_in_Years/1000,child$Upper_Arm_Circumference/10,
          xlab="Age (years)",ylab="Upper Arm Circ. (cm)",main=
            "Graph of Upper Arm Circumference")
#
#Create a histogram of Upper Arm Circumference
hist(child$Upper_Arm_Circumference/10,xlab="Upper Arm Circ. (cm)",
     main="Histogram of Upper Arm Circumference")
#
#Transform the values of upper arm circumference and check the distribution
upperArm<-transform(child$Upper_Arm_Circumference,
                    logUpperArm=log(child$Upper_Arm_Circumference))
lattice::densityplot(upperArm$logUpperArm)
qqnorm(upperArm$logUpperArm)
plot(child$Age_in_Years/1000,upperArm$logUpperArm)

