################################################################################
#
#Author: Xinyi (Anny) Cui
#Date: Oct. 20, 2022
#
#Goal: Study how fertilizer additions affect different aspets of plant growth.
#Data comes from a research study by biology professor Carrie Woods.
#Data Description: Select three plots of land at the Gigante Fertilization Plots
#in Panama. one as the control group with no nurtients added to the soil. The 
#second added nitrogen. The third added phophorus.
#
################################################################################
#
#Import dataset
lands<-read.csv(file.choose(),header=TRUE)
head(lands)
str(lands)
#
#Plot the proportion of the fine roots for three groups
boxplot(X.Mycorrhizal~Trt, data=lands)
lattice::densityplot(~ X.Mycorrhizal, groups=Trt, data=lands, auto.key=TRUE)
lattice::xyplot( X.Mycorrhizal~factor(Trt), data=lands)
#
#aov model for diameter
diaAOV<-aov(Diameter~Trt, data=lands)
summary(diaAOV)
#
#confidence interval for the average proportion of fine roots for each group
mean(lands$X.Mycorrhizal) #grand average

rootAOV<-aov(X.Mycorrhizal~Trt, data=lands)
summary(rootAOV)

confint(rootAOV)

#confidence interval for the difference in average proportion of fine roots
#between the control and the phosphorus groups


#group effects
tapply(lands$X.Mycorrhizal, lands$Trt, mean)-mean(lands$X.Mycorrhizal)

