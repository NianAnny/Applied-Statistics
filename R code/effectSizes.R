# Jake Price
# October 21, 2022
#
# Effect sizes and confidence intervals in ANOVA
#
# Goal: Investigate the impact of different soil additives on proportion of fine roots in root systems
#
# This is a randomized controlled experiment, so we can infer causation
#
# Response: Diameter - % of root system comprised with "fine roots" (smaller than 0.6 mm diameter)
# Explanatory: Trt - treatment group C (Control), N (Nitrogen), and P (Phosphorus)
#
# We begin with visualization and data exploration
# We will construct a one-way ANOVA model
# We will compute confidence intervals and effect sizes
#    NOTE: These CIs and Effect Sizes need to be computed using the formulae in the textbook
#          As far as I could find, there is not an easy way to do it automatically in R

# Data exploration
rootData<-read.csv(file.choose(), header=TRUE)
boxplot(Diameter ~ Trt, data = rootData)
lattice::xyplot(Diameter ~ factor(Trt), data = rootData)
lattice::densityplot(~Diameter, groups = Trt, data = rootData, auto.key = TRUE)

# Construct model
treeModel = aov(Diameter ~ Trt, data = rootData)
summary(treeModel)
# F-stat is 3.42 on 2 degrees of freedom
# p-value associated with that is 0.0416
# We have statistically significant evidence that the proportion
# of fine roots differs between the three groups

# Assess model
lattice::xyplot(treeModel$residuals~factor(rootData$Trt))
plot(treeModel$fitted.values,treeModel$residuals)
boxplot(treeModel$residuals~rootData$Trt)
qqnorm(treeModel$residuals)
lattice::densityplot(treeModel$residuals, groups = rootData$Trt)
# model assessment looks good - all plots show roughly normal residuals
# with roughly same variance across all three groups



# construct confidence intervals for the mean of each group

# get all group means
means = tapply(rootData$Diameter,rootData$Trt,mean)

# extract the standard deviation of the residuals from the model
SD = sqrt(deviance(treeModel)/df.residual(treeModel))
tStar = qt(0.975,df = df.residual(treeModel))

# get total observations in each group
ns = table(rootData$Trt)

# use all of these to construct confidence intervals!
lowerLimit = means - tStar*SD*sqrt(1/ns)
upperLimit = means + tStar*SD*sqrt(1/ns)

rbind(lowerLimit,upperLimit)



# confidence interval for difference in means between C and P
lower = means["P"] - means["C"] - tStar*SD*sqrt(1/ns["P"]+1/ns["C"])
upper = means["P"] - means["C"] + tStar*SD*sqrt(1/ns["P"]+1/ns["C"])

lower
upper




# effect size of each group
groupEffect = means - mean(rootData$Diameter)
groupEffect / SD


# effect size for difference between control and Phosphorus
(means["P"] - means["C"])/SD

