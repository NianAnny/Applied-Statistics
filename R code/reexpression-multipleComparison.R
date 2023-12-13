# Jake Price
# 10/24/2022
# Reexpressions and Multiple Comparisons
#
# Data: Woods lab root data
#
# Response: % of root system made up of "fine roots"
# Explanatory: Trt (control, nitrogen added to soil
#                    phosphorus added to soil)

# look at data
roots<-read.csv(file.choose(),header=TRUE)
head(roots)

# assess whether re-expression would be helpful
means = tapply(roots$Diameter,roots$Trt,mean)
SDs = tapply(roots$Diameter,roots$Trt,sd)

plot(log(means),log(SDs))
heuristicModel = lm(log(SDs)~log(means))
abline(heuristicModel)
# slope of 1.3 ~= 1, so try a log transformation

boxplot(Diameter~Trt,roots)
boxplot(log(Diameter)~Trt, roots)

# construct both models
linearModel = lm(Diameter~Trt, roots)
logModel = lm(log(Diameter)~Trt, roots)

qqnorm(linearModel$residuals)
qqnorm(logModel$residuals)

exp(logModel$coefficients)



# inspect the linear model
summary(linearModel)

# Tukey HSD:
TukeyHSD(aov(Diameter~Trt, roots))
