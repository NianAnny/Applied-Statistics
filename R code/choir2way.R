# Jake Price
# Haloween 2022!
# 
# Two-way ANOVA
#
# Data: measurements of heights for each member
#       of the Portland Symphonic Choir and the
#       Reed Chorus
#
# Question: Is one of these choirs taller on average
#           after controlling for singing parts
#
# Response: Height (in)
# Blocking: Part (Soprano, Alto, Tenor, Bass)
#    expect this to impact response by gender
#    T/B > S/A
# Explanatory: Choir

# load and inspect data
choir = read.csv(file.choose())
head(choir)
str(choir)
nrow(choir)
choir$choir2 = factor(choir$choir, labels = c("P","R"))

# plot the data and check for additive effect
# model being reasonable
boxplot(height ~ choir2+part, data = choir)
# looks like Reed is a bit shorter than PSC in most cases
# and by similar amounts

portland = subset(choir, choir2 == "P")
reed = subset(choir,choir2 == "R")

portlandMeans = tapply(portland$height,portland$part,mean)
reedMeans = tapply(reed$height, reed$part, mean)

portlandMeans
reedMeans

# additive effect plot
plot(reedMeans,portlandMeans)
heuristic = lm(portlandMeans~reedMeans)
abline(heuristic)
heuristic
# slope of 0.87 ~= 1 means additive effects not so bad


#interaction plots
interaction.plot(x.factor=choir$choir,
                 trace.factor = choir$part,
                 response = choir$height,
                 fun=mean,
                 ylab = "Height", 
                 xlab = "choirs",
                 trace.label = "part",
                 col=c("red","blue")) 

interaction.plot(x.factor=choir$part,
                 trace.factor = choir$choir,
                 response = choir$height,
                 fun=mean,
                 ylab = "Height", 
                 xlab = "part",
                 trace.label = "choirs",
                 col=c("blue","red"))



# fit the model
grandAve = mean(choir$height)
partEffect = tapply(choir$height,choir$part,mean) - grandAve
choirEffect = tapply(choir$height,choir$choir2,mean) - grandAve

partEffect
choirEffect

cbind(grandAve,grandAve,grandAve)

twoWayModel = aov(height ~ part + choir2+part:choir2, data = choir)
summary(twoWayModel)


#effect size
SD = sqrt(deviance(twoWayModel)/df.residual(twoWayModel))
SD

meansTable = tapply(choir$height,
                    list(choir$choir,choir$part),
                    mean)
(meansTable-grandAve)/SD


TukeyHSD(twoWayModel)

# assess the residuals
plot(twoWayModel$fitted.values,twoWayModel$residuals)
boxplot(twoWayModel$residuals ~ choir$part + choir$choir2)
qqnorm(twoWayModel$residuals)
lattice::densityplot(twoWayModel$residuals)
