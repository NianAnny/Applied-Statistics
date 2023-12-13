# Choir Data Inference with Interactions
#
# Jake Price
# 11/11/2022
#
# Question: Do the different choirs have different average heights after
#            controlling for parts? Is it the same for all parts?
#
# Data: Measurements of singers in two Portland area choirs
#
# Response: Height (in)
# Block: Part (SATB)
# Treatment: choir (Portland Symphonic Choir, Reed Chorus)

# load the data
head(choir)

# data exploration
boxplot(height ~ choir+ part, data = choir)

# fit an ANOVA with interactions
model = aov(height ~ choir + part + choir:part, data = choir)
summary(model)

count = table(choir$choir)

# get the t* needed for 95% intervals
tStar = qt(0.975,df = model$df.residual)

# get the standard deviation of the residuals
SD = sqrt(deviance(model)/model$df.residual)

left = (model$effects["choirReed Chorus"] 
        - tStar*SD*sqrt(1/count[1]+1/count[2]))

right = (model$effects["choirReed Chorus"] 
        + tStar*SD*sqrt(1/count[1]+1/count[2]))

c(left,right)

effectSize = model$effects["choirReed Chorus"]/SD
effectSize

TukeyHSD(model)
