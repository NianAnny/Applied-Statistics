# Jake Price
# 11/2/2022
#
# Goal: Use two-way ANOVA to assess whether
#  different color M&Ms have different average
#  masses after controlling for type
#
# Response: mass (g)
# Blocking: type (plain, peanut, PB)
# Explanatory: Color (Red, Blue, Green, Yellow, Orange, Brown)

# load and check data
mms = read.csv(file.choose())
head(mms)

# begin visualizing and judging additive effects
boxplot(mass ~ type + color, data = mms)
# colors all side-by-side show that additive effects
# model is reasonable
boxplot(mass ~ color + type, data = mms)
# really different variances between groups, try log transform
boxplot(log(mass) ~ color + type, data = mms)
boxplot(1/mass ~ color + type, data = mms)
boxplot(1/mass^2 ~ color + type, data = mms)
# reciprocal transform looks like it does the best job of 
# getting equal variance

# reciprocal transform the response
mms = transform(mms, massR = 1/mass)

# find the grand average and effects
grandAvg = mean(mms$massR)
blockEffect = tapply(mms$massR,mms$type,mean) - grandAvg
colorEffect = tapply(mms$massR,mms$color,mean) - grandAvg

grandAvg
blockEffect
colorEffect

# construct the model
model = aov(massR ~ type + color, data = mms)
summary(model)
# F-statistic for color 2.687 on 5 df, p-value = 0.02

# construct 95% CIs
TukeyHSD(model)
# only red and green seem to be different! However the difference
# is 0.02 grams aka 2 centigrams or 20 mg (very small!)

boxplot(model$residuals~mms$type + mms$color)
boxplot(model$residuals~mms$color + mms$type)



# kind of hard to interpret reciprocally transformed response
# how does log-transform do?
logmodel = aov(log(mass) ~ type + color, data = mms)
boxplot(logmodel$residuals~mms$type + mms$color)
boxplot(logmodel$residuals~mms$color + mms$type)
# hmmm, plain M&M residuals are clearly smaller
# than the peanut + peanut butter...this is a tough call
summary(logmodel)
tuk = TukeyHSD(logmodel)
exp(tuk$color)
# yellow-green  0.9636302 0.9322953 0.9960183
# yellow is between 93% and 99.6% as massive as green