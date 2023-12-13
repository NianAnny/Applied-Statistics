# Jake Price
# 11/16/22
# Predict SAT scores using measures of state
#
# Potential explanatory variables:
#   Takers (% who take SAT)
#   Income (median income of test-taker families)
#   Years (how many years of instruction before exam)
#   Public (% in public school)
#   Expend ($ per year spent on students)
#   Rank (school rank of test takers)

#
library("Sleuth3")
?case1201
SATdata = case1201
head(SATdata)

# visualization
lattice::densityplot(~SAT, data = SATdata)
# no transform needed
# bimodal

lattice::densityplot(~Takers, data = SATdata) # bimodal
lattice::densityplot(~Income, data = SATdata)
lattice::densityplot(~Years, data = SATdata)
lattice::densityplot(~Public, data = SATdata)
lattice::densityplot(~Expend, data = SATdata)
lattice::densityplot(~Rank, data = SATdata) # bimodal

# scatterplots
# bimodality due to rank?
plot(SAT ~ Rank, data = SATdata)
plot(SAT ~ Takers, data = SATdata)
plot(Rank ~ Takers, data = SATdata)
# probably should avoid models with rank and takers together

car::scatterplotMatrix(SATdata)


# candidate models
model1 = lm(SAT~Takers+Income+Years+Public+Expend+Rank, data = SATdata)
model2 = lm(SAT~Expend, data = SATdata)
model3 = lm(SAT ~ Expend + Takers, data = SATdata)
model4 = lm(SAT ~ Expend + Public, data = SATdata)
model5 = lm(SAT ~ Income + Years + Public + Expend + Rank, data = SATdata)

AIC(model1,model2,model3,model4,model5)
# first full model looks like the best
car::vif(model1)
car::vif(model5)
anova(model1,model5)
# model 5 is the winner - check the conditions of inference next and then do analysis
