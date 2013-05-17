library(mosaic)
library(lmtest)	# for LR test
library(MASS) # for ordinal logistic regression

# Import data on gardasil completion
#gardasil = read.csv("gardasil.csv", header=TRUE)
summary(gardasil)

tab1=xtabs(~Completed+AgeGroup,data=gardasil)
tab2=xtabs(~Completed+InsuranceType,data=gardasil)

# Simple chi-squared test of association between
summary(tab1)
summary(tab2)

rowSums(tab1)
colSums(tab1)


# Let's build a logit model for "Completed"

glm1 = glm(Completed~PracticeType+AgeGroup, data=gardasil, family=binomial)
glm1b = glm(Completed~AgeGroup+ PracticeType, data=gardasil, family=binomial)

summary(glm1)
anova(glm1)

glm2 = glm(Completed~PracticeType+AgeGroup+InsuranceType, data=gardasil, family=binomial)
glm3 = glm(Completed~PracticeType+AgeGroup+InsuranceType+Location, data=gardasil,family=binomial)

# Try a likelihood ratio test, which is like an F test for generalized linear models
lrtest(glm2, glm3)

glm4 = glm(Completed~PracticeType+AgeGroup+InsuranceType+Race+Location, data=gardasil, family=binomial)
lrtest(glm4, glm3)
lrtest(glm4, glm2)



# Can also do step-wise selection automatically
# Fit an intercept-only model
glm0 = glm(Completed~1, data=gardasil, family=binomial)

# Now do stepwise selection, defining the maximum variable scope
mystepBIC = step(glm0, scope=~PracticeType+AgeGroup+InsuranceType+Race+Location, k=log(1413))
# Notice how we ended up fitting a smaller model than glm4!
# Not if we use AIC instead
mystepAIC = step(glm0, scope=~PracticeType+AgeGroup+InsuranceType+Race+Location, k=2)



# Now let's try a proportional-odds ordinal logit model
olr1 = polr(factor(Shots)~PracticeType+AgeGroup+InsuranceType+Race+Location, data=gardasil)

# Compare with glm4
coef(glm4)
coef(olr1)

# Notice the cutpoints!
summary(olr1)
confint(olr1, level=0.95)


# How to present the results?
# Way easier to interpret on a probability scale
# Extract the unique entries in the model matrix
gnew = unique(olr1$model)
pred1 = predict(olr1,newdata=gnew, type="probs")

# Bind the labels and predictions together
pred1=cbind(gnew, pred1)

# Spit out a .csv file to be played with in, e.g. Excel
write.csv(pred1, "pred1.csv", row.names=FALSE, quote=FALSE)

