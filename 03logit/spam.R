library(mosaic)

spam = read.csv("spam.csv", header=TRUE)
names(spam)

# Fit a logistic regression model to predict spam status
# The ~. syntax says "regress upon all variables not otherwise named."
glm1 = glm(y~., data=spam, family=binomial)
# Don't worry about this warning message.
# It's just telling you that some of the messages were predicted
# as spam with probability 0 or 1, to (finite) numerical precision.

coef(glm1)
exp(coef(glm1))

sigmoid = function(x) {
  exp(x)/(1+exp(x))
}

curve(sigmoid, from=-3,to=3)


boot1 = do(1000)*{
  
}

# Quantifying uncertainty in the multiple logit model
# Inference based on asymptotic normality
summary(glm1)
exp(confint(glm1))



# Analysis of deviance
# Deviance is based on likelihood, and is the analogue of sum of squares.
summary(glm1)
anova(glm1)


# Suppose now we wanted to predict on a new data set.
#spamtest = read.csv("spamtest.csv", header=TRUE)
psipred = predict.glm(glm1, newdata=spamtest)
wpred = exp(psipred)/(1+exp(psipred))

# Compare predicted probabilities to actual outcomes.
stripchart(wpred~spamtest$y, method='jitter', vertical=TRUE)

# Could also construct wpred this way:
wpred = predict.glm(glm1, newdata=spamtest, type='response')

# Tabulate the confusion matrix.
# First record whether mypred was bigger than 50%.
# Then compare versus truth.
class1 = (wpred > 0.5)
tab1 = xtabs(~ class1 + spamtest$y)
tab1;

# Convert to proportions; could compute FPR, FDR, etc
prop.table(tab1)
prop.table(tab1, margin=2)





##### Bonus: parametric bootstrapping

pboot.logit = function(myglm, nmc=1000) {
  psi = myglm$linear.predictors
  n = length(psi)
  w = 1/{1+exp(-psi)}
  x = as.matrix(myglm$model[,2:ncol(myglm$model)])
  betahat = matrix(0, nrow=nmc, ncol=length(coef(myglm)))
  for(i in 1:nmc) {
    y = rbinom(n, 1, w)
    betahat[i,] = coef(glm(y~x, family=binomial))
  }
  betahat
}


pboot1 = pboot.logit(glm1, 250)
apply(pboot1, 2, sd)
