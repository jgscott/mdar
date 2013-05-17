# This is a comment.
# R will ignore anything after the # sign.

# Load the mosaic library
library(mosaic)

# Now we read in the data (in tab-delimited .txt format)
# Use RStudio's "Import Dataset" command or use read.table() 
# rxntime = read.table("rxntime.txt", header=TRUE, sep="\t")


###################################
# Some initial exploratory analysis
###################################

summary(rxntime)
hist(rxntime$PictureTarget.RT)

# Some plots to show between-group and within-group variation
boxplot(PictureTarget.RT ~ FarAway, data=rxntime)
stripchart(PictureTarget.RT ~ Littered, data=rxntime, method='jitter', vertical=TRUE, jitter=.02)


# Compute the group-wise means and standard deviations.
# If you haven't loaded the mosaic library,
# this will generate an error.
mean(PictureTarget.RT ~ FarAway, data=rxntime)
mean(PictureTarget.RT ~ Littered, data=rxntime)

sd(PictureTarget.RT ~ FarAway, data=rxntime)
sd(PictureTarget.RT ~ Littered, data=rxntime)


###################################
# Simple group-wise models
###################################

# First by whether the scene was littered
lm1 = lm(PictureTarget.RT ~ Littered, data=rxntime)
coef(lm1)

# Compare the model coefficients with the group means
mean(PictureTarget.RT ~ Littered, data=rxntime)


# Residuals and fitted values
fitted(lm1)
resid(lm1)
cbind(rxntime$PictureTarget.RT, fitted(lm1) + resid(lm1)) 

# The decomposition of variance
# Define a function to calculate the sum of squared deviations from the mean
sumsq = function(x) {
  return( sum((x - mean(x))^2) )
}
sumsq(rxntime$PictureTarget.RT)
sumsq(fitted(lm1))
sumsq(resid(lm1))
sumsq(fitted(lm1)) + sumsq(resid(lm1))

# Analysis of variance
anova(lm1)
sumsq(fitted(lm1)) / sumsq(rxntime$PictureTarget.RT) 

# Can pick off R^2 directly from the summary command
summary(lm1)

# Now by subject
# Notice the factor command
# This tells R that Subject is a label,
# not a number with a meaningful magnitude.
lm2 = lm(PictureTarget.RT ~ factor(Subject), data=rxntime)
anova(lm2)

# Results are expressed in "baseline/offset" form
summary(lm2)


###################################
# More than one grouping factor
###################################

# Stratify means and sd by two categories
# Notice that the design is balanced: 480 in each group
mean(PictureTarget.RT ~ Littered + FarAway, data=rxntime)
sd(PictureTarget.RT ~ Littered+FarAway, data=rxntime)
tally( ~ Littered + FarAway, data=rxntime)


# Fit a model with main effects only
# That is, where the Littered and FarAway effects are separable
lm3 = lm(PictureTarget.RT ~ Littered+FarAway, data=rxntime)

# Examine the coefficients
coef(lm3)

# Include an interaction term
# This allows that the joint effect of Littered and FarAway
# may be different than the sum of the parts
lm3int = lm(PictureTarget.RT ~ Littered+FarAway+Littered:FarAway, data=rxntime)

# This is shorthand for the same model statement.
lm3int = lm(PictureTarget.RT ~ Littered*FarAway, data=rxntime)

# And the variance decomposition.
anova(lm3)
anova(lm3int)

# What if we flipped the order?
lm3b = lm(PictureTarget.RT ~ FarAway + Littered, data=rxntime)

# Notice that the coefficients are the same.
# This will always be true.
coef(lm3); coef(lm3b)

# The variance decomposition is also the same.
# This will not always be true.
# Here it is a consequence of balanced design -- no collinearity.
anova(lm3)
anova(lm3b)

# Is the subject-specific effect left in the residuals from model 3?
boxplot(resid(lm3)~factor(Subject), data=rxntime)

# Looks like we have not adequately accounted for subject-specific differences
# Fit a model with the interaction term and subject-specific dummy variables
lm4 = lm(PictureTarget.RT ~ Littered + FarAway + Littered:FarAway + factor(Subject), data=rxntime)
coef(lm4)


###################################
# Quantifying parameter uncertainty
###################################

# Estimate sampling distributions via bootstrapping

# Try this a few different times
# Different coefficients each time
lm(PictureTarget.RT ~ Littered, data=resample(rxntime))

# Now collect 1000 bootstrapped samples
# do(1000) encodes a simple for loop
myboot = do(1000)*lm(PictureTarget.RT ~ Littered, data=resample(rxntime))

# Summarize the sampling distributions
hist(myboot)
sd(myboot)

# Compare with the normal-theory standard errors
# Pretty close!
lm1 = lm(PictureTarget.RT ~ Littered, data=rxntime)
summary(lm1)
sd(myboot)

# Notice that the t-stat from summary
# is identical to a two-sample t test
t.test(PictureTarget.RT ~ Littered, data=rxntime)

# Can also extract a confidence interval
confint(lm1, level=0.95)
confint(myboot, level=0.95)




###################################
# Interactions and Model uncertainty
###################################

# Recall the model with an interaction and subject-specific dummies
lm4 = lm(PictureTarget.RT ~ Littered + FarAway + Littered:FarAway + factor(Subject), data=rxntime)

coef(lm4)

# Do we need the interaction term?
# Or is its estimated value consistent with the null hypothesis?
# Here H0 = "no interaction term necessary"

# A quick way: just inspect the standard error

# First, bootstrapping
myboot = do(1000)*lm(PictureTarget.RT ~ Littered + FarAway + Littered:FarAway + factor(Subject), data=resample(rxntime))
coef(lm4)
sd(myboot)

# Looks like the approximate 95% CI for the interaction term
# is 39 +/- 2*12, or basically (15,63).  This doesn't contain 0.

# Compare to the normal-theory inference
summary(lm4)
confint(lm4)

