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

# Some plots to show between-group and within-group variation
boxplot(PictureTarget.RT ~ FarAway, data=rxntime)
stripchart(PictureTarget.RT ~ Littered, data=rxntime, method='jitter', vertical=TRUE, jitter=.02)


# Compute the group-wise means
# If you haven't loaded the mosaic library,
# this will generate an error.
mean(PictureTarget.RT ~ FarAway, data=rxntime)
mean(PictureTarget.RT ~ Littered, data=rxntime)

sd(PictureTarget.RT ~ FarAway, data=rxntime)
sd(PictureTarget.RT ~ Littered, data=rxntime)


###################################
# A couple of one-way ANOVA models
###################################

# First by whether the scene was littered
lm1 = lm(PictureTarget.RT ~ Littered, data=rxntime)
coef(lm1)

# Compare the model coefficients with the group means
mean(PictureTarget.RT ~ Littered, data=rxntime)

# Analysis of variance
anova(lm1)

# Can pick off R^2 directly from the summary command
summary(lm1)

# Now by subject
# Notice the factor command
# Used to tell R that Subject is a category label, not a number
lm2 = lm(PictureTarget.RT ~ factor(Subject), data=rxntime)
anova(lm2)
summary(lm2)


###################################
# Looking at more than one grouping factor
###################################

# Stratify means and variances by two categories
# Notice that the design is balanced: 480 in each group
mean(PictureTarget.RT ~ Littered + FarAway, data=rxntime)

# Can also compute standard deviations by group
sd(PictureTarget.RT ~ Littered+FarAway, data=rxntime)


# Fit a model with main effects only
# That is, where the Littered anf FarAway effects are separable
lm3 = lm(PictureTarget.RT ~ Littered+FarAway, data=rxntime)

# Examine the coefficients
coef(lm3)


lm3int = lm(PictureTarget.RT ~ Littered+FarAway+Littered:FarAway, data=rxntime)

lm3int = lm(PictureTarget.RT ~ Littered*FarAway, data=rxntime)


# And the variance decomposition.
anova(lm3)

# What if we flipped the order?
lm3b = lm(PictureTarget.RT ~ FarAway + Littered, data=rxntime)

# Notice that the coefficients are the same.
# This will always be true.
coef(lm3); coef(lm3b)

# The variance decomposition is also the same.
# This will NOT always be true!
# Here it is a consequence of balanced design -- no collinearity.
anova(lm3);
anova(lm3b)


boxplot(PictureTarget.RT~factor(Subject), data=rxntime)

boxplot(resid(lm3)~factor(Subject), data=rxntime)




###################################
# Quantifying parameter uncertainty
###################################

# Estimate sampling distributions via bootstrapping

# Try this a few different times
# Different coefficients each time
lm(PictureTarget.RT ~ FarAway + Littered, data=resample(rxntime))

# Now collect 1000 bootstrapped samples
# do(1000) encodes a simple for loop
myboot = do(1000)*lm(PictureTarget.RT ~ FarAway + Littered, data=resample(rxntime))

# Summarize the sampling distributions
hist(myboot)
sd(myboot)

# Compare with the normal-theory standard errors
# Pretty close!
summary(lm3)

# Can also extract a confidence interval
confint(lm3, level=0.95)


###################################
# Interactions and Model uncertainty
###################################


# Fit a model with an interaction term
lm4 = lm(PictureTarget.RT ~ Littered+FarAway+Littered:FarAway, data=rxntime)

# An equivalent shorthand is:
lm4b = lm(PictureTarget.RT ~ Littered*FarAway, data=rxntime)

coef(lm4)
coef(lm4b)

# Do we need the interaction term?
# Or is its estimated value consistent with the null hypothesis?
# Here H0 = "no interaction term necessary"

# A quick way: just inspect the standard error
# Let's estimate this two ways.

# First, bootstrapping
myboot = do(1000)*lm(PictureTarget.RT ~ FarAway*Littered, data=resample(rxntime))
coef(lm4)
sd(myboot)

# Looks like the approximate 95% CI for the interaction term
# is 39 +/- 2*12, or basically (15,63).  This doesn't contain 0.

# Compare to the normal-theory inference
summary(lm4)
confint(lm4)

# Now let's try a permutation test
# This creates an estimate of the sampling distribution
# of the interaction term and R^2 under H0
myperm = do(1000)*lm(PictureTarget.RT ~ FarAway+Littered+shuffle(FarAway):shuffle(Littered), data=rxntime)
hist(myperm)

# Can focus on just the R^2
# Compare the sampling distribution of R^2 under H0
hist(myperm$"r-squared", 30)
summary(lm4)
abline(v = 0.1292, col='red')

# This is basically equivalent to an F test
# To see this, transform the R^2 values to F stats

n = 1920
pf = 3
pr = 2
fstats = (myperm$"r-squared" - 0.1244799)/(1-myperm$"r-squared") * {n - pf - 1}/{pf-pr}
f4 = (0.1292 - 0.1244799)/(1-0.1292) * {n - pf - 1}/{pf-pr}


# Now compare the fstats under the permutation test to the theoretical distribution
# Make a grid and evaluate the f density
hist(fstats,breaks=50, prob=TRUE)
grid = seq(0, 15, length=1000)
lines(grid, df(grid, pf-pr, n-pf-1))
abline(v = f4, col='red')

# We can extract the p-value associated with the F test in one line
anova(lm4)



###################################
# Random effects for subject?
# (Beware the "random effects" terminology)
###################################

lm5 = lm(PictureTarget.RT ~ Littered+FarAway+Littered:FarAway+factor(Subject), data=rxntime)
summary(lm5)
anova(lm5)

# Subject-specific dummy variables for FarAway and Littered?

lm6 = lm(PictureTarget.RT ~ Littered+FarAway+Littered:FarAway+factor(Subject) + factor(Subject):Littered, data=rxntime)
summary(lm6)

# Note! We cannot judge the joint significance of the new
# subject-specific Littered variables by "star-gazing."
# Why?
anova(lm6)

# Try adding subject-specific FarAway variables
lm7 = lm(PictureTarget.RT ~ Littered+FarAway+Littered:FarAway+factor(Subject)
	+ factor(Subject):Littered
	+ factor(Subject):FarAway,
	data=rxntime)
summary(lm7)
anova(lm7)


