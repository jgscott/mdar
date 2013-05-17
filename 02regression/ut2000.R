library(mosaic)
library(lattice)
library(contrast)

#######  UT Class of 2000 GPA/SAT data
# Read in the data set, which includes every student who
# entered the University of Texas in the Fall of 2000
# Just use the Import Dataset button under Workspace.
# ut2000 = read.csv("ut2000.csv", header=T)

# Look at the first and last few rows of ut2000 to get a sense of what it looks like
head(ut2000)
tail(ut2000)

plot(GPA ~ SAT.V, data=ut2000)
plot(GPA ~ SAT.Q, data=ut2000)
lm1 = lm(GPA ~ SAT.Q + SAT.V, data=ut2000)
coef(lm1)


# Boxplots: GPA by college
boxplot(GPA ~ School, data=ut2000)
# Large within-group variability
# But still differences among schools
boxplot(GPA ~ School, data=ut2000, las=2, cex.axis=0.6)



# The xyplot function in the lattice package is great.
# Useful for paneled plots.
# It tends to give nice output with minimal fuss.
xyplot(GPA ~ SAT.Q | School, data=ut2000)
xyplot(GPA ~ SAT.V | School, data=ut2000)


# Fit a model with SAT Math, Verbal, and College as predictors
# Notice that Architecture is the baseline
# R's default ordering is alphabetical! ("Alabama first.")
lm2 = lm(GPA ~ SAT.Q + SAT.V + School, data=ut2000)
summary(lm2)

# Arch. has a higher average GPA than, for example, Education.
# But the Education coefficient is positive.
# How should we interpret this?
boxplot(GPA ~ School, data=ut2000, las=2, cex.axis=0.6)
coef(lm2)

# A permutation test for the School variable
perm2 = do(1000)*lm(GPA ~ SAT.Q + SAT.V + shuffle(School), data=ut2000)
hist(perm2$r.squared)
summary(lm2)

# Compare with a parametric F test
anova(lm1, lm2)
