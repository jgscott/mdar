library(mosaic)
library(lattice)
library(ggplot2)

#######  UT Class of 2000 GPA/SAT data
# Read in the data set, which includes every student who
# entered the University of Texas in the Fall of 2000
# Just use the Import Dataset button under Workspace.
# ut2000 = read.csv("ut2000.csv", header=T)
summary(ut2000)

# Look at the first and last few rows of ut2000 to get a sense of what it looks like
head(ut2000)
tail(ut2000)

levels(ut2000$School)

# Rename the levels to avoid labels that YELL AT YOU
ut2000$School = factor(ut2000$School, labels=c("Architecture", "Business", "Communications", "Education", "Engineering", "Fine Arts", "Liberal Arts", "Natural Science", "Nursing", "Social Work"))


mean(SAT.Q ~ School, data=ut2000)
mean(SAT.V ~ School, data=ut2000)


# Store the sample size in a variable called N
N = nrow(ut2000)

# Boxplots stratified by college
boxplot(GPA ~ School, data=ut2000)
# Large within-group variability
# But still differences among schools

plot(GPA ~ SAT.V, data=ut2000)

# Add artificial jitter for plotting purposes
plot(GPA ~ jitter(SAT.V), data=ut2000)

# The xyplot function in the lattice package is great.
# Useful for paneled plots.
# It tends to give nice output with minimal fuss.
xyplot(GPA ~ SAT.Q | School, data=ut2000)
xyplot(GPA ~ SAT.V | School, data=ut2000)


# Fit a model with SAT Math, Verbal, and College as predictors
# Notice that Architecture is the baseline
# R's default ordering is alphabetical! ("Alabama first.")
lm1 = lm(GPA ~ SAT.Q + SAT.V + School, data=ut2000)
summary(lm1)
anova(lm1)


# Arch. has a higher average GPA than, for example, Education.
# But the Education coefficient is positive.
# How should we interpret this?
boxplot(GPA ~ School, data=ut2000)
coef(lm1)

