library(lme4)	# will also need to have Matrix and lattice installed



ut2000 = read.csv("ut2000.csv", header=TRUE)
ut2000$GPA = ut2000$GPA*100

# Main effects
lm1 = lm(GPA~SAT.V + SAT.Q + School, data=ut2000)
anova(lm1)
summary(lm1)

# A mixed-effects model with hierarchical structure for school
#hlm1 = hglm2(GPA ~ SAT.V + SAT.Q + (1 | School), data=ut2000)

hlm1 = lmer(GPA ~ SAT.V + SAT.Q + (1 | School), data=ut2000)
print(hlm1)	# like summary for the lmer package

coef(lm1)
coef(hlm1)
fixef(hlm1)
ranef(hlm1)

# Compare the estimated School intercepts under both models

# Add the intercept to the dummies
batch1 = coef(lm1)[1] + c(0, coef(lm1)[4:12])
batch2 = coef(hlm1)$School[,1]

# Plot the estimated School-specific intercepts
plot(batch1, batch2)
abline(0,1)

# With interaction b/t school and SAT math scores
lm2 = lm(GPA~SAT.V + SAT.Q + School + SAT.Q:School, data=ut2000)
anova(lm2)
summary(lm2)

# Now a mixed-effects model
# This says allow the intercept and SAT.Q slopes to change among the groups
hlm2 = lmer(GPA ~ SAT.V + SAT.Q + (1+SAT.Q|School), data=ut2000)
coef(hlm2)

anova(hlm1, hlm2)

r = ranef(hlm2, postVar=TRUE)
dotplot(r)

attr(r$School,"postVar")

# Compare the estimated SAT.Q slopes
# Notice that the slope of the SAT.Q variable for Social Work
# is estimated to be negative!
coef(lm2)

# This is probably explained by the small sample size for Social Work
xtabs(~School, data=ut2000)

# Add the overall slope to the interaction terms
batch1 = coef(lm2)[2] + c(0, coef(lm2)[13:21])
batch2 = coef(hlm2)$School[,3]

# Pulling the coefficients toward a group mean = shrinkage
plot(batch1, batch2, xlim=c(-0.3,0.2), ylim=c(-0.3,0.2))
abline(0,1)
# Question: why isn't the order of the original coefficients preserved?
# (Think sample size...)


dotplot(ranef(hlm2, postVar = TRUE))

