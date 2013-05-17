library(survival)

data(colon)
summary(colon)
?colon

# First split the data into separate data sets for
# recurrence and mortality
recur = subset(colon, etype==1)
mort = subset(colon, etype==2)

head(recur)

# Important detail: create a "survival status" object
# for subsequent use by other functions.
# This will be used as the response variable.
# First focus on the recurrence outcome
# The first argument is the time.
# The second is an event indicator.
# 1 = event observed
# 0 = right-censored
recur.y = Surv(recur$time, recur$status)


# Plot survival (Kaplan-Meier) curves
sur1 = survfit(recur.y ~ rx, data=recur)
plot(sur1, lty=1:3, mark=4:6, xlab="Days", ylab="Fraction Without Recurrence")
legend("topright", legend=levels(recur$rx), lty=1:3) 

# We could also suppress the marks for censored events.
plot(sur1, lty=1:3, mark.time=FALSE)
legend("topright", legend=levels(recur$rx), lty=1:3) 


# The proportional hazards assumption looks reasonable.
# Fit the Cox proportional hazards model
ph1 = coxph(recur.y ~ rx + sex + age, data=recur)
summary(ph1)



# Now for mortality outcomes
mort.y = Surv(mort$time, mort$status)
sur2 = survfit(mort.y ~ rx, data=mort)
plot(sur2, lty=1:3, mark=4:6, xlab="Days", ylab="Fraction Surviving")
legend("topright", legend=levels(mort$rx), lty=1:3) 


ph2 = coxph(mort.y ~ rx + sex + age, data=mort)
summary(ph2)

# Analysis of deviance
anova(ph2)


# Dummy variable for obstruction is borderline
ph3 = coxph(mort.y ~ rx + obstruct, data=mort)
summary(ph3)
anova(ph3)


# Dummy variable for ">= 4 positive lymph nodes is clearly influential
ph4 = coxph(mort.y ~ rx + obstruct + node4, data=mort)
summary(ph4)
anova(ph4)

# Let's look at a survival curve
sur4 = survfit(mort.y ~ node4, data=mort)
plot(sur4, lty=1:2, mark=4:5, xlab="Days", ylab="Fraction Surviving")
legend("topright", legend=c("< 4", ">=4"), lty=1:2) 

# Looks like proportional-hazards assumption might be suspect
# What if we assume different baseline hazards for patients with 4 or more positive lymph nodes?
ph5 = coxph(mort.y ~ rx + obstruct + strata(node4), data=mort)
summary(ph5)



# Looks like the treatment dummy variables are fairly robust
coef(ph5)
coef(ph4)
coef(ph3)

