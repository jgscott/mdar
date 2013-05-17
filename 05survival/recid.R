library(foreign)	# to read Stata files
library(survival)	# survival modeling


###############################################
# A wee demo showing how to read in Stata files
###############################################

# Session > Set Working Directory > To Source File Location
recid = read.dta("recid.dta")

durat.y = Surv(recid$durat, 1-recid$cens)

# Show survival curves
sur1 = survfit(durat.y ~ factor(married), data=recid)
plot(sur1, lty=1:2)
legend("topright", legend=levels(factor(recid$married)), lty=1:2)



###############################################
# Data from Rossi et al
###############################################

Rossi = read.table("Rossi.txt", header=TRUE)

Rossi[1:5, 1:10]

mod.allison <- coxph(Surv(week, arrest) ~ fin + age + race + wexp + mar + paro + prio, data=Rossi)
summary(mod.allison)

# And so forth from the Fox write-up