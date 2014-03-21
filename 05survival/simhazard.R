library(survival)

# Simulate a random distribution of survival times

D = 1000
tgrid = seq(1/D, 100, length=D)
Fgrid = 0.3*pgamma(tgrid/4, 10,2) + 0.4*pgamma(tgrid/2,50,2) + 0.3*pgamma(tgrid,80,1)

#CDF of survival times
plot(tgrid, Fgrid)

# pdf of survival times
fgrid = diff(c(0,Fgrid))/diff(c(0,tgrid))
plot(tgrid, fgrid)

# Survival curve
Sgrid = 1-Fgrid
plot(tgrid, Sgrid)

# hazard function: approximate derivative with discrete difference
logS = log(Sgrid)
hgrid = -diff(c(0,logS))/diff(c(0,tgrid))
plot(tgrid, hgrid, ylab='Hazard function', type='l')

# Now modify the baseline hazard with a covariate meeting the proportional hazards assumption
# assume a dichotomous covariate that increases the hazard by exp(1.25)
hgrid2 = exp(1.25)*hgrid

# This implies a simple translation on the log-hazard scale
plot(tgrid, log(hgrid2), type='l', col='red')
lines(tgrid, log(hgrid))

# On the hazard scale, a uniform "inflation"
plot(tgrid, hgrid2, type='l', col='red')
lines(tgrid, hgrid)

# Re-map this hazard functions to a survival (Kaplan-Meier) curve
# Not necessarily an intuitive effect!
S2 = exp(-cumsum(hgrid2)*diff(c(0,tgrid)))
plot(tgrid, Sgrid, type='l')
lines(tgrid, S2, col='red')

