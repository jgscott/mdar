library(mosaic)
library(MASS)

# Read in the data, and tell R what column the row labels are in
# That way it doesn't think week is a categorical variable
flutrends = read.csv("flutrends.csv", header=TRUE, row.names=1)
flutrends2012 = read.csv("flutrends2012.csv", header=TRUE, row.names=1)

# Which observations are actually present?
# t = which(!is.na(flutrends$cdcflu))

# the ~. syntax means regress on all variables not otherwise named
lm1 = lm(cdcflu~., data=flutrends)
glm2 = glm(cdcflu~., data=flutrends, family=poisson(link="log"))


# Plot fitted values over time by making "post-dictions"

post1 = predict(lm1, flutrends, interval="prediction", level=0.95)
yhat1 = post1[,1]
yhat.l1 = post1[,2]
yhat.u1 = post1[,3]


plot(flutrends$cdcflu, col='blue', pch=19, ylim=c(-1000,6000))
lines(yhat1, col='red')
lines(yhat.l1, col='red', lty='dashed')
lines(yhat.u1, col='red', lty='dashed')

# Predictions don't respect the domain restrictions


# Now predict using the Poisson GLM
# Trick: predict on the linear predictor scale first
# Then transform to the original scale
post2 = predict(glm2, flutrends, se.fit=TRUE)
yhat2 = exp(post2$fit)
yhat.l2 = exp(post2$fit - 1.96*post2$se.fit)
yhat.u2 = exp(post2$fit + 1.96*post2$se.fit)

plot(flutrends$cdcflu, col='blue', pch=19)
lines(yhat2, col='red')
lines(yhat.l2, col='red', lty='dashed')
lines(yhat.u2, col='red', lty='dashed')

# Predictions do not adequately reflect the over dispersion
# This is a function of the mean-variance relationship of the Poisson


# Let's try a negative-binomial model
# The glm.nb function is in the MASS library

glm3 = glm.nb(cdcflu~., data=flutrends)
summary(glm3)

post3 = predict(glm3, flutrends, se.fit=TRUE)
yhat3 = exp(post3$fit)
yhat.l3 = exp(post3$fit - 1.96*post3$se.fit)
yhat.u3 = exp(post3$fit + 1.96*post3$se.fit)


plot(flutrends$cdcflu, col='blue', pch=19)
lines(yhat3, col='red')
lines(yhat.l3, col='red', lty='dashed')
lines(yhat.u3, col='red', lty='dashed')

# Much better!


# Now let's try some out-of-sample prediction
pred3 = predict(glm3,newdata=flutrends2012, se.fit=TRUE)

y2012 = exp(pred3$fit)
y2012.l = exp(pred3$fit - 1.96*pred3$se.fit)
y2012.u = exp(pred3$fit + 1.96*pred3$se.fit)


# Plot the past and future predictions together
plot(flutrends$cdcflu, col='blue', pch=19, xlim=c(150, 215))
lines(yhat3, col='red')
lines(yhat.l3, col='red', lty='dashed')
lines(yhat.u3, col='red', lty='dashed')


# Now add the future predictions at the appropriate values on the time axis
lines(209:215, y2012, col='grey')
lines(209:215, y2012.l, col='grey', lty='dashed')
lines(209:215, y2012.u, col='grey', lty='dashed')
