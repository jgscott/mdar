
springbok = read.csv("http://jgscott.github.com/SSC325/data/springbok.csv", header=TRUE)

names(springbok)
summary(springbok)
# Reset the baseline year
springbok$timesince1990 = springbok$YEAR - 1990

# Recast SITEI as a factor
springbok$SITEI = factor(springbok$SITEI)

# EDA
plot(COUNTS ~ SITEI, data=springbok)
plot(COUNTS ~ HourFromNoon, data=springbok)

# What about trends?
plot(COUNTS ~ timesince1990, data=springbok)
plot(COUNTS ~ jitter(timesince1990), data=springbok)


# Doesn't appear to be an overall trend...
# What about within sites?
library(lattice)
xyplot(COUNTS ~ timesince1990  | SITEI, data=springbok)
bwplot(COUNTS ~ factor(YEAR) | SITEI, data=springbok)

# Perhaps an aggregation paradox

# Let's model COUNTS with main effects for SITEI
glm1 = glm(COUNTS ~ SITEI, data = springbok, family=poisson(link="log"))

# And now with year-SITEI interactions
glm2 = glm(COUNTS ~ SITEI*timesince1990, data = springbok, family=poisson(link="log"))
summary(glm2)
anova(glm2, test="LRT")

# Looks important!
# Suppose we wanted to add the fit to the data plots

springbok$fit2 = fitted(glm2)
par(mfrow=c(3,4))
for(i in 1:12)
{
	plot(COUNTS ~ timesince1990, data=subset(springbok, SITEI==i), main=i)
	points(fit2 ~ timesince1990, data=subset(springbok, SITEI==i), pch=19, col='blue')
}



# What about time-of-day effects within SITEIs?

par(mfrow=c(4,3))
for(i in 1:12)
{
	plot(COUNTS ~ hourfromnoon, data=subset(springbok, SITEI==i), main=i)
}
# Seems small if present...
# And functional form not clear.
# Is it symmetric about the hot time of day?
# Is it linear over the course of the day?
# Might also be obscured by aggregation across years.

# Just a quick test:
glm3 = glm(COUNTS ~ SITEI*timesince1990 + SITEI:hourfromnoon, data=springbok, family=poisson(link="log"))
anova(glm3, test="LRT")

# Marginal improvement in deviance from the SITEI-year interaction is small
summary(glm3)