library(mosaic)
library(contrast)
library(lattice)
#springbok = read.csv("springbok.csv", header=TRUE)

names(springbok)
summary(springbok)

# Reset the baseline year
springbok$timesince1990 = springbok$year - 1990

# Recast site as a factor
springbok$site = factor(springbok$site)

# EDA
plot(counts ~ site, data=springbok)
plot(counts ~ hourfromnoon, data=springbok)

# What about trends?
plot(counts ~ timesince1990, data=springbok)
plot(counts ~ jitter(timesince1990), data=springbok)
boxplot(counts~timesince1990, data=springbok)

# Doesn't appear to be an overall trend...
# What about within sites?
xyplot(counts ~ timesince1990  | site, data=springbok)

# Perhaps an aggregation paradox

# Let's model counts with main effects for site
glm1 = glm(counts ~ site, data = springbok, family=poisson)

# Every site coefficient looks significant because
# site 1 is the baseline, and has high counts overall
summary(glm1)

# Can get inference for pairwise differences using contrasts
contrast(glm1, list(site='2'), list(site = '6'))


# And now with year-site interactions
glm2 = glm(counts ~ site*timesince1990, data = springbok, family=poisson(link="log"))
summary(glm2)
anova(glm2, test="LRT")

# Looks important!
# Suppose we wanted to add the fit to the data plots

springbok$fit2 = fitted(glm2)
par(mfrow=c(3,4))
for(i in 1:12)
{
	plot(counts ~ timesince1990, data=subset(springbok, site==i), main=i)
	points(fit2 ~ timesince1990, data=subset(springbok, site==i), pch=19, col='blue')
}



# What about time-of-day effects within sites?
xyplot(counts ~ hourfromnoon  | site, data=springbok)

# Seems small if present...
# And functional form not clear.
# Is it symmetric about the hot time of day?
# Is it linear over the course of the day?
# Might also be obscured by aggregation across years.

# Just a quick test:
glm3 = glm(counts ~ site + timesince1990 + site:timesince1990 + site:hourfromnoon, data=springbok, family=poisson(link="log"))
summary(glm3)

# This is the analogue of the F test
anova(glm3, test="LRT")
# Marginal improvement in deviance from the site-year interaction is small

