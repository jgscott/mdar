# Data from http://www.stat.columbia.edu/~gelman/arm/
library(lme4)


polls = read.csv("polls.csv", header=TRUE)
polls$edu = factor(polls$edu, levels=c("NoHS", "HS", "SomeColl", "Bacc"))

# glm doesn't blow up...
glm1 = glm(bush~black + female + state, family=binomial(link="logit"), data=polls)

# But does give nonsense answers for some of the coefficients
summary(glm1)

summary(polls$state, 50)


# A hierarchical models works well here... shrink across states

hglm1 = glmer(bush ~ black + female + (1 | state), family=binomial(link="logit"), data=polls)

coef(hglm1)
r = ranef(hglm1, postVar = TRUE)
d = dotplot(r)
names(d)
plot(d$state)
summary(hglm1)


hglm2 = glmer(bush ~ black + female + (1 | state) + (1 | edu), family=binomial(link="logit"), data=polls)
anova(hglm1, hglm2)
r = ranef(hglm2, postVar = TRUE)
d = dotplot(r)
names(d)
plot(d$state)
plot(d$edu)

hglm3 = glmer(bush ~ black + female + edu + (1 | state) + (female|edu) , family=binomial(link="logit"), data=polls)