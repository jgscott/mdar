library(lme4)
library(mosaic)
library(lmtest)
library(lattice)

# A good example of panel data
# In lme4 library
data(sleepstudy)

summary(sleepstudy)

xy1 = xyplot(Reaction~Days | Subject, data=sleepstudy)
plot(xy1)

summary(xy1)

# Let's again compare a linear model with interactions vs HLM

lm1 = lm(Reaction ~ Days * Subject, data=sleepstudy)

# Remember to include Days as a fixed effect too
# ("uncentered parametrization")
hlm1 = lmer(Reaction ~ Days + (1+Days | Subject), data=sleepstudy)


coef(hlm1)

re1 = ranef(hlm1, postVar=TRUE)
dotplot(re1)



batch1 = coef(lm1)[2] + c(0, coef(lm1)[20:36])
batch2 = coef(hlm1)$Subject[,2]

cbind(batch1, batch2)

