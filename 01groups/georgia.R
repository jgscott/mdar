library(mosaic)

# Use RStudio's Import Dataset button, or...
georgia = read.csv("georgia.csv", header=TRUE)
summary(georgia)

# Define the undercount variable as a percentage
georgia$ucount = 100*(georgia$ballots - georgia$votes)/georgia$ballots

# Undercount appears slightly lower in urban counties
boxplot(ucount ~ urban, data=georgia)

# And slightly higher in counties with more poverty
boxplot(ucount ~ poor, data=georgia)

# Differences in voting equipment?
boxplot(ucount ~ equip, data=georgia)

# Start with a two-way ANOVA model with main effects only
lm1 = lm(ucount~urban+poor,data=georgia)
summary(lm1)

# Notice there are few poor, urban counties
# This contra-indicates the use of an interaction
xtabs(~urban+poor, data=georgia)


##################
# Model uncertainty
##################
# Adjusting for the urban and poor variables, is there an equipment effect?

lm2 = lm(ucount ~ urban + poor + equip,data=georgia)
summary(lm2)

# What kinds of coefficients would we see
# if we "shuffled the cards" for equipment?

lm(ucount ~ urban + poor + shuffle(equip), data=georgia)

# A permutation test
perm1 = do(1000)*lm(ucount ~ urban + poor + shuffle(equip), data=georgia)
hist(perm1$r.squared)

summary(lm2)
prop(perm1$r.squared > 0.2368)

# This is the nonparametric version of an F test
anova(lm2)

