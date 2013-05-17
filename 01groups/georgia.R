library(mosaic)

# Use RStudio's Import Dataset button, or...
#georgia = read.csv("georgia.csv", header=TRUE)
names(georgia)
summary(georgia)

# Define the undercount variable as a percentage
georgia$ucount = 100*(georgia$ballots - georgia$votes)/georgia$ballots

# Undercount appears slightly lower in urban counties
boxplot(ucount ~ urban, data=georgia)

# And slightly higher in counties with more poverty
boxplot(ucount ~ poor, data=georgia)

tally(~urban+poor, data=georgia)

# Are these due to differences in voting equipment?
boxplot(ucount ~ equip, data=georgia)

# Start with a two-way ANOVA model with main effects only
lm1 = lm(ucount~urban+poor,data=georgia)
coef(lm1)

# Question 1: Describe your uncertainty about the "urban" and "poor" effect sizes

# Question 2: Adjusting for the urban and poor variables, is there an equipment effect?
