library(mosaic)
library(lattice)
library(lme4)

mtdna = read.csv('mtdna.csv', header=TRUE)
summary(mtdna)


boxplot(copy ~ tissue, data=mtdna)
boxplot(copy ~ factor(litter), data=mtdna)
boxplot(copy ~ factor(animal):factor(litter), data=mtdna)

hglm1 = glmer(copy ~ tissue + (factor(animal) | litter), data=mtdna, family=poisson)
ranef(hglm1)