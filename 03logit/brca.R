library(mosaic)
library(multcomp)

#brca = read.csv('brca.csv', header=TRUE)
summary(brca)

# Recast radiologist as a factor
brca$radiologist = factor(brca$radiologist)

# Could name all the variables explicitly
glm1 = glm(recall ~ radiologist + age5059 + age6069 + age70plus + familyhistory + biopsurg + symptoms + premeno + postmenohormone + postmenounknown + previousmammogram + density1 + density3 + density4, data=brca, family=binomial)

# Or could name the ones we don't want
glm1 = glm(recall ~ radiologist + ( . - cancer), data=brca, family=binomial)

# Are there are radiologists who seem more conservative than others?
summary(glm1)

# Test a specific contrast matrix
# This is the contrast for radiologist 34 minus radiologist 89
contr1 = matrix( c(0,1,0,-1,rep(0,14)), 1)
glht1 = glht(glm1, linfct = contr1)
summary(glht1)

# Can also name contrasts symbolically
glht1 = glht(glm1, linfct = c("radiologist34 - radiologist89 = 0", "radiologist34 - radiologist66 = 0"))
summary(glht1)

# Or name a factor and a method for multiplicity correction
glht1 = glht(glm1, linfct = mcp(radiologist = "Tukey"))
summary(glht1)

?glht # for other ways


# Now fit a model for cancer, given recall status and radiologist
glm2 = glm(cancer ~ . , data=brca, family=binomial)

glm3 = glm(cancer ~ radiologist + recall + recall:radiologist + . , data=brca, family=binomial)
summary(glm3)


# Are there any risk factors that should be given
# more weight in the recall decision?
summary(glm2)



### Bonus stuff

# Estimate probability of recall for average patient
# with radiologist 13 (the median recall rate).
# Check this with mean(recall~radiologist, data=brca)
p.recall = exp(-2.15)/{1+exp(-2.15)}

p.cancer.giv.norecall = exp(-5)/{1+exp(-5)}
p.cancer.giv.recall = exp(-5 + 2.35)/{1+exp(-5 + 2.35)}

# true negative
# Joint probability of no cancer and no recall
pTN = (1-p.recall) * (1-p.cancer.giv.norecall)

# False positive: no cancer and recall
# = prob(recall) * {1- prob(cancer | recall)}
pFP = p.recall * (1-p.cancer.giv.recall)

# True positive
pTP = p.recall * p.cancer.giv.recall

# False negative
pFN = {1-p.recall} * p.cancer.giv.norecall

pFN; pTN; pFP; pTP
