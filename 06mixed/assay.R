library(mosaic)
library(lattice)

assay = read.csv('assay.csv', header=TRUE)

summary(assay)
assay$ubw = assay$uterus/assay$weight


xtabs(~factor(ZM) + factor(EE), data=assay)

assayZM = subset(assay, EE==3)
assayEE = subset(assay, ZM==0)

bwplot(ubw ~ factor(EE) | protocol, data=assayEE)
bwplot(ubw ~ factor(EE) | lab, data=assayEE)

bwplot(ubw ~ factor(EE) | lab, data=subset(assayEE, protocol=="A"))
bwplot(ubw ~ factor(EE) | lab, data=subset(assayEE, protocol=="B"))
bwplot(ubw ~ factor(EE) | lab, data=subset(assayEE, protocol=="C"))
bwplot(ubw ~ factor(EE) | lab, data=subset(assayEE, protocol=="D"))


lm1 = lm(ubw ~ factor(EE), assayEE)
lm1a = lm(ubw ~ factor(EE), data=subset(assayEE, protocol=="A"))

lm2 = lm(ubw ~ factor(EE)*protocol, data=assayEE)

bwplot(ubw ~ factor(ZM) | protocol, data=assayZM)