library(mosaic)

rxntime = read.table("rxntime.txt", header=TRUE, sep="\t")
rxntime$Subject = factor(rxntime$Subject)
lm1 = lm(PictureTarget.RT ~ Littered + FarAway + Littered:FarAway, data=rxntime)

boot1 = do(1000)*{
  lm(PictureTarget.RT ~  Littered + FarAway + Littered:FarAway, data=resample(rxntime))
}
head(boot1)

# More traditional syntax using a for loop
NMC = 1000
boot2 = matrix(0, nrow=NMC, ncol=length(coef(lm1)))
for(i in 1:NMC) {
  lmboot = lm(PictureTarget.RT ~ Littered + FarAway + Littered:FarAway, data=resample(rxntime))
  boot2[i,] = coef(lmboot)
}
head(boot2)
colnames(boot2) = names(coef(lm1))  # This makes computer scientists crazy
head(boot2)

# The foreach package defines a for loop with a return value
# Can be used for embarrassingly parallel computations
library(foreach)
library(doMC)
registerDoMC(cores=4)

boot2 = foreach(i = 1:NMC, .combine=rbind) %dopar% {
  lmboot = lm(PictureTarget.RT ~ Littered + FarAway + Littered:FarAway, data=resample(rxntime))
  coef(lmboot)
}
head(boot2)

# Resampling subjects
boot3 = matrix(0, nrow=NMC, ncol=length(coef(lm1)))
subject_levels = levels(rxntime$Subject)
nsubjects = length(subject_levels)
for(i in 1:NMC) {
  # Construct a data set by resampling subjects
  bootstrapped_sample = NULL
  mysubjects = mosaic::resample(subject_levels)
  for(j in seq_along(mysubjects)) {
    data_subjectj = subset(rxntime, Subject == mysubjects[j])
    bootstrapped_sample = rbind(bootstrapped_sample, data_subjectj)
  }

  # Fit the model
  lmboot = lm(PictureTarget.RT ~ Littered + FarAway + Littered:FarAway, data=bootstrapped_sample)
  boot3[i,] = coef(lmboot)
}
colnames(boot3) = names(coef(lm1))
head(boot3)

hist(boot3[,2])
