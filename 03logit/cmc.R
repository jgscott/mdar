library(nnet)

cmc = read.csv('cmc.csv', header=TRUE)

ml1 = multinom(Method ~ Age + factor(Educ), data = cmc)
summary(ml1)

# Make "predictions" for class probabilities on original data
pred1 = predict(ml1, cmc, type='probs')


# Put the actual outcomes and fitted class probabilities side by side
pred1 = cbind(cmc, pred1)