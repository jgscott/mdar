library(mlbench)
library(nnet)
library(randomForest)
library(foreach)
library(doMC)
registerDoMC(4)

data(Satellite)
head(Satellite)
summary(Satellite$classes)

dim(Satellite)
n_total = nrow(Satellite)
n_train = 4000
n_test = n_total - n_train

nsplits = 25
crossval = foreach(i = 1:nsplits, .combine=rbind) %dopar% {

  # Split the data into training and test sets
  training_cases = sample(1:n_total, n_train)
  training_set = Satellite[training_cases,]
  testing_set = Satellite[-training_cases,]
  
  # Multinomial logit model
  model1 = multinom(classes ~ ., data= training_set)
  pred1 = predict(model1, testing_set)  # predict on a hold-out set
  table1 = xtabs( ~ testing_set$classes + pred1)  # form the confusion matrix
  error_rate1 = 1 - sum(diag(table1))/n_test  # compute some metric of accuracy
  
  # Random forests
  model2 = randomForest(classes ~ ., data= training_set)
  pred2 = predict(model2, testing_set)  # predict on a hold-out set
  table2 = xtabs( ~ testing_set$classes + pred2)
  error_rate2 = 1 - sum(diag(table2))/n_test  # compute some metric of accuracy
  
  c(error_rate1, error_rate2)
  
}

