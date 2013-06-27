library(nnet)

ctg = read.csv("ctg.csv", header=TRUE)
summary(ctg)

ml1 = multinom(NSP ~ (. - DR), data=ctg)

# Step-down BIC
mystep1 = step(ml1, scope = ~ (. - DR), k = log(nrow(ctg)))

summary(mystep1)
# Make in-sample "predictions" and check the accuracy
predict(mystep1)

table(guess = predict(mystep1), truth = ctg$NSP)


#### Rescaling the columns of the design matrix

# Construct a design matrix without the intercept column
X = model.matrix(NSP ~ (. - DR - 1), data=ctg)

# Compute the column means and column sd's
colMeans(X)
apply(X, 2, sd)

# Center and scale the design matrix
X = scale(X)
colMeans(X)
apply(X, 2, sd)

# Reconstruct the data set
ctgscale = data.frame(NSP = ctg$NSP, X)

ml2 = multinom(NSP ~ ., data=ctgscale)

# Step-down BIC
mystep2 = step(ml2, scope=~., k = log(nrow(ctgscale)))
summary(mystep2)

# Make in-sample "predictions" and check the accuracy
predict(mystep2)

table(guess = predict(mystep2), truth = ctgscale$NSP)

