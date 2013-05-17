library(mosaic)
#kidney = read.csv("kidney.csv", header=TRUE)

plot(Tot~age, data=kidney)

# Feel free to play with the plot defaults
plot(Tot~age, data=kidney,
	pch=19, col='grey', bty='n',
	ylab="Kidney score", xlab="Age")

lm1 = lm(Tot~age, data=kidney)
abline(lm1, lwd=2, col='blue')

# Bootstrapping a few times by hand
myboot = do(1000)*lm(Tot~age, data=resample(kidney))
hist(myboot)
sd(myboot)
confint(myboot)

# Compare with the parametric standard errors
summary(lm1)
confint(lm1)




##### ##### ##### 
##### Optional:
##### For those with some programming experience
##### here is bootstrapping done in a for loop.
##### ##### #####



nsamples = 1000
savedsamples = matrix(0, nrow=1000, ncol=2)
for(i in 1:1000)
{
	boot = resample(kidney)
	lmboot = lm(Tot~age, data=boot)
	abline(lmboot, col=rgb(5,5,5,5, maxColorValue=256))
	savedsamples[i,] = coef(lmboot)
}
abline(lm1, lwd=2, col='blue')

# Look at the sampling distributions
hist(savedsamples[,1])
hist(savedsamples[,2])



##### ##### ##### 
##### Optional: geek out with R plotting commands
##### ##### #####

# Take subsamples of the data
N = nrow(kidney)
samp1 = unique(c(14,sample(1:N, floor(N/2))))
samp2 = unique(c(14,setdiff(1:N, samp1)))

# Sample 1
plot(Tot~age, data=kidney, subset=samp1,
	pch=19, col='chartreuse3', bty='n',
	xlim=range(kidney$age),
	ylim=range(kidney$Tot), 
	ylab="Kidney score", xlab="Age")
lm1 = lm(Tot~age, data=kidney, subset=samp1)
abline(lm1, lwd=2, col='blue')

points(Tot~age, data=kidney[14,], pch=8, col='chartreuse3', cex=1.5)
points(55, sum(coef(lm1)*c(1,55)), pch=8, col='blue', cex=1.5)

#Sample 2
plot(Tot~age, data=kidney, subset=samp2,
	pch=19, col='chartreuse3', bty='n',
	xlim=range(kidney$age),
	ylim=range(kidney$Tot), 
	ylab="Kidney score", xlab="Age")
lm2 = lm(Tot~age, data=kidney, subset=samp2)
abline(lm2, lwd=2, col='blue')
points(Tot~age, data=kidney[14,], pch=8, col='chartreuse3', cex=1.5)
points(55, sum(coef(lm2)*c(1,55)), pch=8, col='blue', cex=1.5)

# The whole data set
plot(Tot~age, data=kidney,
	pch=19, col='chartreuse3', bty='n',
	xlim=range(kidney$age),
	ylim=range(kidney$Tot), 
	ylab="Kidney score", xlab="Age")
lm3 = lm(Tot~age, data=kidney)
abline(lm3, lwd=2, col='blue')
points(Tot~age, data=kidney[14,], pch=8, col='chartreuse3', cex=1.5)
points(55, sum(coef(lm3)*c(1,55)), pch=8, col='blue', cex=1.5)

