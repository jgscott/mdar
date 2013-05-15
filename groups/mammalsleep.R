# Load these packages
# Will need to install them the first time
library(mosaic)
library(graphics)
library(faraway)


### Load in the data on mammalian sleep patterns from the library faraway

data(mammalsleep, package="faraway")

##############################
# Some simple EDA
##############################

# Look at the data
# The NA's are missing data
mammalsleep

# Compute some summary statistics
summary(mammalsleep)

mean(mammalsleep$dream)
sd(mammalsleep$dream)


# Histogram of the outcome variable
hist(mammalsleep$dream)
hist(mammalsleep$dream, breaks=20)



# a table of the "danger" index...
summary(mammalsleep$danger)

# Uh oh, R thinks "danger" is a quantitative variable!
# Inform it of its mistake :-)
# Recast the variable as a factor, i.e. something categorical

mammalsleep$danger = factor(mammalsleep$danger)

# Now try again... we get a table of category counts
summary(mammalsleep$danger)


# Look at a boxplot
boxplot(dream~danger, data=mammalsleep)

# Try a stripchart
stripchart(dream~danger, data=mammalsleep)

# Now a vertical stripchart
stripchart(dream~danger, data=mammalsleep,
	vertical=TRUE)

# Now with horizontal jitter added to the points
# Try this a few different times to get a feel for what 
# method="jitter" is doing
stripchart(dream~danger, data=mammalsleep,
	method="jitter", vertical=TRUE)


# Now compute the groupwise means
mean(dream~danger,data=mammalsleep)

# And the groupwise the groupwise standard deviations
sd(dream~danger,data=mammalsleep)



# Now let's use R's "lm" command, which will be
# a major workhorse for the entire course

lm1 = lm(dream~danger, data=mammalsleep)

# Extract the coefficients (two ways)
coef(lm1)
lm1$coefficients

# How do we interpret these numbers?
# Not the same as the group means
# (Look at them side by side)
GroupMeans = mean(dream~factor(danger),data=mammalsleep)$S
rbind(GroupMeans, lm1$coefficients)


# We could easily change the baseline case from danger=1 to danger=5
# by reordering the factor

mammalsleep$danger.reord = factor(mammalsleep$danger, levels=5:1)

# Now fit a new model versus the different factor
lm2 = lm(dream~danger.reord, data=mammalsleep)
GroupMeans2 = mean(dream~factor(danger.reord),data=mammalsleep)$S
rbind(GroupMeans2, lm2$coefficients)


# Compare the fitted values for both linear models

cbind(fitted(lm1), fitted(lm2))







#################
### And now some pretty plots
#################



par(mar=c(4,4,1,1), mgp=c(3,1,0))
stripchart(dream~danger,method="jitter",data=mammalsleep, vertical=TRUE, frame.plot=FALSE,
	ylab="Dreaming hours per night", main="", xlab="Predation Index (5 = most in danger)",
	pch=21, bg='lightgrey', cex=1.3, axes=FALSE, cex.lab=1.2)
axis(2, tick=TRUE, las=1, cex.axis=1.3)
axis(1, tick=FALSE, las=1, cex.axis=1.3)


# Add the grand means

GroupMeans = mean(dream~factor(danger),data=mammalsleep)$S
abline(h=mean(mammalsleep$dream), col="darkgreen", lty="dashed")

# Now add the group-wise means
points(1:5, GroupMeans, pch=22, bg="blue", cex=2.0)



lm1 = lm(dream~factor(danger), data=mammalsleep)


# plot the individual dreaming hours with species named

myind = which(!is.na(mammalsleep$dream))
myind = myind[order(mammalsleep$predation[myind])]



mycols=c("blue", "green", "darkgrey", "darkblue", "red")

N = length(myind)
yy = mammalsleep$dream[myind]
mynames = row.names(mammalsleep)[myind]

gmu = mean(yy)

par(mar = c(8,4,0,0))
plot(1:N, yy, pch=21,bg=mycols[mammalsleep$predation[myind]], bty='n', axes=FALSE, xlab='', ylab="Dreaming hours per night", main='', type='n')
axis(2, cex.axis=1.1, las=1)
axis(1, at=1:N, labels=mynames, las=3, cex.axis=0.8, tick=FALSE, pos=0)


## Grand means
for(i in 1:N)
{
	lines(c(i,i), c(yy[i],gmu), lty='dotted', col=grey(0.5))
}
abline(h=gmu,col="black",lwd=2.5)
points(1:N, yy, pch=21,bg=mycols[mammalsleep$predation[myind]], cex=1.5)


## Group means
for(i in 1:5)
{
	lines(range((1:N)[mammalsleep$predation[myind]==i]),   c(GroupMeans[i], GroupMeans[i]), col=mycols[i], lwd=2.5)
}
for(i in 1:N)
{
	#abline(v=i,col=grey(0.95),lty='dotted')
	lines(c(i,i), c(yy[i],GroupMeans[mammalsleep$predation[i]]), lty='dotted', col=grey(0.5))
}
points(1:N, yy, pch=21,bg=mycols[mammalsleep$predation[myind]], cex=1.5)


## Fitted values
abline(h=gmu,col="black",lwd=1.0)
points(1:N, yy, pch=21,col=grey(0.8), bg=grey(0.95), cex=1.0)
for(i in 1:5)
{
	lines(range((1:N)[mammalsleep$predation[myind]==i]),   c(GroupMeans[i], GroupMeans[i]), col=mycols[i], lwd=2.5)
}
for(i in 1:N)
{
	points(i, GroupMeans[mammalsleep$predation[myind][i]], pch=23,bg=mycols[mammalsleep$predation[myind]][i], cex=1.5)
	lines(c(i,i), c(gmu,GroupMeans[mammalsleep$predation[myind][i]]), lty='dotted', col=grey(0.5))
}

