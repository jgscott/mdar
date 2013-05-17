#### Galapagos Islands example

# Load the libraries
library(mosaic)

# Import the gala.csv data set
plot(Species~Area, data=gala)

# Looks much better on a log-log scale
plot(log(Species) ~ log(Area), data=gala)

lm1 = lm(log(Species) ~ log(Area), data = gala)
abline(lm1)
summary(lm1)

# Now species versus elevation
plot(log(Species) ~ log(Elevation), data=gala)

lm2 = lm(log(Species) ~ log(Elevation), data = gala)
abline(lm2)
summary(lm2)

## The predictors are collinear
plot(log(Area) ~ log(Elevation), data=gala)


## Fit a multiple regression model.
## This fits a plane through the 3d point cloud!

lm3 = lm(log(Species)~log(Area) + log(Elevation), data=gala)

myboot = do(1000)*lm(log(Species)~log(Area) + log(Elevation), data=resample(gala))
sd(myboot)
summary(lm3)

## Show the 3d plot
## Pedagogical purposes only...
## This may not work on your computer.
library(rgl)
G3 = data.frame(LogElevation = log(gala$Elevation), LogArea = log(gala$Area), LogSpecies = log(gala$Species))
plot3d(G3, size=7)

### Now draw in the plane

lm.gala = lm(LogSpecies ~ LogElevation + LogArea, data=G3)

x1 = seq(min(G3$LogElevation), max(G3$LogElevation), length=20)
x2 = seq(min(G3$LogArea), max(G3$LogArea), length=20)
Xnew = expand.grid(x1,x2)
surface3d(x1,x2,predict.lm(lm.gala, data.frame(LogElevation = Xnew[,1], LogArea = Xnew[,2])), col="lightblue")



