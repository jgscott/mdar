library(mosaic)

# Load data
greenbuildings = read.csv("green.csv", header=TRUE)

summary(greenbuildings)

# Look at leasing_rate and rent
hist(greenbuildings$Rent,100)
hist(greenbuildings$leasing_rate,100)

# scrub those with less than 10% occupancy
# this strikes me as a weird situation and I don't know what to make of it
plot(leasing_rate ~ age, data=greenbuildings)
greenbuildings = subset(greenbuildings, leasing_rate >= 10)

# Define a revenue per square foot measure
greenbuildings$RevPSF = greenbuildings$Rent * greenbuildings$leasing_rate / 100

# relative measures of likely heating and cooling costs
# I assume that people cool with electricity and heat with gas
greenbuildings$cooling_costs = greenbuildings$cd_total_07 * greenbuildings$Electricity_Costs/100
greenbuildings$heating_costs = greenbuildings$hd_total07 * greenbuildings$Gas_Costs/100


# Start with a big model including a few interactions
lm1 = lm(RevPSF ~ log(size) + stories + age + renovated + class_a + class_b + amenities + net
	+ cluster_rent + Precipitation + cooling_costs + heating_costs
	+ net:cooling_costs + net:heating_costs
	+ green_rating + net:green_rating + green_rating:cluster_rent
	+ Precipitation:green_rating  + green_rating:cooling_costs + green_rating:heating_costs,
	data = greenbuildings)
summary(lm1)

# Check correlation
cor(greenbuildings$RevPSF, fitted(lm1))

# Stepwise selection
lmstep1 = step(lm1, direction='both')
summary(lmstep1)

# Decent fit
plot(fitted(lmstep1), greenbuildings$RevPSF)
abline(0,1)
cor(greenbuildings$RevPSF, fitted(lmstep1))


# A permutation test for green rating
perm1 = do(1000)*lm(RevPSF~log(size) + stories + age + class_a + class_b + 
  amenities + net + cluster_rent + Precipitation + cooling_costs + 
  heating_costs + shuffle(green_rating) + net:cooling_costs, data=greenbuildings)

# Check whether observed R^2 falls in the rejection region
hist(perm1$r.squared)
abline(v=0.5671, col='red')

hist(perm1$green_rating)
abline(v=1.369170, col='red')

# Conclusion: the green benefit to RevPSF looks like 1.369170 +- 2*0.423865
