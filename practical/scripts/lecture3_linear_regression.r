############# In-class practical ############# 

load('data/cvdData.RData')

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

# quick look at the data
head(cvdData)

# Construct a multiple linear regression model
m1 = (lm(cvdData$ldl ~ cvdData$bmi + cvdData$age + factor(cvdData$race) + cvdData$gender))
summary(m1)

# calculate the 95% confidence intervals of the parameters
confint(m1)

# regression diagnostics
par(mfrow=c(2,2))
plot(m1)


# Influential plot 
# Studentised residual vs leverage with Cook’s distance
# You will need the car package for this
library(car)
par(mfrow = c(1,1))
influencePlot(m1, id=list(method="noteworthy",n=2))


# Remove potentially influential observations 
cvdDrop = cvdData[-c(41,56,123,146,179),]

# Rerun the regression
m2 = (lm(cvdDrop$ldl ~ cvdDrop$bmi + cvdDrop$age + cvdDrop$race+ cvdDrop$gender))
summary(m2)
confint(m1)

