############# In-class practical ############# 

babies = read.csv('data/babies.csv')

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

## Plot the relationship between birth weight and gestational weeks
# which do you think is the independent and dependent variable? 
# independent variable: gestational weeks
# dependent variable: birth weight

plot(babies$bweight ~ babies$gestwks, 
     xlab='Gestational weeks', ylab = 'Birth weight (in kg)')
cor.test(babies$bweight , babies$gestwks)
# 0.74 (95% CI: 0.70-0.77)

## Describe the relationship
# - moderate to strong positive correlation, 0.74 (95% CI: 0.70-0.77), between gestational weeks and birth weight
# describing the scatter plot
# The horizontal axis is gestation, ranging from 25 to 45 weeks. 
# The vertical axis is birthweight, ranging from 0 to 5 kilograms. 
# The points follow a general upward trend. There are only two points below about 26 weeks.
# The density of points increases as gestational weeks increase. 
# A dense cloud of points lie between about 37 and 42 weeks. 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

##  Use linear regression to investigate the relationship between birth weight 
##  and gestational weeks.  
?lm()
mod = lm(babies$bweight ~ babies$gestwks)
summary(mod)

# linear regression diagnostics
plot(mod)

## Obtain the equation for the regression of birth weight on gestational weeks. 
# intercept: -4.87
# slope: 0.207
  
# Birth weight = -4.87 + 0.207 x weeks of gestation


## Now you can test the hypothesis that slope = 0. 
# t-stat: 27.61
# p-value: <0.001
# There is sufficient evidence to reject the null hypothesis that slope = 0

## Describe the relationship




# Add the fiited regression line to the graph  

plot(babies$bweight ~ babies$gestwks, xlab='Gestational weeks', ylab = 'Birth weight (in kg)')
coefficients(mod)[1]
abline(a= coefficients(mod)[1],b=coefficients(mod)[2],col='red',lwd=2)






