############# In-class practical ############# 
options(scipen=999)
load('data/both.RData')

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

# quick look at the data
head(both)

plot(both$ASR~both$Year, pch = 1,xlab = 'Year',ylab = 'ASR',col='black')


m_male = lm(both$ASR[both$Female %in% 0]~both$Year[both$Female %in% 0])
m_female = lm(both$ASR[both$Female %in% 1]~both$Year[both$Female %in% 1])

plot(both$ASR[both$Female %in% 0]~both$Year[both$Female %in% 0], pch = 16,xlab = 'Year',ylab = 'ASR',col='cornflowerblue')
points(both$ASR[both$Female %in% 1]~both$Year[both$Female %in% 1],pch=18, col = 'salmon')
lines(m_male$fitted.values~both$Year[both$Female %in% 0],lwd=1.5,col='cornflowerblue')
lines(m_female$fitted.values~both$Year[both$Female %in% 1],lwd=1.5,col='salmon')


summary(m_male)
summary(m_female)

# Construct a multiple linear regression model with the interaction term

m1 = lm(both$ASR~both$Year.centered * both$Female.factor)
summary(m1)

# ANOVA (Type I)
m1 = lm(both$ASR~both$Year.centered + both$Female.factor + both$Year.centered:both$Female.factor)
summary(m1)
anova(m1)


m2 = lm(both$ASR~both$Year.centered.Female + both$Female.factor + both$Year.centered)
summary(m2)
anova(m2)

# variance inflation factor (VIF)
library(car)
vif(m1)



############# In-class practical ############# 

load('data/tcData.RData')

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

# quick look at the data
head(tcData)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# Task 0: Construct a multiple linear regression model
mod = lm(tcData$chol ~ tcData$age+ tcData$bmi.1+ tcData$bmi.2+ tcData$gender+ tcData$smoker)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# Task 1: Interpret the model output
summary(mod)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# Task 2: Generate the variance inflation factors for the model built. 
# Report the variable(s) with a collinearity problem and the corresponding variance inflation factor(s). 

library(car)
vif(mod)

round(cov2cor(vcov(mod)),3)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# Task 3: Fit two multiple linear regression models similar to Task 0 where chol is the outcome and the following variables 
# are still predictors: age, gender and smoker, BUT bmi.1 and bmi.2 are included as predictors in two separate models.
# Based on the models you have fitted, compare these findings with those in Task 1. 

mod1 = lm(tcData$chol ~ tcData$age+ tcData$bmi.1+ tcData$gender+ tcData$smoker)
summary(mod1)

mod2 = lm(tcData$chol ~ tcData$age+ tcData$bmi.2+ tcData$gender+ tcData$smoker)
summary(mod2)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# Task 4: Generate the variance inflation factors for the model built in Task 3. 
# Report the variable(s) with a collinearity problem and the corresponding variance inflation factor(s). 

vif(mod1)

vif(mod2)





