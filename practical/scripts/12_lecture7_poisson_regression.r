
############# In-class practical ############# 

library(tidyverse)

# Load the dataset
hh_phl <- read.csv("data/fHH1.csv",as.is = TRUE)
# View the first few rows
head(hh_phl)

############# Task 1: Variable of interest
# Explore the distribution of the variable of interest, number of household members, and describe it 
hist(hh_phl$total,xlab = 'Number of people in the household other than the head',ylab = 'Number of HH members',main = '',col='grey')

summary(hh_phl$total)
var(hh_phl$total)
# The mean number of people in a household is ______ (Var = ______). 


############# Task 2: Relationship with other variables 
# Explore the relationship of the number of household members with other variables (e.g., roof)
table(hh_phl$roof)
table(hh_phl$roof)/nrow(hh_phl)
# ______ household roofs are made from predominantly light and salvaged material. 

mean(hh_phl$total[hh_phl$roof %in% "Predominantly Strong Material"])
var(hh_phl$total[hh_phl$roof %in% "Predominantly Strong Material"])
mean(hh_phl$total[hh_phl$roof %in% "Predominantly Light/Salvaged Material"])
var(hh_phl$total[hh_phl$roof %in% "Predominantly Light/Salvaged Material"])
# The mean number of people in a house for houses with a roof made from predominantly strong material is ______ (Var = ______), 
# whereas houses with a roof made from predominantly light/salvaged material average ______ people (Var = ______). 

# Explore the relationship of the number of household members with other variables (e.g., location)
aggregate(x = hh_phl$tota, by = list(hh_phl$location),FUN = mean)
# Households in Visayas has the largest size, on average, with a mean of ______ in the household, 
# and the Davao Region has the smallest with a mean of ______

# Explore the relationship of the number of household members with other variables (e.g., age)
# Compare mean and variance of household size within each age group
hh_phl$age_group = cut(hh_phl$age ,breaks = 5*(3:14))
hh_age =data.frame(aggregate(x = hh_phl$tota, by = list(hh_phl$age_group),FUN = length))
colnames(hh_age) = c('Age group', 'n')
hh_age$Mean = aggregate(x = hh_phl$tota, by = list(hh_phl$age_group),FUN = mean)[,2]
hh_age$Var = aggregate(x = hh_phl$tota, by = list(hh_phl$age_group),FUN = var)[,2]
hh_age

par(mfrow=c(4,3))

for(a in levels(hh_phl$age_group))
{
  hist(hh_phl$total[hh_phl$age_group %in% a],xlab = 'Number of people in the household other than the head',ylab = 'Number of HH members',main = a,col='grey')
}
par(mfrow=c(1,1))

sumStats <- hh_phl %>% group_by(age) %>% 
  summarise(mntotal = mean(total),
            logmntotal = log(mntotal), n=n())

plot(x=sumStats$age,sumStats$logmntotal,xlab = 'Age of the head of household', ylab = 'Log of the mean household sizes')
# What kind of relationship with age do you observe?




############# Task 3: Perform Poisson regression
m1 = glm(total ~ age, family = poisson, data = hh_phl)
summary(m1)

# How can the coefficient estimates be interpreted?

# CI for betas using profile likelihood
confint(m1)
exp(confint(m1))

# m0 is the null/reduced model
m0 <- glm(total ~ 1, family = poisson, data = hh_phl)
drop_in_dev <- anova(m0, m1, test = "Chisq")
drop_in_dev



# m2 is a second-order model
hh_phl$age2 = hh_phl$age*hh_phl$age
m2 = glm(total ~ age + age2, family = poisson, data = hh_phl)
summary(m2)


drop_in_dev <- anova(m1, m2, test = "Chisq")
drop_in_dev


# Add another covariate "location" to the model and test for its significance 


# m3 =


