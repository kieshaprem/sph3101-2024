############# Practical ############# 


head(chp)
table(chp$cvd)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


# ++++++++ Question 1 ++++++++ #

# 1.	Populate the table 

## a) Age and CVD (Tips:  use the sd() function to calculate standard deviation, e.g., sd(chp$age[chp$cvd %in% 1]))
mean(chp$age[chp$cvd %in% 1])
mean(chp$age[chp$cvd %in% 0])

t.test(chp$age~chp$cvd) 
sd(chp$age[chp$cvd %in% 1])
sd(chp$age[chp$cvd %in% 0])

## b) BMI and CVD 
t.test(chp$bmi~chp$cvd) 
sd(chp$bmi[chp$cvd %in% 1])
sd(chp$bmi[chp$cvd %in% 0])

## c) LDL cholesterol and CVD 
t.test(chp$ldl~chp$cvd) 
sd(chp$ldl[chp$cvd %in% 1])
sd(chp$ldl[chp$cvd %in% 0])

## d) Gender and CVD 
table(chp$gender,chp$cvd)
tab_gender_cvd = table(chp$female,chp$cvd)
dimnames(tab_gender_cvd) = list(c('male','female'),c('no CVD','CVD'))
tab_gender_cvd
apply(tab_gender_cvd,MARGIN = 2,FUN = function(x) round(x/sum(x)*100,2))



## e) Ethnicity and CVD 
tab_ethnicity_cvd = table(chp$ethnicity,chp$cvd)
tab_ethnicity_cvd
apply(tab_ethnicity_cvd,MARGIN = 2,FUN = function(x) round(x/sum(x)*100,2))


## f) Smoking status and CVD 
tab_smoke_cvd = table(chp$smoke,chp$cvd)
tab_smoke_cvd
apply(tab_smoke_cvd,MARGIN = 2,FUN = function(x) round(x/sum(x)*100,2))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


# The table1 package in R is a highly useful tool designed to create 
# descriptive summary statistics tables, often referred to as "Table 1" in 
# research papers, particularly in the fields of medicine, epidemiology, and 
# public health. These tables typically summarize the baseline characteristics 
# of study participants or subjects by different groups 
# (e.g., treatment vs. control groups).

library(table1)


