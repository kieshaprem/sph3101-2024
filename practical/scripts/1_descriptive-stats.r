############# Practical ############# 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


# ++++++++ Question 1 ++++++++ #

# 1.	Populate the table 

## a) Age and CVD (Tips:  use the sd() function to calculate standard deviation, e.g., sd(chp$age[chp$cvd %in% 1]))

mean(chp$age[chp$cvd %in% 0])
mean(chp$age[chp$cvd %in% 1])
sd(chp$age[chp$cvd %in% 1])
sd(chp$age[chp$cvd %in% 0])


## b) BMI and CVD 




## c) LDL cholesterol and CVD 



## d) Gender and CVD 



## e) Ethnicity and CVD 



## f) Smoking status and CVD 



# The table1 package in R is a highly useful tool designed to create 
# descriptive summary statistics tables, often referred to as "Table 1" in 
# research papers, particularly in the fields of medicine, epidemiology, and 
# public health. These tables typically summarize the baseline characteristics 
# of study participants or subjects by different groups 
# (e.g., treatment vs. control groups).

library(table1)


