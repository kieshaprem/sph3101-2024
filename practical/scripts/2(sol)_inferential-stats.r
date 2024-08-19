############# Analyse your data ############# 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #



## Data analysis : 2 sample t-test (to compare two population means)

# Is gender associated with levels of LDL cholesterol?
t.test(chp$ldl~chp$female) 

# Is the LDL-gender association confounded by smoking status?

# ++++++++ In-class exercise ++++++++ #
# Is there a difference in the mean LDL values between smokers and non-smokers?
t.test(chp$ldl[chp$smoker %in% 0]~chp$female[chp$smoker %in% 0]) 
# p-value = 0.9488

t.test(chp$ldl[chp$smoker %in% 1]~chp$female[chp$smoker %in% 1]) 
# p-value = 0.5946

table(chp$gender,chp$smoker)
t.test(chp$ldl~chp$smoker) 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

# Data analysis : Chi-square/Fisher’s exact test (association between two categorical variables)

# Is the gender associated with smoking status?
tab_gender_smoker = table(chp$female,chp$smoker)
dimnames(tab_gender_smoker) = list(c('male','female'),c('non-smoker','smoker'))

tab_gender_smoker
# to get % in each combinations: use the "apply" function to compute column percentages
apply(tab_gender_smoker,MARGIN = 2,FUN = function(x) round(x/sum(x)*100,2))

chisq.test(tab_gender_smoker)
fisher.test(tab_gender_smoker)

# Data analysis : Investigating confounding effect

# Is the LDL-gender association confounded by smoking status?

t.test(chp$ldl[chp$smoker %in% 0]~chp$female[chp$smoker %in% 0]) 
t.test(chp$ldl[chp$smoker %in% 1]~chp$female[chp$smoker %in% 1]) 



# Data analysis : Correlation (linear association between two continuous variables)

# Is age associated with levels of LDL cholesterol?
cor.test(chp$ldl,chp$age)
# Scatter plot of LDL cholesterol and age
plot(chp$ldl~chp$age, 
     ylab = "LDL Cholesterol (mmol/L)",xlab = 'Age (years)')
plot(chp$ldl[chp$smoker %in% 1]~chp$age[chp$smoker %in% 1], 
     ylab = "LDL Cholesterol (mmol/L)",xlab = 'Age (years)')
points(chp$ldl[chp$smoker %in% 0]~chp$age[chp$smoker %in% 0],col = 'steelblue') 
     

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


# ++++++++ In-class exercise ++++++++ #

# Is the BMI-CVD association confounded by smoking status?

tab_overweight_cvd_nonsmoker = table(chp$overweight[chp$smoker %in% 0],chp$cvd[chp$smoker %in% 0]) 
dimnames(tab_overweight_cvd_nonsmoker) = list(c('Not overweight','Overweight'),c('no CVD','CVD'))
tab_overweight_cvd_nonsmoker
chisq.test(tab_overweight_cvd_nonsmoker)

tab_overweight_cvd_smoker = table(chp$overweight[chp$smoker %in% 1],chp$cvd[chp$smoker %in% 1]) 
dimnames(tab_overweight_cvd_smoker) = list(c('Not overweight','Overweight'),c('no CVD','CVD'))
tab_overweight_cvd_smoker

chisq.test(tab_overweight_cvd_smoker)

table(chp$ldl[chp$smoker %in% 1],chp$female[chp$smoker %in% 1])


############# Practical ############# 

# populate table 1 inferential tests 

# ++++++++ Question 1 ++++++++ #

# 1.	Populate the t-test and pvale table 

## a) Age and CVD 
t.test(chp$age~chp$cvd) 

## b) BMI and CVD 
t.test(chp$bmi~chp$cvd) 

## c) LDL cholesterol and CVD 
t.test(chp$ldl~chp$cvd) 


## d) Gender and CVD 
tab_gender_cvd = table(chp$gender,chp$cvd)
tab_gender_cvd
chisq.test(tab_gender_cvd)


## e) Ethnicity and CVD 
tab_ethnicity_cvd = table(chp$ethnicity,chp$cvd)
tab_ethnicity_cvd
chisq.test(tab_ethnicity_cvd)

## f) Smoking status and CVD 
tab_smoke_cvd = table(chp$smoke,chp$cvd)
tab_smoke_cvd
chisq.test(tab_smoke_cvd)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

# ++++++++ Question 2 ++++++++ #

# 2.	Is BMI (being overweight) associated with CVD? 
# Populate your results in the table.
tab_overweight_cvd = table(chp$overweight,chp$cvd)
dimnames(tab_overweight_cvd) = list(c('Not overweight','Overweight'),c('no CVD','CVD'))
tab_overweight_cvd

apply(tab_overweight_cvd,MARGIN = 2,FUN = function(x) round(x/sum(x)*100,2))
chisq.test(tab_overweight_cvd)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


# ++++++++ Question 3 ++++++++ #

# 3.	Is smoking associated with BMI (being overweight)? 
# Populate your results in the table.

tab_smoke_overweight = table(chp$smoker,chp$overweight)
dimnames(tab_smoke_overweight) = list(c('non-smoker','smoker'),c('Not overweight','Overweight'))
tab_smoke_overweight

apply(tab_smoke_overweight,MARGIN = 2,FUN = function(x) round(x/sum(x)*100,2))
chisq.test(tab_smoke_overweight)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


# ++++++++ Question 4 ++++++++ #

# 4.	Is smoking associated with CVD? 
# Populate your results in the table.

tab_smoker_cvd = table(chp$smoker,chp$cvd)
dimnames(tab_smoker_cvd) = list(c('non-smoker','smoker'),c('no CVD','CVD'))
tab_smoker_cvd


apply(tab_smoker_cvd,MARGIN = 2,FUN = function(x) round(x/sum(x)*100,2))
chisq.test(tab_smoker_cvd)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #




