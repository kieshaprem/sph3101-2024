############# Analyse your data ############# 

# Linear regression

m_ldl_female = lm(chp$ldl ~ chp$female)
summary(m_ldl_female)

# t.test(chp$ldl ~ chp$female)

m_ldl_age = lm(chp$ldl ~ chp$age)
m_ldl_age_summary = summary(m_ldl_age)
m_ldl_age_summary
# Estimate (95% CI)
paste0('Estimate (95% CI): ',
       round(m_ldl_age_summary$coefficients[2,1],4),' (',
       round(m_ldl_age_summary$coefficients[2,1]-1.96*m_ldl_age_summary$coefficients[2,2],4),', ',
       round(m_ldl_age_summary$coefficients[2,1]+1.96*m_ldl_age_summary$coefficients[2,2],4),')')

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #


############# Practical ############# 

# ++++++++ Question ++++++++ #

# Is there a linear association between BMI and LDL cholesterol levels? 
# Report the correlation coefficient and p value of correlation. 




# ++++++++ Question ++++++++ #

# Explore different linear models and their association with LDL cholesterol levels





