############# In-class practical ############# 

load('data/cvdData_week5.RData')


# Initialise a full model (without id and cvd)
mod1 = lm(ldl ~ race + age + gender + bmi,data = cvdData)
summary(mod1)

# Backward stepwise regression
mod1_back <- step(mod1, direction = "backward", scope = formula(~ .))
summary(mod1_back)

# Forward and Backward stepwise regression
mod1_both <- step(mod1, direction = "both")
summary(mod1_both)

# Interpret the outputs of the backwards and stepwise regression? 



