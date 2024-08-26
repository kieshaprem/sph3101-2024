############# In-class practical ############# 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

load('data/hospital.RData')

# ++++++++ Task 1 ++++++++ #

par(mfrow = c(1,2))
plot(x = hospital$LOS,y = hospital$EXPADM, pch = 16, xlab =  'Mean length of stay',
     ylab = 'Mean expense per admission')
plot(x = hospital$SALARY,y = hospital$EXPADM, pch = 18, xlab =  'Mean salary per employee',
     ylab = 'Mean expense per admission')




# ++++++++ Task 2 ++++++++ #
 
m1 = lm(hospital$EXPADM~hospital$LOS)
summary(m1)

m2 = lm(hospital$EXPADM~hospital$SALARY)
summary(m2)


# ++++++++ Task 3 ++++++++ #

m3 = lm(hospital$EXPADM~hospital$LOS + hospital$SALARY)
summary(m3)

confint(m3)

# ++++++++ Task 6 ++++++++ #
par(mfrow = c(2,2))
plot(m3)

modsum = data.frame(cookdist = cooks.distance(m3),
                    hatval = hatvalues(m3),
                    rstudent = rstudent(m3))
summary(modsum)

library(car)
par(mfrow = c(1,1))
influencePlot(m3, id=list(method="noteworthy",n=2))

