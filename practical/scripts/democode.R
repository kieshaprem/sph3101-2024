


# Scenario 1: Survey on vaping --------------------------------------------
library(DescTools)
# Part 1a: saw the poster -------------------------------------------------
ns = c(1325,1454) # MOH+CAG, CAG
ys = c(146,112) # saw signage

print(prop.test(ys[1],ns[1])) # to get the proportions and CIs within arms
print(prop.test(ys[2],ns[2])) # to get the proportions and CIs within arms

print(prop.test(ys,ns)) # for difference in proportions and CIs between arms

ct = matrix(c(ys[1],ys[2],ns[1]-ys[1],ns[2]-ys[2]),2,2) # make a 2x2 table
DescTools::RelRisk(ct,conf.level = .95) # relative risk & CI

# Tidy tidy
rm(ct,ys)

# Part 1b: surrendered their vapes ---------------------------------------------
# May not have seen the poster?
ys = c(10,1) # submitted
xs = c(1,0) # in the regression as the x variable


fit = glm(ys~xs,family = "poisson",offset = log(ns))
sfit = summary(fit)
print(sfit)
exp(fit$coefficients[2]) # relative rate 
exp(fit$coefficients[2]+c(-1,1)*1.96*sfit$coefficients[2,2]) # 95%CI

# Tidy tidy
rm(fit,sfit,ys)


# Part 1c: vapes confiscated!! ---------------------------------------------
# May not have seen the poster?
ys = c(16,5) # submitted
xs = c(1,0) # in the regression as the x variable


fit = glm(ys~xs,family = "poisson",offset = log(ns))
sfit = summary(fit)
print(sfit)
exp(fit$coefficients[2]) # relative rate 
exp(fit$coefficients[2]+c(-1,1)*1.96*sfit$coefficients[2,2]) # 95%CI

# Tidy tidy
rm(fit,sfit,ys,ns,xs)








# Scenario 2: Platelets and dengue --------------------------------------------
setwd("~/NUS Dropbox/Alex R Cook/t/sshsph/SPH3101")
d = read.csv('data_Dengue_Singapore.csv')

# On admission:
d$y = as.numeric(d$SD_presentation=='Yes')
fit = glm(y~Platelet,family='binomial',data = d)
summary(fit)

# At any point:
d$y = as.numeric(d$SD_ever=='Yes')
fit = glm(y~Platelet,family='binomial',data = d)
summary(fit)

# Among those without SD at presentation
d2 = d[d$SD_presentation=='No',]
d2$y = as.numeric(d2$SD_ever=='Yes')
fit = glm(y~Platelet,family='binomial',data = d2)
summary(fit)

# Easy solution
hist(d$Platelet)
d$Platelet = d$Platelet/100000000000 # rescale to per 100 billion

# On admission:
d$y = as.numeric(d$SD_presentation=='Yes')
fit = glm(y~Platelet,family='binomial',data = d)
summary(fit)

# At any point:
d$y = as.numeric(d$SD_ever=='Yes')
fit = glm(y~Platelet,family='binomial',data = d)
summary(fit)

# Among those without SD at presentation
d2 = d[d$SD_presentation=='No',]
d2$y = as.numeric(d2$SD_ever=='Yes')
fit = glm(y~Platelet,family='binomial',data = d2)
summary(fit)

# Tidy tidy
rm(d,d2,fit)


# Scenario 3: Predicting dengue risk --------------------------------------
# Naive approach
setwd("~/NUS Dropbox/Alex R Cook/t/sshsph/SPH3101")
d = read.csv('data_Dengue_Singapore.csv')
d2 = d[d$SD_presentation=='No',]
d2$y = as.numeric(d2$SD_ever=='Yes')

# change variables to numbers
d2$Sex = as.numeric(d2$Sex=='Male')
for(i in 18:26)d2[,i] = as.numeric(d2[,i]=='Yes')

# see which ones are predictive
significantones = c()
for(i in c(1,2,6:26))
{
  fit = glm(d2$y~d2[,i],family='binomial')
  sfit = summary(fit)
  pvalue = sfit$coefficients[2,4]
  if(pvalue<0.05)significantones = c(significantones,i)
}
names(d2)[significantones] # it seems these are the only significant ones???

# More appropriate approach
library(logistf)
significantones = c()
for(i in c(1,2,6:26))
{
  fit = logistf(d2$y~d2[,i])
  pvalue = fit$prob[2]
  if(pvalue<0.05)significantones = c(significantones,i)
}
names(d2)[significantones] # it seems these are the only significant ones???

# Tidy tidy
rm(d,d2,fit,sfit,i,pvalue,significantones)



# Scenario 4: Mortality and temperature -----------------------------------
setwd("~/NUS Dropbox/Alex R Cook/t/sshsph/SPH3101")
d = read.csv('mortality.csv')
d$day = as.numeric(as.Date(d$Date)-as.Date('2015-12-31'))
plot(d$day,d$Temperature)
plot(d$day,d$Mortality)
plot(d$Temperature,d$Mortality)

# naive answer
fit = glm(Mortality~Temperature,family='poisson',data=d)
summary(fit)
plot(d$Temperature,d$Mortality)
points(d$Temperature,fit$fitted.values,col=2,pch=16)

# better answer
library(mgcv)
fit = gam(Mortality~s(Temperature),family='poisson',data=d)
summary(fit)
plot(fit)
plot(d$Temperature,d$Mortality)
points(d$Temperature,fit$fitted.values,col=2,pch=16)

# Tidy tidy
rm(d,fit)

