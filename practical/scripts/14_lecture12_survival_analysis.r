# Load necessary libraries
library(survival)
library(survminer)


# Task 1: Kaplan-Meier Analysis of Time to parasitemia

# Load necessary libraries
library(survival)
library(survminer)

# Load the data
malaria_trial <- read.csv('data/malaria_trial.csv')

# View the structure of the data
str(malaria_trial)
head(malaria_trial)

# Fit Kaplan-Meier survival curves by treatment arm
malariafit = survfit(Surv(malaria_trial$Time,1-malaria_trial$Censored)~malaria_trial$Tx,conf.type='log-log')


# Plot the Kaplan-Meier curves
plot(malariafit, main="Kaplan-Meier Curves by Treatment",
     xlab="Time (hours)", ylab="Probability of Parasitic Presence",
     col=c("black", "steelblue"),conf.int=TRUE,lwd=2)
legend("topright", legend=c("PA", "MA"), col=c("black", "steelblue"), lty=1)

plot(malariafit, main="Kaplan-Meier Curves by Treatment",
     xlab="Time (hours)", ylab="Probability of Parasitic Presence",
     col=c("black", "steelblue"),conf.int=FALSE,lwd=2)
legend("topright", legend=c("PA", "MA"), col=c("black", "steelblue"), lty=1)

# Interpretation

# Compare the time to parasitemia for both treatments.
# Look for differences in survival probabilities over time between the PA and MA groups.

# Task 2: Testing for Differences Between Treatment Arms

# Conduct log-rank test to compare survival curves between treatment arms
# The log-rank test result will show if there is a statistically significant difference in the time to aparasitemia between the PA and MA groups.
# In a paper: You would report the p-value and whether it indicates a significant difference, along with any descriptive statistics on time to aparasitemia for each treatment group.

survdiff(Surv(malaria_trial$Time, 1 - malaria_trial$Censored) ~ malaria_trial$Tx)

# Interpretate the results


# Task 3: Exploring Differences by Country
# Why was Cambodia (kh) singled out?
  
# Fit survival curves by country
malariafit <- survfit(Surv(malaria_trial$Time, 1 - malaria_trial$Censored) ~ malaria_trial$Country, conf.type='log-log')

# Plot survival curves for Cambodia vs. other countries
plot(malariafit, col=c("black", "coral"), xlab="Time (days)", ylab="Probability of Parasitic Presence", conf.int=TRUE)
legend("topright", legend=c("Other countries", "Cambodia"), col=c("black", "coral"), lty=1)

survdiff(Surv(malaria_trial$Time, 1 - malaria_trial$Censored) ~ malaria_trial$Country)

# Fit Cox model with interaction between Country and Treatment
malariafit <- coxph(Surv(malaria_trial$Time, 1 - malaria_trial$Censored) ~ malaria_trial$Country)
summary(malariafit)
malariafit <- coxph(Surv(malaria_trial$Time, 1 - malaria_trial$Censored) ~ malaria_trial$Country * malaria_trial$Tx)
summary(malariafit)

# Further Analysis

# Recode into four groups for combined Country and Treatment effects
group <- 1 * (malaria_trial$Country == 'kh' & malaria_trial$Tx == 'PA') +
  2 * (malaria_trial$Country == 'ot' & malaria_trial$Tx == 'PA') +
  3 * (malaria_trial$Country == 'kh' & malaria_trial$Tx == 'MA') +
  4 * (malaria_trial$Country == 'ot' & malaria_trial$Tx == 'MA')

# Fit Cox model based on new grouping variable
malariafit <- coxph(Surv(malaria_trial$Time, 1 - malaria_trial$Censored) ~ factor(group))
summary(malariafit)

# Comparing Treatment Effects in Cambodia Only and Excluding Cambodia
# Remove Cambodia (ot only)
i <- which(malaria_trial$Country == 'ot')
survdiff(Surv(malaria_trial$Time[i], 1 - malaria_trial$Censored[i]) ~ malaria_trial$Tx[i])

# Analyse Cambodia (kh only)
i <- which(malaria_trial$Country == 'kh')
survdiff(Surv(malaria_trial$Time[i], 1 - malaria_trial$Censored[i]) ~ malaria_trial$Tx[i])

# Interpretation
# 
# Why Cambodia was singled out: The analysis could reveal that patients in Cambodia respond differently to the treatments, 
# potentially due to factors like parasite resistance or regional health characteristics.






#######################################################################################

# Practical

# Step 1: Load the Data
# Read the dataset into R and explore it.

# Load the data
x <- read.csv('data/vivax_incubation_periods.csv')

# View the structure of the data
str(x)
head(x)

# Dataset: vivax_incubation_periods.csv, containing columns:
# id: study id  
# time: Incubation period (in days).
# latitude: Geographic latitude (either "temperate" or "tropical").
# hemisphere: Geographic hemisphere (either "newworld" for the Americas or "oldworld" for Eurasia and Africa).


# Step 2: Kaplan-Meier Plot for Overall Incubation Periods
# Generate a Kaplan-Meier plot for the overall incubation period.

# Fit survival curve for all data
vivaxfit <- survfit(Surv(x$time) ~ 1)

# Plot the survival curve
plot(vivaxfit, main="Kaplan-Meier Curve for Malaria Incubation Periods", xlab="Time (days)", ylab="Survival Probability",
     xlim=c(0,30))



# Step 3: Kaplan-Meier Curves by Latitude
# Examine the difference in incubation periods based on geographic latitude (tropical vs. temperate).

# Fit survival curve by latitude
# is there any irregularities in the data? 
x$latitude 
x$latitude[x$latitude=='2. Temperate']='temperate'

vivaxfit <- survfit(Surv(x$time) ~ x$latitude)
# Plot survival curves for different latitudes
plot(vivaxfit, main="KM Curves by Latitude", xlab="Time (days)", ylab="Pr(Incubation time > t)",xlim=c(0,30),col = c(1,2,3))
legend("topright", legend=c("Temperate", "Tropical"), col=c("black", "red"), lty=1)



# Step 4: Kaplan-Meier Curves by Hemisphere
# Generate Kaplan-Meier plots based on hemisphere (newworld vs. oldworld).

# Fit survival curve by hemisphere
vivaxfit <- survfit(Surv(x$time) ~ x$hemisphere)

# Plot survival curves for different hemispheres
plot(vivaxfit, main="KM Curves by Hemisphere", xlab="Time (days)", ylab="Survival Probability",col = c(1,2),xlim=c(0,30))

plot(density(x$time[x$hemisphere %in% "oldworld"]),col=2,xlim=c(0,30))
lines(density(x$time[x$hemisphere %in% "newworld"]),col = 1,xlim=c(0,30))

# Task 2: Compare Geographic Groups
# Step 5: Kaplan-Meier Curves with Both Latitude and Hemisphere
# To examine incubation periods by combining latitude and hemisphere, create four groups:
  
# 1. Temperate, New World
# 2. Tropical, New World
# 3. Temperate, Old World
# 4. Tropical, Old World


# Define groups based on latitude and hemisphere
group <- 1 * (x$latitude == 'temperate' & x$hemisphere == 'newworld') +
  2 * (x$latitude == 'tropical' & x$hemisphere == 'newworld') +
  3 * (x$latitude == 'temperate' & x$hemisphere == 'oldworld') +
  4 * (x$latitude == 'tropical' & x$hemisphere == 'oldworld')

# Fit survival curve by group
vivaxfit <- survfit(Surv(x$time) ~ group)

# Plot the survival curves for each group
plot(vivaxfit, xlim=c(0,30), col=c('black', 'red', 'blue', 'orange'),
     main="KM Curves by Latitude and Hemisphere Combination", xlab="Time (days)", ylab="Survival Probability")
legend("topright", legend=c("Temperate New World", "Tropical New World", "Temperate Old World", "Tropical Old World"),
       col=c("black", "red", "blue", "orange"), lty=1)

# Step 6: Log-Rank Test for Statistical Significance
# Perform the log-rank test to determine if there are significant differences between the four groups.

survdiff(Surv(x$time) ~ group)

# A significant p-value would indicate that incubation periods differ between these geographic groups. What do you observe? 

# Step 7: Cox Proportional Hazards Model
# To further quantify the differences, fit a Cox proportional hazards model with the geographic group as a factor.


# Fit Cox proportional hazards model
cox_model <- coxph(Surv(x$time) ~ factor(group))
summary(cox_model)

?intersect()




