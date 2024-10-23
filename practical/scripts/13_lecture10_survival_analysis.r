
# Install necessary packages 
install.packages("survival")
install.packages("survminer")

# Load the libraries
library(survival)
library(survminer)

# Load the lung dataset
lung = lung
?lung
head(lung)  # View first few rows of the dataset


# Create a survival object (Surv function)
km_surv <- Surv(time = lung$time, event = lung$status)

# Fit the Kaplan-Meier model
km_fit <- survfit(km_surv ~ 1, data = lung)

# Summary of Kaplan-Meier fit
summary(km_fit)

# Plot the Kaplan-Meier curve
ggsurvplot(km_fit, 
           conf.int = TRUE, 
           xlab = "Time (days)", 
           ylab = "Survival Probability", 
           ggtheme = theme_minimal())



# Kaplan-Meier curve by sex
km_fit_sex <- survfit(Surv(time, status) ~ sex, data = lung)

# Plot survival curves by sex
ggsurvplot(km_fit_sex, 
           conf.int = TRUE, 
           pval = TRUE,   # Include p-value from log-rank test
           xlab = "Time (in days)", 
           ylab = "Survival Probability",
           legend.labs = c("Male", "Female"), 
           ggtheme = theme_minimal())


# Log-rank test
survdiff(Surv(time, status) ~ sex, data = lung)
