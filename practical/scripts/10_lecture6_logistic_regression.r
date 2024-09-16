############# In-class practical ############# 

# load the data
tardive <- read.table("data/tardive.txt", sep="\t", header = T)
head(tardive)

# Exploratory data analysis

# What do you notice about the variables such as ‘race’, ‘sex’, ‘td’, ‘htra’ and ‘t102’? 
summary(tardive)
# Hint1: These are supposed to be categorical variables! 
# Hint2: tardive[,"race"] <- as.factor(tardive[,"race"])







summary(tardive)
# What has changed? What can you say from the summary of the variables?


# It is important to ensure that categorical variables are correctly coded, 
# before the interpretation makes sense.



# Perform exploratory data analyses on the data, do a the 2x2 contingency table
# e.g., table(tardive$td, tardive$race)






# Perform exploratory data analyses on the data, plot the histogram of the 
# continuous variables 
# e.g., hist(tardive$age)








# There are clearly some problems with three variables (race, age and exponeur). 
# Let's exclude those observations for now.

tardive <- tardive[-which(tardive$age < 10 | tardive$exponeur > 80 | tardive$race == "2"),]
summary(tardive)


# We can perform a series of univariate analyses prior to the use of a 
# regression-based approach, to compare those that are affected with 
# tardive dyskinesia with those that are unaffected. 
# Note that this should make use of the information obtained from the histograms.

# What can you say about the distributions of the numerical variables, 
# especially for the variable cpz? How does that affect subsequent analyses?




tardive$unaffected <- ifelse(tardive$td %in% 1,1,0)
tardive$affected <- ifelse(tardive$td %in% 3,1,0)


t.test(tardive$age[tardive$unaffected %in% 1], tardive$age[tardive$affected %in% 1])
t.test(tardive$durill[tardive$unaffected %in% 1], tardive$durill[tardive$affected %in% 1])
t.test(tardive$exponeur[tardive$unaffected %in% 1], tardive$exponeur[tardive$affected %in% 1])
t.test(tardive$cpz[tardive$unaffected %in% 1], tardive$cpz[tardive$affected %in% 1])

chisq.test(tardive$td, tardive$sex)
chisq.test(tardive$td, tardive$htra)
chisq.test(tardive$td, tardive$t102)

# What do the analyses reveal about the differences between the unaffected and the affected? 
# Are there any analyses that are not necessarily appropriate? 
  


# Let us try to visualise the differences for CPZ. 
# There are multiple ways of doing this:
# 1) boxplots
boxplot(tardive$cpz[tardive$unaffected%in% 1], tardive$cpz[tardive$affected%in% 1], names = c("Unaffected", "Affected"))

# 2) density plots
plot(density(tardive$cpz[tardive$unaffected%in% 1]), xlab = "", col = 2, lwd = 2)
lines(density(tardive$cpz[tardive$affected%in% 1]), col = 4, lwd = 2)
legend(1000, 0.0015, col = c(2, 4), lty = 1, lwd = 2, legend = c("Unaffected", "Affected"),bty='n')
# What do you notice? 

# You can try the same for the variables age and durill.



# Let us also explore the relationship between pairs of numerical variables. 
# This can be achieved graphically with scatterplots.

pairs(~ tardive$age + tardive$durill + tardive$exponeur + tardive$cpz)
plot(tardive$age, tardive$durill, pch = 16)
plot(tardive$durill, tardive$exponeur, pch = 16)

# Explore the relationship between some of the numerical variables. 
# What can you say about the relationships between some of these variables? 
  

# Let us now consider fitting a series of logistic regression to identify the 
# factors that are associated with the status of tardive dyskinesia. 

# You may want to start off with the following commands:

fit.1 <- glm(td ~ age + durill + exponeur + cpz + sex + htra + t102, data = tardive,family = "binomial")
summary(fit.1)
anova(fit.1, test = "Chisq")
# What is the anova command essentially trying to do? 
# Can you identify the hypothesis that it is trying to test?

# What can you say about the results of the logistic regression? 
# Notice the number of parameters fitted for the variables ’htra’ and ’t102’.
# What will be the next step?
  

# Perhaps remove the variable exponeur from the model.
# Then, compare the model fits 
# Continue the model selection to identify a parsimonious model.





# Remove the least useful explanatory variable and re-fit the logistic regression.
# Then, compare the model fits using the anova function. 








# How do the results of the parsimonious model differ from the findings obtained 
# from the univariate analyses? What is the likely explanation for the difference?


  
