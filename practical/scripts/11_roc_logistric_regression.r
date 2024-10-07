# Install necessary package
install.packages("pROC")

# Load the required libraries
library(pROC)

# Load the dataset
# You can download the dataset from the UCI Machine Learning Repository
# We'll use a URL to load it directly
url <- "https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.data.csv"
pima_data <- read.csv(url, header = FALSE)
head(pima_data)
rm(url)

# Assigning column names based on the dataset's description
colnames(pima_data) <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", 
                         "Insulin", "BMI", "DiabetesPedigreeFunction", "Age", "Outcome")

# Display the first few rows of the dataset
head(pima_data)

# Convert the Outcome variable to a factor
table(pima_data$Outcome)
pima_data$Outcome <- as.factor(pima_data$Outcome)

# Build a logistic regression model using "Glucose" and "BMI" as predictors
model_pima <- glm(Outcome ~ Glucose + BMI, data = pima_data, family = binomial)

# Display a summary of the model
summary(model_pima)

# Obtain predicted probabilities from the model
pred_probs <- predict(model_pima, type = "response")

# Generate the ROC curve using pROC
roc_curve <- roc(pima_data$Outcome, pred_probs)
plot(roc_curve, main = "ROC Curve for Diabetes Prediction", col = "steelblue")
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 3)))


pima_data$modelled_outcome = 1*(pred_probs > 0.5)
table(pima_data$Outcome,pima_data$modelled_outcome)
