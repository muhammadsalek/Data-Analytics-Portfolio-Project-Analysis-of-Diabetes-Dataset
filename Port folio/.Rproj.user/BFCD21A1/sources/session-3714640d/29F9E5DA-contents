#### Install necessary libraries (run only if not installed)####

install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("corrplot")



#### Load libraries####
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyverse)


####Load and Inspect Data####
# Load dataset
data <- read.csv("D:\\COURSE PAID\\R Batch Naim Vai\\Portfolio project\\Dataset\\diabetes - diabetes.csv")

# View first few rows
head(data)

# Check for missing values
sum(is.na(data))

# Summary statistics
summary(data)

####Problem Statement 1: Blood Pressure vs. BMI####
# Scatter plot of Blood Pressure vs. BMI
# Load necessary libraries
library(ggplot2)

# Create enhanced scatter plot
ggplot(data, aes(x = BloodPressure, y = BMI)) +
  geom_point(alpha = 0.7, color = "#1f77b4", size = 2) +  # Better contrast blue
  geom_smooth(method = "lm", col = "#d62728", linewidth = 1.5, linetype = "solid") + # Red regression line
  labs(
    title = "Association Between Blood Pressure and BMI",
    x = "Blood Pressure (mm Hg)",
    y = "BMI (kg/m²)"
  ) +
  theme_minimal(base_size = 16) +  # Larger base font size for readability
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5), # Centered, bold title
    axis.title = element_text(face = "bold", size = 16), # Bold axis titles
    axis.text = element_text(size = 14), # Larger axis text
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"), # Subtle grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )




####Problem Statement 2: Factors Affecting Glucose Levels ####
# Correlation test
cor.test(data$BloodPressure, data$BMI, use = "complete.obs")


# Compute the correlation matrix
cor_matrix <- cor(data[, c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "Age", "DiabetesPedigreeFunction")], 
                  use = "complete.obs")

# Visualize the correlation matrix
corrplot(cor_matrix, 
         method = "color",        # Use colored visualization
         col = colorRampPalette(c("blue", "white", "red"))(200),  # Blue to Red gradient
         addCoef.col = "black",   # Add correlation values in black
         tl.cex = 1,              # Adjust text label size
         number.cex = 0.8,        # Adjust correlation coefficient text size
         order = "hclust",        # Hierarchical clustering for better arrangement
         diag = FALSE)            # Hide diagonal

# Linear regression to predict glucose levels
glucose_model <- lm(Glucose ~ BloodPressure + SkinThickness + Insulin + BMI + Age + DiabetesPedigreeFunction, data = data)
summary(glucose_model)

# Check for Multicollinearity (VIF)

# Install and load the 'car' package if not installed
install.packages("car") 
library(car)

# Check VIF for multicollinearity
vif(glucose_model)


# Check Residual Diagnostics

# Plot residual diagnostics
par(mfrow = c(2, 2)) # Set layout for multiple plots
plot(glucose_model)
par(mfrow = c(1, 1)) # Reset layout




# Check Model Performance
# Model summary for coefficients, R², and p-values
summary(glucose_model)






####Problem Statement 3: Most Important Factor for Diabetes Risk####

# Ensure Diabetes is a Binary Factor
# Check unique values of Diabetes
unique(data$Diabetes)
# Convert "Yes" to 1 and "No" to 0
data$Diabetes <- as.numeric(data$Diabetes == "Yes")


# Run Logistic Regression
diabetes_model <- glm(Diabetes ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + Age + DiabetesPedigreeFunction, 
                      data = data, family = binomial)

summary(diabetes_model)








#Find the Most Significant Predictor
most_significant <- names(coef(diabetes_model))[which.min(summary(diabetes_model)$coefficients[, 4])]
print(paste("The most significant factor influencing diabetes risk is:", most_significant))





# Install and load pROC package (if not installed)
install.packages("pROC")
library(pROC)

# Install and load pROC package (if not installed)
install.packages("pROC")
library(pROC)

# Get the predicted probabilities from the logistic regression model
pred_prob <- predict(diabetes_model, type = "response")

# Compute the ROC curve
roc_curve <- roc(data$Diabetes, pred_prob)

# Plot the ROC curve
plot(roc_curve, 
     main = "ROC Curve for Diabetes Risk Prediction", 
     col = "#1f77b4", 
     lwd = 2, 
     cex.main = 1.5, 
     cex.lab = 1.2)

# Add AUC to the plot
text(0.6, 0.2, paste("AUC =", round(auc(roc_curve), 3)), cex = 1.5, col = "red")








