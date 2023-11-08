
# Load necessary libraries
library(tidyverse)
library(caret)
library(corrplot)

# Load the dataset
data <- read.csv("6288_source_data.csv")

# Remove unnecessary columns
data$srcStateName <- NULL
data$srcYear <- NULL
data$Year <- NULL

# Checking for missing values
sum(is.na(data))

# If there are missing values, you can either remove those rows or fill them with appropriate values.
# For this demonstration, we'll simply remove rows with missing values.
data <- na.omit(data)

# Splitting data into predictors and target
predictors <- data[, 1:(ncol(data)-2)]
target <- data$`Yield of total Cereals & Millets`

# Exploratory Data Analysis (EDA)

# Histograms for each of the yields
par(mfrow=c(3,3))
for (col in names(predictors)) {
  hist(predictors[[col]], main=col, xlab=col, col='lightblue', border='black')
}

# Correlation Heatmap
correlation_matrix <- cor(predictors)
corrplot(correlation_matrix, method='circle')

# Split data
set.seed(123) # Setting seed for reproducibility
trainIndex <- createDataPartition(target, p = 0.8, list = FALSE)
train_data <- predictors[trainIndex, ]
test_data <- predictors[-trainIndex, ]
train_target <- target[trainIndex]
test_target <- target[-trainIndex]

# Model training
model <- lm(train_target ~ ., data = train_data)
summary(model)

# Model evaluation
predictions <- predict(model, newdata = test_data)
mse <- mean((predictions - test_target)^2)
cat("Mean Squared Error:", mse)

# Residual Plot
residuals <- test_target - predictions
plot(predictions, residuals, main="Residual Plot", 
     xlab="Predicted Values", ylab="Residuals", pch=19, col='blue')
abline(h=0, col='red', lwd=2)

# Prediction vs. Actual Plot
plot(test_target, predictions, main="Prediction vs. Actual", 
     xlab="Actual Values", ylab="Predicted Values", pch=19, col='darkgreen')
abline(0, 1, col='red', lwd=2)

# Predictions
cat("Predictions:", predictions)

