
# Load necessary libraries
library(tidyverse)
library(caret)

# Load the dataset
data <- read.csv("/path/to/6288_source_data.csv")

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

# Predictions
cat("Predictions:", predictions)

