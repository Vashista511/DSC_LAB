
# Load necessary libraries
library(tidyverse)
library(caret)
library(corrplot)
library(ggplot2)
library(dplyr)
library(lubridate)

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

# Identify non-numeric columns
non_numeric_cols <- names(predictors)[sapply(predictors, function(col) !is.numeric(col))]

# Exclude non-numeric columns from the histogram plotting
numeric_predictors <- predictors[ , !(names(predictors) %in% non_numeric_cols)]
dev.new()
# Histograms for numeric columns
par(mfrow=c(3,3))
for (col in names(numeric_predictors)) {
  hist(numeric_predictors[[col]], main=col, xlab=col, col='lightblue', border='black')
}
dev.new()
# Correlation Heatmap
correlation_matrix <- cor(numeric_predictors)
corrplot(correlation_matrix, method="circle", tl.cex = 0.6)

# Read the dataset
data1 <- read.csv("rounded_6288_source_data.csv")

yield_columns <- c("Yield of Paddy", "Yield of Jowar", "Yield of Bajra", "Yield of Maize", 
                   "Yield of Ragi", "Yield of Wheat", "Yield of  Small Millets", 
                   "Yield of total Cereals & Millets")

colnames(data)
data_long <- data %>%
  gather(key = "cereal", value = "yield", -1, -2)
dev.new()
data_long %>%
  ggplot(aes(x = yield, fill = cereal)) +
  geom_bar() +
  facet_wrap(~cereal, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of yield for different cereals & millets across sub-districts",
       x = "Yield", y = "Count")

# Filter out the 'YearCode' or any non-cereal columns
data_filtered <- data_long %>% filter(cereal != "YearCode")

# For each cereal, identify the sub-district with the highest and lowest yield
data_filtered %>%
  group_by(cereal) %>%
  top_n(n = 1, wt = yield) %>%
  select(cereal, yield) %>%
  bind_rows(
    data_filtered %>%
      group_by(cereal) %>%
      top_n(n = -1, wt = yield) %>%
      select(cereal, yield)
  ) %>%
  arrange(cereal, yield)

# Filter data for a single cereal (e.g., the first unique cereal in the dataset)
single_cereal <- unique(data_long$cereal)[1]

data_single_cereal <- data_long %>% 
  filter(cereal == single_cereal)
dev.new()
# Plot the trend for this single cereal
ggplot(data_single_cereal, aes(x = as.numeric(data[[2]]), y = yield)) + 
  geom_line(alpha = 0.6) +
  theme_minimal() +
  labs(title = paste("Trend of yield for", single_cereal, "over the years"),
       x = "Year", y = "Yield")

# Read the data
data_r <- read.csv('rounded_6288_source_data.csv')

# Extract columns related to crop yields for clustering
clustering_data <- data_r[, c('Yield.of.Paddy', 'Yield.of.Jowar', 'Yield.of.Bajra', 'Yield.of.Maize', 'Yield.of.Ragi', 'Yield.of.Wheat', 'Yield.of..Small.Millets', 'Yield.of.total.Cereals...Millets')]

# Compute sum of squared distances for a range of cluster numbers
set.seed(123)  # Setting seed for reproducibility
wss <- map_dbl(1:10, function(k) {
  model <- kmeans(clustering_data, centers = k)
  model$tot.withinss
})

# Plot the results
tibble(k = 1:10, wss = wss) %>%
  ggplot(aes(x = k, y = wss)) +
  geom_line() +
  labs(title = "Elbow Method for Determining Optimal Number of Clusters",
       x = "Number of Clusters",
       y = "Within-cluster Sum of Squares") +
  theme_minimal()

set.seed(123)  # Setting seed for reproducibility
k <- 3  # Replace 3 with the optimal number of clusters you determined from the elbow method
clusters <- kmeans(clustering_data, centers = k)

# Reduce dimensions using PCA for visualization
pca_res <- prcomp(clustering_data, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_res$x[, 1:2])
pca_data$cluster <- as.factor(clusters$cluster)
dev.new()
# Plot the clusters
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(aes(shape = cluster), size = 3) +
  labs(title = "Clusters Visualization using PCA",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  scale_color_discrete(name = "Cluster")

# Load the dataset
data <- read.csv("rounded_6288_source_data.csv")

# Remove columns with only one unique value
data <- data[, sapply(data, function(col) length(unique(col)) > 1)]

# One-hot encode the entire dataset
dummy <- dummyVars(" ~ .", data=data, fullRank = TRUE)
data_encoded <- as.data.frame(predict(dummy, data))

# Split the encoded dataset into training and testing sets
set.seed(42)
splitIndex <- createDataPartition(data$`Yield.of.Paddy`, p = 0.8, list = FALSE)
train_set <- data_encoded[splitIndex,]
test_set <- data_encoded[-splitIndex,]

# Linear Regression Model
linear_model <- lm(`Yield.of.Paddy` ~ ., data = train_set)
linear_preds <- predict(linear_model, newdata = test_set)

# Model Evaluation for Linear Regression
mae_linear <- mean(abs(linear_preds - test_set$`Yield.of.Paddy`))
mse_linear <- mean((linear_preds - test_set$`Yield.of.Paddy`)^2)
r2_linear <- 1 - (sum((linear_preds - test_set$`Yield.of.Paddy`)^2) / sum((mean(test_set$`Yield.of.Paddy`) - test_set$`Yield.of.Paddy`)^2))

# Data for plotting
plot_data <- data.frame(Actual = test_set$`Yield.of.Paddy`, Predicted = linear_preds)
dev.new()
# Scatter plot of actual vs predicted values
p <- ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Linear Regression: Actual vs Predicted", x = "Actual Yield of Paddy", y = "Predicted Yield of Paddy")

p


