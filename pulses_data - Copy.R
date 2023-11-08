# Load necessary library
library(readr)
library(dplyr)

# Load the dataset
data <- read_csv("C:/Users/vashi/OneDrive/Documents/LAB_ASSIGNMENT/PULSES/6289_source_data.csv")

# Identify numeric columns
numeric_columns <- sapply(data, is.numeric) & !sapply(data, is.integer)

# Calculate the integer mean for each numeric column, excluding zeros
integer_means <- sapply(data[, numeric_columns], function(x) {
  non_zero_entries <- x[x > 0]
  if (length(non_zero_entries) == 0) {
    return(0) # Return zero if there are no non-zero entries to calculate mean
  } else {
    return(as.integer(mean(non_zero_entries)))
  }
})

# Replace zeros with the integer mean for each numeric column
data <- data %>%
  mutate(across(all_of(names(integer_means)), ~replace(., . == 0, integer_means[names(integer_means) == deparse(substitute(.))])))

# Save the updated dataframe to a new CSV file
write_csv(data, "C:/Users/vashi/OneDrive/Documents/LAB_ASSIGNMENT/PULSES/updated_6289_source_data.csv")

# Load necessary libraries
library(readr)
library(ggplot2)
library(dplyr)


# Summary of the column 'Yeild of Total Pulses'
summary(data$`Yeild of Total Pulses`)

# Perform a one-sample t-test
t_test_result <- t.test(data$`Yeild of Total Pulses`, mu = 600)

# Print the results of the t-test
print(t_test_result)

# Visualize the data with a histogram
ggplot(data, aes(x = `Yeild of Total Pulses`)) +
  geom_histogram(binwidth = 10, fill = 'blue', color = 'black') +
  labs(title = "Histogram of Total Pulses Yield", x = "Yield", y = "Frequency")

# Visualize the data with a boxplot
ggplot(data, aes(y = `Yeild of Total Pulses`)) +
  geom_boxplot(fill = 'blue') +
  labs(title = "Boxplot of Total Pulses Yield", y = "Yield")

# Load necessary libraries
library(readr)
library(ggplot2)
library(corrplot)

# Calculate correlations
crops_yield_data <- data %>% select(starts_with("Yeild"))
correlation_matrix <- cor(crops_yield_data)

# Plot the correlation heatmap
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust")

# Load necessary libraries
library(readr)
library(ggplot2)
library(corrplot)
library(reshape2)
library(cluster)

# Calculate correlations for yield columns and plot heatmap
crops_yield_data <- data %>% select(starts_with("Yeild"))
correlation_matrix <- cor(crops_yield_data, use = "complete.obs") # Handle missing values if any
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", tl.cex = 0.7)

# Melt the data for ggplot
melted_crops_data <- melt(crops_yield_data)

# Plot histograms for each crop yield
ggplot(melted_crops_data, aes(x = value)) + 
  geom_histogram(binwidth = 10, fill = 'blue', color = 'black') + 
  facet_wrap(~variable, scales = 'free_x') + 
  labs(x = "Yield", y = "Frequency") + 
  theme_minimal()

# Perform clustering using K-means
set.seed(123) # Set seed for reproducibility
clusters <- kmeans(crops_yield_data, centers = 3) # assuming we want 3 clusters

# Add cluster assignment to the data
data$cluster <- as.factor(clusters$cluster)

# Plot clusters
pairs(crops_yield_data, col = data$cluster)

# Fit a linear model with one crop yield as the response and others as predictors
lm_model <- lm(`Yeild of Total Pulses` ~ ., data = crops_yield_data)

# Summary of the model
summary(lm_model)

library(cluster)
library(factoextra)

# Calculate total within-cluster sum of square
wss <- (nrow(crops_yield_data)-1)*sum(apply(crops_yield_data,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(crops_yield_data, centers=i)$withinss)

# Plot the Elbow Method
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

library(ggplot2)

# Assuming 'lm_model' is your linear model with `Yeild of Total Pulses` as the response
# and you have a variable (let's say `Yeild of Tur`) as a predictor.

# Scatter plot with regression line
ggplot(data, aes(x = `Yeild of Tur`, y = `Yeild of Total Pulses`)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(x = "Yield of Tur", y = "Yield of Total Pulses", title = "Regression of Total Pulses on Tur Yield") +
  theme_minimal()

