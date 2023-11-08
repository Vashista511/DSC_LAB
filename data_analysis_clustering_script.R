# Load necessary libraries
library(dplyr)
library(readr)

# Read the dataset
data <- read_csv("6288_source_data.csv")

# List of yield columns to update
yield_columns <- c("Yield of Paddy", "Yield of Jowar", "Yield of Bajra", "Yield of Maize", 
                   "Yield of Ragi", "Yield of Wheat", "Yield of  Small Millets", 
                   "Yield of total Cereals & Millets")

# Replace zeroes with NA for computation of mean without considering zeroes
data[data == 0] <- NA

# Replace NA values with rounded column means
data <- data %>%
  mutate(across(all_of(yield_columns), ~ ifelse(is.na(.), round(mean(., na.rm = TRUE)), .), .names = "updated_{.col}"))

# Display the first few rows after the replacement
head(data)
