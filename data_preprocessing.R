# To get the current directory
getwd()

# To set directory to new working directory
setwd("D:/MSc Big Data/Applied Statistics/Dataset")

# Loading dataset into amazon_data variable
amazon_dataset <- read.csv("amazon_dataset.csv")
amazon_dataset

library(dplyr)

# Step 1: Removing 'imgURL' and 'productURL' columns
processed_data <- select(amazon_dataset, -c(imgUrl, productURL))

# Step 2: Performing random sampling to select 20,000 rows
set.seed(123)  # Set seed for reproducibility
processed_data <- sample_n(processed_data, 20000, replace = FALSE)

# Step 3: Checking for missing values
missing_values <- colSums(is.na(processed_data))
if (any(missing_values > 0)) {
  print("There are missing values in the dataset.")
} else {
  print("There are no missing values in the dataset.")
}

# Step 4: Adding 'StarSentiment' column based on star ratings
processed_data$StarSentiment <- ifelse(processed_data$stars >= 4, "Positive",
                                       ifelse(processed_data$stars > 2 & processed_data$stars < 4, "Normal",
                                              "Negative"))

# Step 5: Adding 'ReviewSentiment' column based on number of reviews
processed_data$ReviewSentiment <- ifelse(processed_data$reviews >= 100, "Positive",
                                         ifelse(processed_data$reviews >= 50 & processed_data$reviews < 100, "Normal",
                                                "Negative"))

# Displaying top 10 records of preprocessed data
head(processed_data, 10)

# Save the preprocessed data into a new file (e.g., 'preprocessed_data.csv')
write.csv(processed_data, file = "D:/MSc Big Data/Applied Statistics/Dataset/preprocessed_data.csv", row.names = FALSE)


