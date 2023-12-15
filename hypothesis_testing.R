# To get the current directory
getwd()

#To set directory to new working directory
setwd("D:/MSc Big Data/Data Mining/Dataset")

# Loading dataset into amazon_data variable
amazon_data <- read.csv("D:/MSc Big Data/Data Mining/Dataset/preprocessed_amazon_data.csv") 
amazon_data

# Perform ANOVA
anova_result <- aov(stars ~ categoryName, data = amazon_data)
summary(anova_result)

# Post-hoc Tukey test
tukey_result <- TukeyHSD(anova_result)
summary(tukey_result)

# Boxplot
boxplot(stars ~ categoryName, data = amazon_data, col = "skyblue", main = "Product Ratings by Category")

# Bar plot
library(ggplot2)
ggplot(amazon_data, aes(x = categoryName, y = stars, fill = categoryName)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Product Ratings by Category", x = "Category", y = "Average Ratings") +
  theme_minimal()







# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assuming 'amazon_data' contains your dataset

# Select 15 categories for visualization
selected_categories <- amazon_data %>%
  group_by(categoryName) %>%
  summarize(count = n()) %>%
  top_n(15, count) %>%
  pull(categoryName)

# Filter data for selected categories
filtered_data <- amazon_data %>%
  filter(categoryName %in% selected_categories)

# Perform ANOVA for selected categories
anova_result <- aov(stars ~ categoryName, data = filtered_data)
summary(anova_result)

# Post-hoc Tukey test for selected categories
tukey_result <- TukeyHSD(anova_result)
summary(tukey_result)

# Boxplot for selected categories
boxplot(stars ~ categoryName, data = filtered_data, col = "skyblue", main = "Product Ratings by Category")

# Bar plot for selected categories
ggplot(filtered_data, aes(x = categoryName, y = stars, fill = categoryName)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Product Ratings by Category (Top 15)", x = "Category", y = "Average Ratings") +
  theme_minimal()






# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assuming 'amazon_data' contains your dataset

# Perform ANOVA for all categories
anova_result <- aov(stars ~ categoryName, data = amazon_data)
summary(anova_result)

# Post-hoc Tukey test for all categories
tukey_result <- TukeyHSD(anova_result)
summary(tukey_result)

# Line plot for average ratings across all categories
ggplot(amazon_data, aes(x = categoryName, y = stars, group = 1)) +
  stat_summary(fun = "mean", geom = "line", aes(color = "Average Ratings")) +
  labs(title = "Average Product Ratings across All Categories",
       x = "Category",
       y = "Average Ratings") +
  theme_minimal()
