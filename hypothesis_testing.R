# To get the current directory
getwd()

#To set directory to new working directory
setwd("D:/MSc Big Data/Data Mining/Dataset")

# Loading dataset into amazon_data variable
amazon_data <- read.csv("D:/MSc Big Data/Data Mining/Dataset/preprocessed_amazon_data.csv") 
amazon_data

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
