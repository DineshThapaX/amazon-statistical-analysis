# To get the current directory
getwd()

# To set directory to new working directory
setwd("D:/MSc Big Data/Applied Statistics/Dataset")
preprocessed_data <- read.csv("preprocessed_data.csv")
preprocessed_data

#Question 1

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(forcats)

# Get top 15 categories based on the number of products
top_categories <- preprocessed_data %>%
  count(categoryName) %>%
  top_n(15, n) %>%
  arrange(desc(n)) %>%
  pull(categoryName)

# Filter data for the top 15 categories
top_categories_data <- preprocessed_data %>%
  filter(categoryName %in% top_categories)

# Summary statistics for star ratings
summary(top_categories_data$stars)

# Plotting star ratings for the top 15 categories
ggplot(top_categories_data, aes(x = fct_reorder(categoryName, stars, .fun = median), y = stars)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(title = "Distribution of Star Ratings for Top 15 Categories",
       x = "Category",
       y = "Star Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Essential statistical analysis for top categories
top_total_ratings <- nrow(top_categories_data)
top_mean_rating <- mean(top_categories_data$stars)
top_median_rating <- median(top_categories_data$stars)
top_max_rating <- max(top_categories_data$stars)
top_min_rating <- min(top_categories_data$stars)

# Summary statistics for top categories
cat("Total number of ratings in top 15 categories:", top_total_ratings, "\n")
cat("Mean rating in top 15 categories:", top_mean_rating, "\n")
cat("Median rating in top 15 categories:", top_median_rating, "\n")
cat("Maximum rating in top 15 categories:", top_max_rating, "\n")
cat("Minimum rating in top 15 categories:", top_min_rating, "\n")

# Extract summary statistics for visualizations
summary_table <- data.frame(
  Statistics = c("Total number of ratings", "Mean rating", "Median rating", "Maximum rating", "Minimum rating"),
  Values = c(lowest_total_ratings, lowest_mean_rating, lowest_median_rating, lowest_max_rating, lowest_min_rating)
)

# Display the table
summary_table



#Question 2
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(forcats)

# Get top 15 categories based on the number of products
top_categories <- preprocessed_data %>%
  count(categoryName) %>%
  arrange(desc(n)) %>%
  slice_head(n = 15) %>%
  pull(categoryName)

# Filter data for the top 15 categories
top_categories_data <- preprocessed_data %>%
  filter(categoryName %in% top_categories)

# Plotting prices for the top 15 categories with scaled Y-axis
ggplot(top_categories_data, aes(x = fct_reorder(categoryName, price, .fun = median), y = price)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Distribution of Prices for Top 15 Categories",
       x = "Category",
       y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = quantile(top_categories_data$price, c(0.05, 0.95)))

# Essential statistical analysis for top categories
top_total_products <- nrow(top_categories_data)
top_mean_price <- mean(top_categories_data$price)
top_median_price <- median(top_categories_data$price)
top_max_price <- max(top_categories_data$price)
top_min_price <- min(top_categories_data$price)

# Summary statistics for top categories
cat("Total number of products in top 15 categories:", top_total_products, "\n")
cat("Mean price in top 15 categories:", top_mean_price, "\n")
cat("Median price in top 15 categories:", top_median_price, "\n")
cat("Maximum price in top 15 categories:", top_max_price, "\n")
cat("Minimum price in top 15 categories:", top_min_price, "\n")

# Essential statistical analysis for top categories
top_summary_table <- top_categories_data %>%
  group_by(categoryName) %>%
  summarise(
    Total_Products = n(),
    Mean_Price = mean(price),
    Median_Price = median(price),
    Max_Price = max(price),
    Min_Price = min(price)
  )

top_summary_table



#Question 3

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Scatter plot to visualize the correlation between reviews and star ratings
ggplot(preprocessed_data, aes(x = reviews, y = stars)) +
  geom_point(alpha = 0.6, size = 2, color = 'blue') +
  labs(title = "Correlation between Number of Reviews and Star Ratings",
       x = "Number of Reviews",
       y = "Star Ratings") +
  theme_minimal()

# Calculate correlation coefficient
correlation <- cor(preprocessed_data$reviews, preprocessed_data$stars)
cat("Correlation coefficient between reviews and star ratings:", correlation, "\n")

#Better visualization by applying log function to reviews
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Scatter plot to visualize the correlation between reviews and star ratings with log scaling for 'Number of Reviews'
ggplot(preprocessed_data, aes(x = log(reviews), y = stars)) +
  geom_point(alpha = 0.6, size = 2, color = 'blue') +
  labs(title = "Correlation between Log of Reviews and Star Ratings",
       x = "Log of Number of Reviews",
       y = "Star Ratings") +
  theme_minimal()

# Calculate correlation coefficient
correlation <- cor(log(preprocessed_data$reviews), preprocessed_data$stars)
cat("Correlation coefficient between reviews and star ratings:", correlation, "\n")



#Question 4
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Scatter plot to visualize the correlation between price and sales volume
ggplot(preprocessed_data, aes(x = price, y = boughtInLastMonth)) +
  geom_point(alpha = 0.6, size = 2, color = 'green') +
  labs(title = "Correlation between Price and Sales Volume",
       x = "Price",
       y = "Sales Volume") +
  theme_minimal()

# Calculate correlation coefficient
correlation <- cor(preprocessed_data$price, preprocessed_data$boughtInLastMonth)
cat("Correlation coefficient between price and sales volume:", correlation, "\n")


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Scatter plot to visualize the correlation between price and sales volume
ggplot(preprocessed_data, aes(x = price, y = boughtInLastMonth)) +
  geom_hex(alpha = 0.6, bins = 50) +
  labs(title = "Correlation between Price and Sales Volume",
       x = "Price",
       y = "Sales Volume") +
  theme_minimal()


#Question 5
# Load necessary libraries
library(ggplot2)
library(dplyr)


# Convert isBestSeller to factor
preprocessed_data$isBestSeller <- as.factor(preprocessed_data$isBestSeller)

# Fit a multiple linear regression model
model <- lm(reviews ~ price + isBestSeller + categoryName, data = preprocessed_data)

# Summary of the regression model
summary(model)

# Visualizing the regression model
# Plotting observed vs. predicted reviews
predicted_values <- predict(model, newdata = preprocessed_data)

ggplot(preprocessed_data, aes(x = reviews, y = predicted_values)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', color = 'red') +
  labs(title = "Regression Model: Observed vs. Predicted Reviews",
       x = "Observed Reviews",
       y = "Predicted Reviews") +
  theme_minimal()


#Question 6

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Perform linear regression to predict price based on stars and reviews
model <- lm(price ~ stars + reviews, data = preprocessed_data)

# Summary of the regression model
summary(model)

# Visualize the relationship between price and stars/reviews
# Scatterplot with regression line for stars
ggplot(preprocessed_data, aes(x = stars, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Price vs Star Rating", x = "Star Rating", y = "Price")

# Scatterplot with regression line for reviews
ggplot(preprocessed_data, aes(x = reviews, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Price vs Number of Reviews", x = "Number of Reviews", y = "Price")

# Statistical analysis based on the question

# Coefficients from the regression model
coefficients <- coef(model)

# R-squared value of the model
r_squared <- summary(model)$r.squared

# Adjusted R-squared value
adjusted_r_squared <- summary(model)$adj.r.squared

# Residual standard error
residual_standard_error <- summary(model)$sigma

# F-statistic and its p-value
f_statistic <- summary(model)$fstatistic[1]
f_p_value <- summary(model)$fstatistic[4]

# Print statistical analysis results
cat("Coefficients:\n")
print(coefficients)
cat("\n")

cat("R-squared: ", r_squared, "\n")
cat("Adjusted R-squared: ", adjusted_r_squared, "\n")
cat("Residual standard error: ", residual_standard_error, "\n")
cat("F-statistic: ", f_statistic, "\n")
cat("p-value: ", f_p_value, "\n")




#Question 7
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(logistf)

# Create a logistic regression model to predict if a product will be a best-seller based on star rating
model <- logistf(isBestSeller ~ stars, data = preprocessed_data)

# Summary of the logistic regression model
summary(model)

# Visualize the relationship between isBestSeller and stars
# Scatterplot with logistic regression line for stars
ggplot(preprocessed_data, aes(x = stars, y = isBestSeller)) +
  geom_point() +
  geom_smooth(method = "logistf", se = FALSE) +
  labs(title = "isBestSeller vs Star Rating", x = "Star Rating", y = "isBestSeller")

# Statistical analysis based on the question
# Coefficients from the logistic regression model
coefficients <- coef(model)

# Odds ratios for coefficients
odds_ratios <- exp(coefficients)

# AIC (Akaike Information Criterion)
AIC_value <- AIC(model)

# BIC (Bayesian Information Criterion)
BIC_value <- BIC(model)

# Print statistical analysis results
cat("Coefficients:\n")
print(coefficients)
cat("\n")

cat("Odds Ratios:\n")
print(odds_ratios)
cat("\n")

cat("AIC: ", AIC_value, "\n")
cat("BIC: ", BIC_value, "\n")


#Part 2

library(dplyr)
library(ggplot2)

# Create a logistic regression model with regularization
model_price <- glm(isBestSeller ~ jitter(price, amount = 0.1), data = preprocessed_data, family = binomial)

# Summary of the logistic regression model
summary(model_price)

# Visualize the relationship between isBestSeller and price
# Scatterplot with logistic regression line for price
ggplot(preprocessed_data, aes(x = price, y = isBestSeller)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "isBestSeller vs Price", x = "Price", y = "isBestSeller")

# Statistical analysis based on the question
# Coefficients from the logistic regression model
coefficients_price <- coef(model_price)

# Odds ratios for coefficients
odds_ratios_price <- exp(coefficients_price)

# AIC (Akaike Information Criterion)
AIC_value_price <- AIC(model_price)

# BIC (Bayesian Information Criterion)
BIC_value_price <- BIC(model_price)

# Print statistical analysis results
cat("Coefficients:\n")
print(coefficients_price)
cat("\n")

cat("Odds Ratios:\n")
print(odds_ratios_price)
cat("\n")

cat("AIC: ", AIC_value_price, "\n")
cat("BIC: ", BIC_value_price, "\n")




#Question 8
library(dplyr)
library(ggplot2)

# Fit a linear regression model
sales_model <- lm(boughtInLastMonth ~ price + categoryName + reviews, data = preprocessed_data)

# Summary of the linear regression model
summary(sales_model)

# Make predictions for next month's sales volume
preprocessed_data$predicted_sales <- predict(sales_model, newdata = preprocessed_data)

# Visualize predicted vs. actual sales volume for next month
ggplot(preprocessed_data, aes(x = predicted_sales, y = boughtInLastMonth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Predicted vs. Actual Sales Volume for Next Month",
       x = "Predicted Sales Volume", y = "Actual Sales Volume")

# Statistical analysis based on the question
# Coefficients from the linear regression model
coefficients_sales <- coef(sales_model)

# Confidence intervals for coefficients
conf_intervals <- confint(sales_model)

# AIC (Akaike Information Criterion)
AIC_value_sales <- AIC(sales_model)

# BIC (Bayesian Information Criterion)
BIC_value_sales <- BIC(sales_model)

# Print statistical analysis results
cat("Model Coefficients:\n")
print(coefficients_sales)
cat("\n")

cat("Confidence Intervals for Coefficients:\n")
print(conf_intervals)
cat("\n")

cat("AIC: ", AIC_value_sales, "\n")
cat("BIC: ", BIC_value_sales, "\n")


