install.packages("tidyverse")
install.packages("caret")
install.packages("ggplot2")
install.packages("broom") # For tidying model outputs

# Load necessary libraries
library(tidyverse)
library(caret)
library(broom)

# Load the data from a CSV file
# data: https://www.kaggle.com/datasets/henryshan/meta-platforms-inc-meta
data <- read_csv("C:/Users/scgoo/OneDrive/Documents/colleges/Jacksonville/Fall 2024/Probability/week 3/archive/META.csv")

# View the first few rows of the data to understand its structure
print(head(data))

# Check the structure of the dataset
str(data)

# Split the data into training and testing sets
set.seed(42) # For reproducibility
trainIndex <- createDataPartition(data$Open, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Define the linear model
model <- lm(Open ~ High + Low + Close + Volume, data = trainData)

# Summary of the model
summary(model)

# Make predictions on the test data
predictions <- predict(model, newdata = testData)

# Evaluate the model
actuals <- testData$Open
mse <- mean((predictions - actuals)^2)
print(paste("Mean Squared Error:", mse))

# View predictions and actual values
results <- tibble(
  Actual = actuals,
  Predicted = predictions
)
print(results)

# Create synthetic datasets
# Linear Regression Dataset
linear_data <- tibble(
  x = seq(-10, 10, length.out = 100),
  y = 2 * x + 3 + rnorm(100, sd = 2)  # Linear relationship with noise
)

# Nonlinear Regression Dataset
nonlinear_data <- tibble(
  x = seq(-10, 10, length.out = 100),
  y = 5 * sin(x) + rnorm(100, sd = 2)  # Sinusoidal relationship with noise
)

# Logistic Regression Dataset
set.seed(42)
logistic_data <- tibble(
  x = seq(-10, 10, length.out = 100),
  y = as.factor(ifelse(1 / (1 + exp(-(0.5 * x + 2))) > 0.5, 1, 0))  # Logistic relationship
)

# Train Linear Regression Model
linear_model <- lm(y ~ x, data = linear_data)

# Plot Linear Regression - simulated
ggplot(linear_data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression", x = "x", y = "y") +
  theme_minimal()


# Train Nonlinear Regression Model (using polynomial regression as an example)
nonlinear_model <- lm(y ~ poly(x, 3), data = nonlinear_data)

# Predict values for plotting
nonlinear_data$predicted_y <- predict(nonlinear_model, newdata = nonlinear_data)

# Plot Nonlinear Regression - simulated
ggplot(nonlinear_data, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = predicted_y), color = "red") +
  labs(title = "Nonlinear Regression (Polynomial)", x = "x", y = "y") +
  theme_minimal()


# Train Logistic Regression Model
logistic_model <- glm(y ~ x, data = logistic_data, family = binomial)

# Predict probabilities for plotting
logistic_data$predicted_prob <- predict(logistic_model, newdata = logistic_data, 
                                        type = "response")

# Plot Logistic Regression - simulated
ggplot(logistic_data, aes(x = x, y = predicted_prob)) +
  geom_point(aes(color = y), shape = 21) +
  geom_line(aes(y = predicted_prob), color = "blue") +
  labs(title = "Logistic Regression", x = "x", y = "Predicted Probability") +
  theme_minimal()



# Linear Regression with Actual Data
ggplot(linear_data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression", x = "x", y = "y") +
  theme_minimal()

# Nonlinear Regression with Actual Data
ggplot(nonlinear_data, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = y), color = "red") +
  labs(title = "Nonlinear Regression (Polynomial)", x = "x", y = "y") +
  theme_minimal()

# Logistic Regression with Actual Data
ggplot(logistic_data, aes(x = x, y = y)) +
  geom_point(aes(color = y), shape = 21) +
  geom_line(aes(y = y), color = "blue") +
  labs(title = "Logistic Regression", x = "x", y = "y") +
  theme_minimal()
