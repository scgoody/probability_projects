# Load necessary libraries
library(ggplot2)  # For visualization
library(caret)    # For data splitting and model evaluation

# Sample dataset
data <- data.frame(
  Size = c(2000, 1500, 2500, 1800, 2200),
  Bedrooms = c(3, 2, 4, 3, 3),
  Age = c(10, 20, 5, 15, 8),
  Location_Score = c(8, 6, 9, 7, 9),
  Price = c(400000, 250000, 500000, 320000, 450000)
)

# View the dataset
print(data)

# Split data into training and testing sets
set.seed(42)  # For reproducibility
trainIndex <- createDataPartition(data$Price, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Define the model formula
model_formula <- Price ~ Size + Bedrooms + Age + Location_Score

# Train the linear regression model
model <- lm(model_formula, data = trainData)

# Summary of the model
summary(model)

# Make predictions on the test set
predictions <- predict(model, newdata = testData)

# Evaluate the model
mse <- mean((testData$Price - predictions)^2)
r2 <- summary(model)$r.squared

# Print evaluation metrics
cat("Mean Squared Error:", mse, "\n")
cat("R-squared:", r2, "\n")

# Predicting price for a new house
new_house <- data.frame(Size = 2100, Bedrooms = 3, Age = 12, Location_Score = 8)
predicted_price <- predict(model, newdata = new_house)

# Print the predicted price for the new house
cat("Predicted Price for the new house:", predicted_price, "\n")

ggplot(testData, aes(x = Price, y = predicted_price)) +
  geom_point(aes(color = 'Actual vs Predicted'), size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', color = 'red') +
  labs(title = "Predicted vs. Actual Prices",
       x = "Actual Price ($)",
       y = "Predicted Price ($)") +
  theme_minimal() +
  scale_color_manual(values = c('Actual vs Predicted' = 'blue'))
