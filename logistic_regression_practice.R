install.packages("caret")
install.packages("ggplot2")

# Load necessary libraries
library(caret)    # For data splitting and model evaluation
library(ggplot2)  # For visualization (optional)

# Sample dataset
data <- data.frame(
  Size = c(2000, 1500, 2500, 1800, 2200),
  Bedrooms = c(3, 2, 4, 3, 3),
  Age = c(10, 20, 5, 15, 8),
  Location_Score = c(8, 6, 9, 7, 9),
  Expensive = c(1, 0, 1, 0, 1)  # Binary target variable
)

# View the dataset
print(data)

# Split data into training and testing sets
set.seed(42)  # For reproducibility
trainIndex <- createDataPartition(data$Expensive, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Define the model formula
model_formula <- Expensive ~ Size + Bedrooms + Age + Location_Score

# Train the logistic regression model
model <- glm(model_formula, data = trainData, family = binomial)

# Summary of the model
summary(model)

# Make predictions on the test set
predictions_prob <- predict(model, newdata = testData, type = "response")
predictions <- ifelse(predictions_prob > 0.5, 1, 0)

# Evaluate the model
# conf_matrix <- confusionMatrix(as.factor(predictions), as.factor(testData$Expensive))
# print(conf_matrix)

# Predicting class for a new house
new_house <- data.frame(Size = 2100, Bedrooms = 3, Age = 12, Location_Score = 8)
predicted_prob <- predict(model, newdata = new_house, type = "response")
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)

# Print the predicted class and probability for the new house
cat("Predicted Probability for the new house:", predicted_prob, "\n")
cat("Predicted Class for the new house:", predicted_class, "\n")

# Create a new data frame for plotting the decision boundary
size_range <- seq(min(data$Size), max(data$Size), length.out = 100)
plot_data <- data.frame(Size = size_range,
                        Bedrooms = mean(data$Bedrooms),
                        Age = mean(data$Age),
                        Location_Score = mean(data$Location_Score))
plot_data$Probability <- predict(model, newdata = plot_data, type = "response")

# Plotting the decision boundary
ggplot(data, aes(x = Size, y = Expensive)) +
  geom_point(aes(color = factor(Expensive)), size = 3) +
  geom_line(data = plot_data, aes(x = Size, y = Probability), color = 'blue', size = 1) +
  labs(title = "Logistic Regression: Probability vs. Size",
       x = "Size (sqft)",
       y = "Probability of Being Expensive") +
  theme_minimal() +
  scale_color_manual(values = c("red", "green"), labels = c("Not Expensive", "Expensive"))
