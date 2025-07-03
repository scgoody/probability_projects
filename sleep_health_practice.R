# Load necessary libraries
library(tidyverse)
# Load the data
# data: https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset
data <- read.csv("C:/Users/scgoo/OneDrive/Documents/colleges/Jacksonville/Fall 2024/Probability/week 5/Sleep_health_and_lifestyle_dataset.csv")
# Basic exploration
str(data)
summary(data)


# Visualization: Sleep Duration by Occupation
ggplot(data, aes(x = Occupation, y = Sleep.Duration)) +
  geom_boxplot() +
  labs(title = "Sleep␣Duration␣by␣Occupation", y = "Sleep␣Duration␣(hours)", x = "Occupation") +
  theme_minimal()
# Scatter plot: Sleep Duration vs Quality of Sleep
ggplot(data, aes(x = Sleep.Duration, y = Quality.of.Sleep)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Sleep␣Duration␣vs␣Quality␣of␣Sleep", x = "Sleep␣Duration", y = "Quality␣of␣Sleep
 ") +
  theme_minimal()
# Scatter plot: Physical Activity Level vs Stress Level
ggplot(data, aes(x = Physical.Activity.Level, y = Stress.Level)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Physical␣Activity␣Level␣vs␣Stress␣Level", x = "Physical␣Activity␣Level", y = "
 Stress␣Level") +
  theme_minimal()


# Select numeric columns for correlation matrix
numeric_data <-data %>% select(Sleep.Duration, Quality.of.Sleep, Physical.Activity.Level, Stress.Level, Heart.Rate, Daily.Steps)
# Correlation matrix
correlation_matrix <-cor(numeric_data, use = "complete.obs")
# Visualize the correlation matrix
#install.packages("corrplot")
library(corrplot)
corrplot(correlation_matrix, method = "circle", type = "upper")


# Build a linear regression model: Sleep Duration ~ Quality of Sleep + Physical Activity Level
model <-lm(Sleep.Duration ~ Quality.of.Sleep + Physical.Activity.Level, data = data)
# Summary of the model
summary(model)
# Visualize regression results for Sleep Duration and Quality of Sleep
ggplot(data, aes(x = Quality.of.Sleep, y = Sleep.Duration)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regression:␣Sleep␣Duration␣vs␣Quality␣of␣Sleep", x = "Quality␣of␣Sleep", y = "
 Sleep␣Duration") +
  theme_minimal()




multiple_model <- lm(Sleep.Duration ~ Quality.of.Sleep + Physical.Activity.Level + Stress.Level + Heart.Rate + Daily.Steps, data = data)

# Summary of the multiple regression model
summary(multiple_model)

# Visualize regression results for Sleep Duration vs Quality of Sleep
ggplot(data, aes(x = Quality.of.Sleep, y = Sleep.Duration)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regression: Sleep Duration vs Quality of Sleep", 
       x = "Quality of Sleep", y = "Sleep Duration") +
  theme_minimal()

# Residual plot
ggplot(data, aes(x = multiple_model$fitted.values, y = multiple_model$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Histogram of residuals
ggplot(data, aes(x = multiple_model$residuals)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()

# QQ plot for normality of residuals
qqnorm(multiple_model$residuals)
qqline(multiple_model$residuals, col = "red", lwd = 2)  


# QQ plot for normality of residuals from the simple linear regression
qqnorm(model$residuals)
qqline(model$residuals, col = "blue", lwd = 2)  
# Perform ANOVA for the simple model
anova_simple <- anova(model)
print(anova_simple)

# Perform ANOVA for the multiple model
anova_multiple <- anova(multiple_model)
print(anova_multiple)

