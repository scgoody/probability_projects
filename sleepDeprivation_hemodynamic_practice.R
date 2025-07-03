# Install necessary libraries if they are not already installed
packages <- c("readxl", "dplyr", "ggplot2", "summarytools", "tidyr", "corrplot")

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) {
  install.packages(new_packages)
}

# Load necessary libraries
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(ggplot2)     # For data visualization
library(tidyr)       # For tidying data
library(summarytools) # For descriptive statistics

# Set the path to your Excel file
# data = https://physionet.org/content/sleep-dep-hemo-cog/1.0.0/SD_CANTAB_DB_public.csv
file_path <- "C:/Users/scgoo/OneDrive/Documents/colleges/Jacksonville/Fall 2024/Probability/week 10/effect-of-24-hour-sleep-deprivation-on-cerebral-hemodynamics-and-cognitive-performance-1.0.0/SD_CANTAB_DB_public.csv"

# Read the Excel file
data <- read.csv(file_path)

# Display the first few rows of the data
print(head(data))

# Produce descriptive statistics
descriptive_stats <- dfSummary(data)

# View descriptive statistics
print(descriptive_stats)

# Visualizing the data
# Example: Histograms for numeric columns
numeric_columns <- sapply(data, is.numeric)
data_numeric <- data[, numeric_columns]

# Create histograms for each numeric column
for (col in names(data_numeric)) {
  ggplot(data_numeric, aes_string(x = col)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histogram of", col), x = col, y = "Frequency") +
    theme_minimal() +
    ggsave(filename = paste0(col, "_histogram.png"))
}

# Example: Boxplots for numeric columns
for (col in names(data_numeric)) {
  ggplot(data_numeric, aes_string(x = col)) +
    geom_boxplot(fill = "lightblue", outlier.colour = "red") +
    labs(title = paste("Boxplot of", col), x = col, y = "Values") +
    theme_minimal() +
    ggsave(filename = paste0(col, "_boxplot.png"))
}

# Example: Correlation plot if there are multiple numeric columns
if (ncol(data_numeric) > 1) {
  corr_matrix <- cor(data_numeric, use = "pairwise.complete.obs")
  library(corrplot) # Install if not already done
  corrplot(corr_matrix, method = "circle")
}


# Focused Correlation Matrix
selected_data <- data_numeric[, c("DMSCC", "DMSL0SD","DMSL12SD",
                                  "DMSL4SD","DMSLADSD","DMSLSD","DMSLSSD",
                                  "DMSMDL", "DMSMDL0","DMSMDL12","DMSMDL4",
                                  "DMSMDLAD", "DMSMDLS","DMSML","DMSML0",
                                  "DMSML12", "DMSML4","DMSMLAD","DMSMLS")]
cor_matrix <- cor(selected_data, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45)
corrplot(cor_matrix, method = "circle")


# DMSML
linear_model <- lm(DMSML ~ DMSMDL + DMSMLAD, data = data_numeric)
summary(linear_model)
linear_model <- lm(DMSML ~ DMSMDL + DMSMLAD + DMSMDLAD + DMSMDLS 
                   + DMSML4 + DMSMLS + DMSML0 + DMSMDL4 + DMSMDL0, 
                   data = data_numeric)
summary(linear_model)
ggplot(data_numeric, aes(x = DMSMDL, y = DMSML)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Linear Regression: DMSML vs DMSMDL",
       x = "DMSMDL", y = "DMSML") +
  theme_minimal()
ggplot(data_numeric, aes(x = DMSMLAD, y = DMSML)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Linear Regression: DMSML vs DMSMLAD",
       x = "DMSMLAD", y = "DMSML") +
  theme_minimal()

# DMSMLAD
linear_model <- lm(DMSMLAD ~ DMSML + DMSMDLAD, data = data_numeric)
summary(linear_model)
linear_model <- lm(DMSMLAD ~ DMSML + DMSMDLAD + DMSML4 + DMSMDL4 
                   + DMSMDL + DMSML0 + DMSMDL0 + DMSMDL12 + DMSML12, 
                   data = data_numeric)
summary(linear_model)
ggplot(data_numeric, aes(x = DMSML, y = DMSMLAD)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Linear Regression: DMSMLAD vs DMSML",
       x = "DMSML", y = "DMSMLAD") +
  theme_minimal()
ggplot(data_numeric, aes(x = DMSMDLAD, y = DMSMLAD)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Linear Regression: DMSMLAD vs DMSMDLAD",
       x = "DMSMDLAD", y = "DMSMLAD") +
  theme_minimal()



# DMSMDL
linear_model <- lm(DMSMDL ~ DMSML + DMSMDLAD, data = data_numeric)
summary(linear_model)
linear_model <- lm(DMSMDL ~ DMSML + DMSMDLAD + DMSMDLS + DMSMDL0 
                   + DMSMLAD + DMSMLS, data = data_numeric)
summary(linear_model)
ggplot(data_numeric, aes(x = DMSML, y = DMSMDL)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Linear Regression: DMSMDL vs DMSML",
       x = "DMSML", y = "DMSMDL") +
  theme_minimal()
ggplot(data_numeric, aes(x = DMSMDLAD, y = DMSMDL)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Linear Regression: DMSMDL vs DMSMDLAD",
       x = "DMSMDLAD", y = "DMSMDL") +
  theme_minimal()




# DMSML4
linear_model <- lm(DMSML4 ~ DMSMDL4 + DMSMLAD, data = data_numeric)
summary(linear_model)
linear_model <- lm(DMSML4 ~ DMSMDL4 + DMSMLAD + DMSMDLAD + DMSML 
                   + DMSML4, data = data_numeric)
summary(linear_model)
ggplot(data_numeric, aes(x = DMSMDL4, y = DMSML4)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Linear Regression: DMSML4 vs DMSMDL4",
       x = "DMSMDL4", y = "DMSML4") +
  theme_minimal()
ggplot(data_numeric, aes(x = DMSMLAD, y = DMSML4)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Linear Regression: DMSML4 vs DMSMLAD",
       x = "DMSMLAD", y = "DMSML4") +
  theme_minimal()



