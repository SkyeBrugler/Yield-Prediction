# Data cleaning and processing --------------------------------------------
library(readxl)
library(dplyr)

# Reading the data set
data.1 <- read_excel("Github_CSV_example.xlsx")

# Display the structure of the new dataframe
str(data.1)
df<-data.1

# Function to calculate Z-scores and filter outliers
filter_outliers <- function(data, column_name, threshold = 2) {
  data <- data.1 %>%
    group_by(site) %>%
    mutate(Z_score = (get(column_name) - mean(get(column_name), na.rm = TRUE)) / sd(get(column_name), na.rm = TRUE)) %>%
    ungroup()
  
  # Identify outliers
  outliers <- data %>%
    filter(Z_score > threshold | Z_score < -threshold)
  
  # Filter out outliers
  filtered_data <- data %>%
    filter(Z_score <= threshold & Z_score >= -threshold)
  
  return(list(filtered_data = filtered_data, outliers = outliers))
}

library(dplyr)

# Apply the function to your data
result <- filter_outliers(data, "Yield_bu_ac")

# Extract filtered data and outliers
filtered_data <- result$filtered_data
outliers <- result$outliers

df<-filtered_data


#Create a subset of the dataset to remove and add variables from dataset
library(dplyr)

# Define the required columns
required_columns <- c("site", "year", "Nrate_lbs_ac", "Yield_bu_ac", 
                      "SOM", "Seeding_rate", "pH", "CEC", "Bd", "avg_wind_speed", 
                      "sol_rad", "max_air_temp", "min_air_temp", "avg_air_temp", 
                      "max_rel_hum", "min_rel_hum", "avg_rel_hum", 
                      "avg_dewpt_temp", "precip")

# Create the new dataframe with the required columns
subset_df <- df %>% 
  select(all_of(required_columns))

# Convert relevant columns to numeric
# Here, assuming 'site' is a factor and 'year' is integer, and the rest should be numeric
subset_df <- subset_df %>%
  mutate(across(c(Nrate_lbs_ac, Yield_bu_ac, SOM, Seeding_rate, pH, CEC, Bd, avg_wind_speed, 
                  sol_rad, max_air_temp, min_air_temp, avg_air_temp, max_rel_hum, min_rel_hum, 
                  avg_rel_hum, avg_dewpt_temp, precip), as.numeric))

# Print the structure of the new dataframe
str(subset_df)

# Optionally, print the new dataframe to see the first few rows
print(head(subset_df))




# Training and Testing Data sets ------------------------------------------

# Load the 'caret' package
library(caret)

#80% training and 20% Testing
train_idx <- createDataPartition(subset_df$Yield_bu_ac, p = 0.8, list = FALSE)
train_data <- subset_df[train_idx, ]
test_data <- subset_df[-train_idx, ]


# Check sizes of training and testing datasets
cat("Size of training data:", nrow(train_data), "\n")
cat("Size of testing data:", nrow(test_data), "\n")


# Check which columns have NA values in train_data
na_columns_train <- sapply(train_data, function(x) any(is.na(x)))

# Check which columns have NA values in test_data
na_columns_test <- sapply(test_data, function(x) any(is.na(x)))

# Identify critical columns that have NA values in either train_data or test_data
critical_columns <- names(which(na_columns_train | na_columns_test))

# Print critical columns
print(critical_columns)

# Remove rows with NA values in the specified columns for train_data
train_data_clean <- train_data[complete.cases(train_data[, critical_columns]), ]

# Remove rows with NA values in the specified columns for test_data
test_data_clean <- test_data[complete.cases(test_data[, critical_columns]), ]

# Print the cleaned datasets to verify
print(head(train_data_clean))
print(head(test_data_clean))


# Check if any missing values are left
train_data_clean<-train_data
test_data_clean<-test_data
summary(train_data_clean)
summary(test_data_clean)


# RF model ----------------------------------------------------------------

# Load necessary libraries
library(randomForest)

str(train_data_clean)

# Fit the random forest model using the training data
rf_model <- randomForest(Yield_bu_ac ~ ., data = train_data_clean, ntree = 300)

# Get predictions from the model using both training and test data
train_predictions <- predict(rf_model, newdata = train_data_clean)
test_predictions <- predict(rf_model, newdata = test_data_clean)

# Extract observed values from the cleaned data
train_observed <- train_data_clean$Yield_bu_ac
test_observed <- test_data_clean$Yield_bu_ac

# Remove NA values for accurate calculations
valid_train_indices <- !is.na(train_predictions) & !is.na(train_observed)
valid_test_indices <- !is.na(test_predictions) & !is.na(test_observed)

train_predictions <- train_predictions[valid_train_indices]
train_observed <- train_observed[valid_train_indices]
test_predictions <- test_predictions[valid_test_indices]
test_observed <- test_observed[valid_test_indices]

# Calculate MAE for both training and test data
train_mae <- mean(abs(train_observed - train_predictions))
test_mae <- mean(abs(test_observed - test_predictions))

# Calculate RMSE for both training and test data
train_rmse <- sqrt(mean((train_observed - train_predictions)^2))
test_rmse <- sqrt(mean((test_observed - test_predictions)^2))

# Calculate R-squared (R²) for both training and test data
train_r_squared <- cor(train_observed, train_predictions)^2
test_r_squared <- cor(test_observed, test_predictions)^2

# Print the calculated metrics
cat("Training Mean Absolute Error (MAE):", round(train_mae, 2), "\n")
cat("Test Mean Absolute Error (MAE):", round(test_mae, 2), "\n")
cat("Training Root Mean Squared Error (RMSE):", round(train_rmse, 2), "\n")
cat("Test Root Mean Squared Error (RMSE):", round(test_rmse, 2), "\n")
cat("Training R-squared (R²):", round(train_r_squared, 2), "\n")
cat("Test R-squared (R²):", round(test_r_squared, 2), "\n")


# Create a data frame with the metrics
metrics_df <- data.frame(
  Metric = c("Mean Absolute Error (MAE)", "Root Mean Squared Error (RMSE)", "R-squared (R²)"),
  Training = c(round(train_mae, 2), round(train_rmse, 2), round(train_r_squared, 2)),
  Test = c(round(test_mae, 2), round(test_rmse, 2), round(test_r_squared, 2))
)

# Print the metrics as a table
print(knitr::kable(metrics_df, caption = "Model Performance Metrics"))




#Produce Prediction ------------------------------------------

data.2<- read_excel("Prediction_information.xlsx")

# Random Forest (RF) model prediction for 2023
rf_prediction <- predict(rf_model,data.2)

# Assuming 'data.2' has columns 'site' and 'Nrate_lbs_ac'
# Adding a column for predictions
data.2$Prediction <- rf_prediction

# Select only the relevant columns for display
prediction_results <- data.2 %>%
  select(site, Nrate_lbs_ac, Prediction)

# Display the prediction results
print(prediction_results,n=42)


# VIP plots ---------------------------------------------------------------

# Extract variable importance
importance_values <- importance(rf_model)
print(importance_values)

# Load necessary libraries
library(randomForest)
library(ggplot2)

# Assuming you have the trained random forest model 'rf_model'
# Extract variable importance
importance_values <- importance(rf_model)

# Convert importance values to a data frame
importance_df <- as.data.frame(importance_values)

# Add variable names as a new column
importance_df$Variable <- rownames(importance_df)

# Sort the data frame by IncNodePurity
importance_df <- importance_df[order(importance_df$IncNodePurity, decreasing = TRUE), ]

# Create a bar plot of variable importance
importance_plot <- ggplot(importance_df, aes(x = reorder(Variable, IncNodePurity), y = IncNodePurity)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variable", y = "IncNodePurity", title = "Variable Importance") +
  theme_minimal()

# Display the plot
print(importance_plot)




# Plot scater with rmse, r2 and mae ---------------------------------------

# Assuming you have the observed (actual) values 'observed_values_clean' and predicted values 'predicted_values'

# Load the necessary library
library(ggplot2)

# Create a data frame for the plot
plot_data <- data.frame(Observed = test_observed, Predicted = test_predictions)

# Create scatter plot
scatter_plot <- ggplot(plot_data, aes(x = Observed, y = Predicted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Observed Values", y = "Predicted Values",
       title = paste("Scatter Plot of Observed vs Predicted\n",
                     "RMSE:", round(test_rmse, 2),
                     "R²:", round(test_r_squared, 2),
                     "MAE:", round(test_mae, 2))) +
  theme_minimal()

# Display the plot
print(scatter_plot)


# Plot Residuals ----------------------------------------------------------


# Create the residuals
residuals <- test_observed - test_predictions

# Create the residual plot
residual_plot <- ggplot(data.frame(Observed = test_observed, Residuals = residuals), aes(x = Observed, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Observed", y = "Residuals", title = "Residual Plot") +
  theme_minimal()

# Display the residual plot
print(residual_plot)



# Plot Confidence Intervals  ----------------------------------------------

library(ggplot2)

# Create the line plot with confidence intervals
line_plot <- ggplot(plot_data, aes(x = 1:length(Observed))) +
  geom_line(aes(y = Observed, color = "Observed"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
  geom_ribbon(aes(ymin = Predicted - 1.96 * sd(Predicted), ymax = Predicted + 1.96 * sd(Predicted), fill = "Confidence Interval"),
              alpha = 0.5) +
  labs(x = "Index", y = "Value", title = "Line Plot with Confidence Intervals: Observed vs. Predicted") +
  scale_color_manual(name = "Legend", values = c("Observed" = "blue", "Predicted" = "red")) +
  scale_fill_manual(name = "Legend", values = c("Confidence Interval" = "pink")) +
  theme_minimal()

# Display the line plot with confidence intervals
print(line_plot)


