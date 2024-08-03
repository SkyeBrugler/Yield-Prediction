# Yield-Prediction

## Description
This project includes two Excel files designed for testing and training a Random Forest model:

Yield and Site Data: Contains simulated example data on yield and site characteristics for model training and evaluation.
Site Information: Provides additional simulated site data used for making predictions with the trained Random Forest model.
The goal of this project is to showcase how machine learning models can be effectively integrated into farming operations in a user-friendly manner.

## Files Included
**Github_CSV_example.xlsx**: Contains historical data for training and testing the Random Forest model. Replace the example data with your own dataset for improved accuracy. Providing data from multiple years will enhance the model's performance and prediction reliability.

**Prediction_information.xlsx**: Includes site-specific information for the target year and field where you wish to make predictions. Ensure that this file contains the same variables used in training and testing the Random Forest model.

**script.R**: Features the R code required to run the Random Forest model. You will need to have R and RStudio installed to execute this script effectively.

## Installation
install.packages("dplyr")
install.packages("ggplot2")
install.packages("randomForest")
install.packages("caret")
