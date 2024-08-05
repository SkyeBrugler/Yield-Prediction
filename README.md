# **Yield Prediction Model with Random Forest - README**

Welcome to the Yield Prediction Model with Random Forest! This repository contains a UI application designed to predict crop yields using Random Forest models. Below are the details of the files included and instructions on how to use the application.

## **Files Included**

1. **Example_Data_For_Model.xlsx**
   - This file includes example data that can be used to train and test the model.
   - Users are encouraged to replace the example data with their own data for model training and testing.

2. **Example_Data_for_Prediction.xlsx**
   - This file contains example data for making predictions.
   - Users are invited to replace the example data with their own data for prediction purposes.

## **Application Overview**

The UI application allows you to:
1. Upload files for training and testing the model.
2. Upload files for making predictions.
3. Set the training and testing ratio (e.g., entering 0.8 means training with 80% of your data).
4. Specify the price of corn and the cost of nitrogen (N).

## **Running the Analysis**

Once the data is uploaded and the parameters are set, you can run the analysis to obtain:
- Metrics on the training and testing performance of the model.
- Predicted yields for the site.
- Economic Optimum Nitrogen Rate (EONR) and yield at EONR.
- Quadratic plateau plot and variable importance.

## **Variable Importance**

- The variable importance plot helps you understand the significance of each variable in the model.
- You can add or remove variables before analysis using a checkbox provided at the top of the UI.

## **Downloading Predictions**

- After running the analysis, you have the option to download an Excel sheet of your predictions.

## Installation
install.packages("dplyr")
install.packages("ggplot2")
install.packages("randomForest")
install.packages("caret")
