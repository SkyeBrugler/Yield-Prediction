library(shiny)
library(readxl)
library(dplyr)
library(caret)
library(randomForest)
library(knitr)
library(ggplot2)
library(writexl)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Yield Prediction Model with Random Forest"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Choose Excel File for Model", accept = c(".xlsx")),
      fileInput("predfile", "Choose Excel File for Predictions", accept = c(".xlsx")),
      
      checkboxGroupInput("variables", "Select Variables to Include", 
                         choices = NULL, selected = NULL),
      
      numericInput("split_ratio", "Training/Testing Split Ratio", value = 0.8, min = 0.5, max = 0.9, step = 0.05),
      
      checkboxInput("stratify", "Use Stratification in Train/Test Split", value = FALSE),
      
      numericInput("corn_price", "Enter Corn Price ($/bu)", value = 1),
      numericInput("n_price", "Enter Nitrogen Price ($/lb)", value = 0.1),
      
      actionButton("run_analysis", "Run Analysis"),
      downloadButton("downloadData", "Download Predictions")
    ),
    
    mainPanel(
      h3("Model Performance Metrics"),
      tableOutput("metricsTable"),
      
      h3("Predicted Yields"),
      tableOutput("predictionTable"),
      
      h3("EONR and Yield at EONR"),
      verbatimTextOutput("eonrOutput"),
      
      h3("Predicted Yield vs. N Rate Plot"),
      plotOutput("yieldPlot"),
      
      h3("Variable Importance Plot"),
      plotOutput("importancePlot"),
      
      h3("Stratification"),
      plotOutput("Stratification")
    )
  )
)

# Define server logic required to run the model and generate predictions
server <- function(input, output, session) {
  
  # Reactive expression to load data
  dataInput <- reactive({
    req(input$datafile)
    read_excel(input$datafile$datapath)
  })
  
  # Update variable selection based on uploaded data
  observe({
    data <- dataInput()
    updateCheckboxGroupInput(session, "variables", choices = names(data), selected = names(data))
  })
  
  # Reactive expression for processing data
  processedData <- reactive({
    data <- dataInput()
    
    # Filter selected variables
    selected_data <- data %>% select(all_of(input$variables))
    
    # Clean the data (remove NAs)
    clean_data <- na.omit(selected_data)
    
    return(clean_data)
  })
  
  # Reactive expression for train/test split
  splitData <- reactive({
    data <- processedData()
    split_ratio <- input$split_ratio
    stratify <- input$stratify
    
    if (stratify && "site" %in% colnames(data)) {
      train_idx <- createDataPartition(data$site, p = split_ratio, list = FALSE)
    } else {
      train_idx <- sample(seq_len(nrow(data)), size = round(nrow(data) * split_ratio))
    }
    
    train_data <- data[train_idx, ]
    test_data <- data[-train_idx, ]
    
    return(list(train_data = train_data, test_data = test_data))
  })
  
  # Reactive expression for training the model
  rfModel <- eventReactive(input$run_analysis, {
    split <- splitData()
    train_data <- split$train_data
    
    rf_model <- randomForest(Yield_bu_ac ~ ., data = train_data, ntree = 300)
    
    return(rf_model)
  })
  
  # Reactive expression for predictions and EONR calculation
  predictions <- reactive({
    req(input$predfile)
    new_data <- read_excel(input$predfile$datapath)
    
    model <- rfModel()
    pred <- predict(model, new_data)
    new_data$Predicted_Yield <- pred
    
    # Fit the quadratic model
    fit_quadratic_model <- function(data) {
      model <- lm(Predicted_Yield ~ Nrate_lbs_ac + I(Nrate_lbs_ac^2), data = data)
      coeff <- summary(model)$coefficients
      
      # Extract and label the coefficients
      named_coeff <- c(
        a = coeff["(Intercept)", "Estimate"],
        b = coeff["Nrate_lbs_ac", "Estimate"],
        c = coeff["I(Nrate_lbs_ac^2)", "Estimate"]
      )
      
      return(named_coeff)
    }
    
    coefficients_site <- fit_quadratic_model(new_data)
    
    # Extract user inputs for corn price and nitrogen price
    corn_price <- input$corn_price
    n_price <- input$n_price
    
    # Calculate EONR using the updated formula
    EONR <- (n_price - (coefficients_site["b"] * corn_price)) / (2 * coefficients_site["c"] * corn_price)
    
    # Calculate yield at EONR
    Yield_at_EONR <- coefficients_site["a"] + coefficients_site["b"] * EONR + coefficients_site["c"] * EONR^2
    
    # Store EONR and Yield at EONR in the reactive expression
    eonr_values <- list(EONR = EONR, Yield_at_EONR = Yield_at_EONR)
    
    # Return predictions along with EONR and Yield at EONR
    return(list(predictions = new_data, eonr = eonr_values))
  })
  
  # Calculate model performance metrics
  output$metricsTable <- renderTable({
    split <- splitData()
    train_data <- split$train_data
    test_data <- split$test_data
    
    model <- rfModel()
    
    train_pred <- predict(model, train_data)
    test_pred <- predict(model, test_data)
    
    train_metrics <- c(
      MAE = mean(abs(train_data$Yield_bu_ac - train_pred)),
      RMSE = sqrt(mean((train_data$Yield_bu_ac - train_pred)^2)),
      R_Squared = cor(train_data$Yield_bu_ac, train_pred)^2
    )
    
    test_metrics <- c(
      MAE = mean(abs(test_data$Yield_bu_ac - test_pred)),
      RMSE = sqrt(mean((test_data$Yield_bu_ac - test_pred)^2)),
      R_Squared = cor(test_data$Yield_bu_ac, test_pred)^2
    )
    
    metrics_df <- data.frame(
      Metric = c("Mean Absolute Error (MAE)", "Root Mean Squared Error (RMSE)", "R-squared (R²)"),
      Training = round(train_metrics, 2),
      Test = round(test_metrics, 2)
    )
    
    return(metrics_df)
  })
  
  # Output predicted yields table without R²
  output$predictionTable <- renderTable({
    pred_data <- predictions()$predictions
    
    pred_data %>% select(site, Nrate_lbs_ac, Predicted_Yield)
  })
  
  # Output EONR and Yield at EONR
  output$eonrOutput <- renderPrint({
    eonr_values <- predictions()$eonr
    
    cat("EONR:", round(eonr_values$EONR, 2), "lbs/ac\n")
    cat("Yield at EONR:", round(eonr_values$Yield_at_EONR, 2), "bu/ac\n")
  })
  
  # Plot Predicted Yield vs. N Rate
  output$yieldPlot <- renderPlot({
    pred_data <- predictions()$predictions
    
    ggplot(pred_data, aes(x = Nrate_lbs_ac, y = Predicted_Yield)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +
      labs(title = "Predicted Yield vs. N Rate",
           x = "N Rate (lbs/ac)",
           y = "Predicted Yield (bu/ac)")
  })
  
  # Reactive expression for variable importance
  variableImportance <- reactive({
    model <- rfModel()
    importance <- randomForest::importance(model)
    
    # Check structure of importance matrix
    print("Importance Matrix Structure:")
    print(str(importance))
    
    # Handle different structures of importance matrix
    if ("MeanDecreaseGini" %in% colnames(importance)) {
      importance_df <- data.frame(Variable = rownames(importance), Importance = importance[, "MeanDecreaseGini"])
    } else if ("IncNodePurity" %in% colnames(importance)) {
      importance_df <- data.frame(Variable = rownames(importance), Importance = importance[, "IncNodePurity"])
    } else {
      stop("No recognized importance metric found in importance matrix")
    }
    
    importance_df <- importance_df[order(-importance_df$Importance), ]
    return(importance_df)
  })
  
  # Plot Variable Importance
  output$importancePlot <- renderPlot({
    importance_df <- variableImportance()
    
    ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Variable Importance", x = "Variables", y = "Importance")
  })
  
  # New density plot for stratification visualization
  output$Stratification <- renderPlot({
    split <- splitData()
    train_data <- split$train_data
    test_data <- split$test_data
    
    if ("site" %in% colnames(train_data)) {
      combined_data <- bind_rows(
        mutate(train_data, Dataset = "Training"),
        mutate(test_data, Dataset = "Testing")
      )
      
      ggplot(combined_data, aes(x = site, fill = Dataset)) +
        geom_bar(position = "dodge") +
        labs(title = "Stratification by Site",
             x = "Site",
             y = "Count")
    } else {
      plot.new()
      title("Stratification plot not available: 'site' column missing.")
    }
  })
  
  # Download predictions as an Excel file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("predictions_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      pred_data <- predictions()$predictions
      write_xlsx(pred_data, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

