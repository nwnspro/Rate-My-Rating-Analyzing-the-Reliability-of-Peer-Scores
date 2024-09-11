# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(psych)
library(reshape2)
library(shinythemes)
library(shinycssloaders)
library(plotly)

# Read your dataset
data <- read.csv('sample-appraisal-data.csv')

# UI part with a modern dark theme and more interactive options
ui <- fluidPage(
  theme = shinytheme("superhero"),  # Dark theme for a modern look
  titlePanel("Peer Ratings Analysis & Outlier Detection"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("analysis_type", "Choose Analysis Type:", 
                  choices = c("Appraisee" = "appraisee", "Appraiser" = "appraiser")),
      selectInput("group", "Choose Group:", choices = unique(data$group)),
      radioButtons("outlier_rule", "Choose Outlier Detection Rule:", 
                   choices = c("Z-score" = "z_score", "IQR" = "iqr", "Standard Deviation" = "std_dev")),
      sliderInput("outlier_threshold", "Outlier Detection Threshold:", min = 1, max = 5, value = 3, step = 0.5),
      actionButton("analyze", "Analyze 🚀"),
      downloadButton("downloadData", "Download Report")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Statistical Summary", verbatimTextOutput("stats_output")),  # Moved to first position
        tabPanel("Visualization", withSpinner(plotlyOutput("main_plot"), type = 4)),
        tabPanel("Heatmap", withSpinner(plotlyOutput("heatmap_output"), type = 4)),
        tabPanel("Outlier Detection", verbatimTextOutput("outlier_output")),
        tabPanel("Reliability Analysis", verbatimTextOutput("reliability_output"))
      )
    )
  )
)

# Server part
server <- function(input, output) {
  
  observeEvent(input$analyze, {
    
    selected_data <- data %>%
      filter(group == input$group)
    
    # Function to detect outliers using Z-score
    detect_outliers_z <- function(df) {
      df %>%
        mutate(across(starts_with("Category"), ~ abs((. - mean(.)) / sd(.)) > input$outlier_threshold, .names = "outlier_{.col}"))
    }
    
    # Function to detect outliers using IQR
    detect_outliers_iqr <- function(df) {
      df %>%
        mutate(across(starts_with("Category"), ~ (. < (quantile(., 0.25) - 1.5 * IQR(.)) | 
                                                    . > (quantile(., 0.75) + 1.5 * IQR(.))), .names = "outlier_{.col}"))
    }
    
    # Function to detect outliers using Standard Deviation
    detect_outliers_std <- function(df) {
      df %>%
        mutate(across(starts_with("Category"), ~ abs(. - mean(.)) > input$outlier_threshold * sd(.), .names = "outlier_{.col}"))
    }
    
    # **Appraiser Outlier Analysis**
    if (input$analysis_type == "appraiser") {
      appraiser_summary <- selected_data %>%
        group_by(appraiser) %>%
        summarise(across(starts_with("Category"), list(mean = mean, std = sd)))
      
      # Detect outlier appraisers
      if (input$outlier_rule == "z_score") {
        outliers <- detect_outliers_z(selected_data)
      } else if (input$outlier_rule == "iqr") {
        outliers <- detect_outliers_iqr(selected_data)
      } else {
        outliers <- detect_outliers_std(selected_data)
      }
      
      abnormal_appraisers <- unique(outliers %>% filter(if_any(starts_with("outlier_"), ~ . == TRUE)) %>% pull(appraiser))
      
      output$stats_output <- renderPrint({
        cat("**Appraiser Ratings Statistical Summary**\n", sep = "")
        appraiser_summary %>%
          mutate(appraiser = ifelse(appraiser %in% abnormal_appraisers, paste0(appraiser, " *"), appraiser)) %>%
          print()
        cat("\nThe following appraisers have abnormal ratings:\n")
        if (length(abnormal_appraisers) > 0) {
          print(abnormal_appraisers)
        } else {
          cat("No appraisers with abnormal ratings detected.\n")
        }
      })
      
      # Boxplot for appraiser ratings with tooltips
      output$main_plot <- renderPlotly({
        melted_data <- melt(selected_data, id.vars = "appraiser", measure.vars = c("Category1", "Category2", "Category3", "Category4", "Category5"))
        p <- ggplot(melted_data, aes(x = as.factor(appraiser), y = value, color = variable)) +
          geom_boxplot(aes(fill = variable), alpha = 0.7) +
          labs(title = "Appraiser Ratings Distribution", x = "Appraiser", y = "Rating") +
          theme_minimal()
        
        ggplotly(p, tooltip = c("x", "y", "variable"))
      })
      
      # Heatmap for appraiser ratings with tooltips
      output$heatmap_output <- renderPlotly({
        melted_data <- melt(selected_data, id.vars = "appraiser", measure.vars = c("Category1", "Category2", "Category3", "Category4", "Category5"))
        p <- ggplot(melted_data, aes(x = variable, y = as.factor(appraiser), fill = value)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          labs(title = "Appraiser Ratings Heatmap", x = "Category", y = "Appraiser") +
          theme_minimal()
        
        ggplotly(p, tooltip = c("x", "y", "fill"))
      })
      
      # **Appraisee Outlier Analysis**
    } else if (input$analysis_type == "appraisee") {
      appraisee_summary <- selected_data %>%
        group_by(appraisee) %>%
        summarise(across(starts_with("Category"), list(mean = mean, std = sd)))
      
      # Detect outlier appraisees
      if (input$outlier_rule == "z_score") {
        outliers <- detect_outliers_z(selected_data)
      } else if (input$outlier_rule == "iqr") {
        outliers <- detect_outliers_iqr(selected_data)
      } else {
        outliers <- detect_outliers_std(selected_data)
      }
      
      abnormal_appraisees <- unique(outliers %>% filter(if_any(starts_with("outlier_"), ~ . == TRUE)) %>% pull(appraisee))
      
      output$stats_output <- renderPrint({
        cat("**Appraisee Ratings Statistical Summary**\n", sep = "")
        appraisee_summary %>%
          mutate(appraisee = ifelse(appraisee %in% abnormal_appraisees, paste0(appraisee, " *"), appraisee)) %>%
          print()
        cat("\nThe following appraisees have abnormal ratings:\n")
        if (length(abnormal_appraisees) > 0) {
          print(abnormal_appraisees)
        } else {
          cat("No appraisees with abnormal ratings detected.\n")
        }
      })
      
      # Boxplot for appraisee ratings with tooltips
      output$main_plot <- renderPlotly({
        melted_data <- melt(selected_data, id.vars = "appraisee", measure.vars = c("Category1", "Category2", "Category3", "Category4", "Category5"))
        p <- ggplot(melted_data, aes(x = as.factor(appraisee), y = value, color = variable)) +
          geom_boxplot(aes(fill = variable), alpha = 0.7) +
          labs(title = "Appraisee Ratings Distribution", x = "Appraisee", y = "Rating") +
          theme_minimal()
        
        ggplotly(p, tooltip = c("x", "y", "variable"))
      })
      
      # Heatmap for appraisee ratings with tooltips
      output$heatmap_output <- renderPlotly({
        melted_data <- melt(selected_data, id.vars = "appraisee", measure.vars = c("Category1", "Category2", "Category3", "Category4", "Category5"))
        p <- ggplot(melted_data, aes(x = variable, y = as.factor(appraisee), fill = value)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "blue") +
          labs(title = "Appraisee Ratings Heatmap", x = "Category", y = "Appraisee") +
          theme_minimal()
        
        ggplotly(p, tooltip = c("x", "y", "fill"))
      })
    }
    
    # Outlier Detection Summary
    output$outlier_output <- renderPrint({
      cat("**Outlier Detection Summary**\n", sep = "")
      cat("Detected outliers based on selected rule and threshold:\n")
      if (input$outlier_rule == "z_score") {
        outliers <- detect_outliers_z(selected_data)
      } else if (input$outlier_rule == "iqr") {
        outliers <- detect_outliers_iqr(selected_data)
      } else {
        outliers <- detect_outliers_std(selected_data)
      }
      print(outliers)
    })
    
    # **Reliability Analysis**
    output$reliability_output <- renderPrint({
      cat("**Reliability Analysis**\n")
      cronbach_alpha <- psych::alpha(selected_data %>% select(starts_with("Category")))
      print(cronbach_alpha$total)
    })
    
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)

