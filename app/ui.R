

# Libraries for data manipulation, modeling, and visualization
library(tidyverse)       # Core data manipulation and visualisation tools 
library(corrplot)        # Visualizing correlation matrices
library(randomForest)    # Traditional Random Forest modeling
library(broom)           # Tidying model outputs into tibbles
library(shiny)           # Building interactive web applications
library(bslib)           # Theming and layout tools for Shiny apps
library(tm)              # Text mining framework (cleaning and tokenising)
library(wordcloud)       # Generating word cloud visualizations
library(RColorBrewer)    # Color palettes for better plot aesthetics
library(gridExtra)       # Arranging multiple ggplot objects in a grid
library(ordinalForest)   # Ordinal random forest modeling for ordered outcomes
library(MASS)            # Classic stats functions, including LDA and distributions
library(irr)             # Inter-rater reliability metrics (e.g., Cohen’s Kappa)
library(caret)           # Classification and Regression Training framework
library(ggplot2)         # Grammar of graphics visualisation package 
library(stringr)         # String manipulation utilities
library(ranger)          # Fast implementation of Random Forests
library(readr)           # Fast and tidy reading of CSV and text files
library(Polychrome)      # Load Polychrome for generating visually distinct colour palettes

fluidPage(
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Poppins")),
  
  tags$head(
    tags$title("🍷 Wine Quality Prediction Dashboard"),
    tags$link(rel = "icon", type = "image/png", href = "www/wine_favicon.png"),
    tags$style(HTML("
      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
      }
      #page-container {
        position: relative;
        min-height: 100vh;
      }
      #content-wrap {
        padding-bottom: 60px; /* Space for footer */
      }
      #footer {
        position: fixed;
        bottom: 0;
        width: 100%;
        height: 60px;
        text-align: center;
        padding-top: 15px;
        color: #888;
        background-color: transparent;
      }
      .card {
        background-color: white;
        border-radius: 15px;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.1);
        padding: 20px;
        margin-bottom: 20px;
      }
      .predict-button {
        font-size: 1.2em;
        padding: 10px 30px;
      }
      .result-box {
        background: linear-gradient(to right, #f2f2f2, #e6ffe6);
        padding: 25px;
        font-size: 1.6em;
        border-radius: 20px;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.15);
        text-align: center;
      }
    "))
  ),
  
  div(id = "page-container",
      div(id = "content-wrap",
          
          titlePanel(h2("🍷 Wine Quality Predictor", class = "mt-4 mb-4 text-center")),
          
          tabsetPanel(
            # ----------------------------------------------------------
            # Tab 1: Welcome Summary
            # ----------------------------------------------------------
            tabPanel("Welcome",
                     fluidRow(
                       column(12, class = "mt-4 mb-4",
                              h3("Welcome to the Wine Quality Dashboard"),
                              p("This application allows you to explore and predict wine quality using advanced models based on physicochemical attributes."),
                              p("Navigate through the tabs to:"),
                              tags$ul(
                                tags$li("🧠 Explore reviewer feedback and pricing trends"),
                                tags$li("🔍 Examine distribution and relationships between wine attributes"),
                                tags$li("📈 Predict wine quality using the predictive model"),
                                tags$li("📊 Review prediction history and filtered samples"),
                                tags$li("📚 Access data dictionaries for both datasets")
                              )
                       )
                     )
            ),
            
            # ----------------------------------------------------------
            # Tab 2: Critics Review
            # ----------------------------------------------------------
            tabPanel("Critics Review",
                     fluidRow(
                       column(6, class = "mt-4 mb-4", plotOutput("wordcloud_plot", height = "1600px")),
                       column(6, class = "mt-4 mb-4", plotOutput("log_regression_plot", height = "700px"))
                     )
            ),
            
            # ----------------------------------------------------------
            # Tab 3: Distribution Of Wines
            # ----------------------------------------------------------
            tabPanel("Distribution Of Wines",
                     fluidRow(
                       column(6, class = "mt-4 mb-4",
                              plotOutput("dist_quality_plot", height = "700px")
                       ),
                       column(6, class = "mt-4 mb-4",
                              plotOutput("dist_score_plot", height = "700px")
                       )
                     )
            ),
            
            # ----------------------------------------------------------
            # Tab 4: Predictors (Correlation + Importance)
            # ----------------------------------------------------------
            tabPanel("Predictors",
                     fluidRow(
                       column(6, class = "mt-4 mb-4", plotOutput("corr_plot", height = "800px", width = "100%")),
                       column(6, class = "mt-4 mb-4", plotOutput("varimp_plot", height = "800px", width = "100%"))
                     )
            ),
            
            # ----------------------------------------------------------
            # Tab 5: Prediction Tool
            # ----------------------------------------------------------
            tabPanel("Prediction Tool",
                     
                     # --- Predictor Sliders (Top, Mid, Lower) ---
                     fluidRow(class = "mt-4 mb-4",
                              column(4,
                                     div(class = "card",
                                         h4("Top Predictors"),
                                         fluidRow(
                                           column(8,
                                                  sliderInput("alcohol_slider", "Alcohol", 
                                                              min = feature_ranges$alcohol_min, max = feature_ranges$alcohol_max, 
                                                              value = mean(c(feature_ranges$alcohol_min, feature_ranges$alcohol_max)), step = 0.1)
                                           ),
                                           column(4,
                                                  numericInput("alcohol_num", NULL, 
                                                               value = mean(c(feature_ranges$alcohol_min, feature_ranges$alcohol_max)), 
                                                               min = feature_ranges$alcohol_min, max = feature_ranges$alcohol_max, step = 0.1)
                                           )
                                         ),
                                         fluidRow(
                                           column(8,
                                                  sliderInput("volatile_acidity_slider", "Volatile Acidity", 
                                                              min = feature_ranges$volatile_acidity_min, max = feature_ranges$volatile_acidity_max, 
                                                              value = 0.5, step = 0.01)
                                           ),
                                           column(4,
                                                  numericInput("volatile_acidity_num", NULL, 
                                                               value = 0.5, 
                                                               min = feature_ranges$volatile_acidity_min, max = feature_ranges$volatile_acidity_max, step = 0.01)
                                           )
                                         ),
                                         fluidRow(
                                           column(8,
                                                  sliderInput("density_slider", "Density", 
                                                              min = feature_ranges$density_min, max = feature_ranges$density_max, 
                                                              value = mean(c(feature_ranges$density_min, feature_ranges$density_max)), step = 0.0001)
                                           ),
                                           column(4,
                                                  numericInput("density_num", NULL, 
                                                               value = mean(c(feature_ranges$density_min, feature_ranges$density_max)), 
                                                               min = feature_ranges$density_min, max = feature_ranges$density_max, step = 0.0001)
                                           )
                                         )
                                     )
                              ),
                              column(4,
                                     div(class = "card",
                                         h4("Mid-range Predictors"),
                                         fluidRow(
                                           column(8,
                                                  sliderInput("chlorides_slider", "Chlorides", 
                                                              min = feature_ranges$chlorides_min, max = feature_ranges$chlorides_max, 
                                                              value = 0.05, step = 0.001)
                                           ),
                                           column(4,
                                                  numericInput("chlorides_num", NULL, 
                                                               value = 0.05, 
                                                               min = feature_ranges$chlorides_min, max = feature_ranges$chlorides_max, step = 0.001)
                                           )
                                         ),
                                         fluidRow(
                                           column(8,
                                                  sliderInput("free_sulfur_slider", "Free Sulfur Dioxide", 
                                                              min = feature_ranges$free_sulfur_dioxide_min, max = feature_ranges$free_sulfur_dioxide_max, 
                                                              value = 30, step = 1)
                                           ),
                                           column(4,
                                                  numericInput("free_sulfur_num", NULL, 
                                                               value = 30, 
                                                               min = feature_ranges$free_sulfur_dioxide_min, max = feature_ranges$free_sulfur_dioxide_max, step = 1)
                                           )
                                         ),
                                         fluidRow(
                                           column(8,
                                                  sliderInput("residual_sugar_slider", "Residual Sugar", 
                                                              min = feature_ranges$residual_sugar_min, max = feature_ranges$residual_sugar_max, 
                                                              value = 5.0, step = 0.1)
                                           ),
                                           column(4,
                                                  numericInput("residual_sugar_num", NULL, 
                                                               value = 5.0, 
                                                               min = feature_ranges$residual_sugar_min, max = feature_ranges$residual_sugar_max, step = 0.1)
                                           )
                                         ),
                                         fluidRow(
                                           column(8,
                                                  sliderInput("total_sulfur_slider", "Total Sulfur Dioxide", 
                                                              min = feature_ranges$total_sulfur_dioxide_min, max = feature_ranges$total_sulfur_dioxide_max, 
                                                              value = 115, step = 1)
                                           ),
                                           column(4,
                                                  numericInput("total_sulfur_num", NULL, 
                                                               value = 115, 
                                                               min = feature_ranges$total_sulfur_dioxide_min, max = feature_ranges$total_sulfur_dioxide_max, step = 1)
                                           )
                                         )
                                     )
                              ),
                              column(4,
                                     div(class = "card",
                                         h4("Lower-range Predictors"),
                                         fluidRow(
                                           column(8,
                                                  sliderInput("citric_acid_slider", "Citric Acid", 
                                                              min = feature_ranges$citric_acid_min, max = feature_ranges$citric_acid_max, 
                                                              value = 0.3, step = 0.01)
                                           ),
                                           column(4,
                                                  numericInput("citric_acid_num", NULL, 
                                                               value = 0.3, 
                                                               min = feature_ranges$citric_acid_min, max = feature_ranges$citric_acid_max, step = 0.01)
                                           )
                                         ),
                                         fluidRow(
                                           column(8,
                                                  sliderInput("sulphates_slider", "Sulphates", 
                                                              min = feature_ranges$sulphates_min, max = feature_ranges$sulphates_max, 
                                                              value = 0.5, step = 0.01)
                                           ),
                                           column(4,
                                                  numericInput("sulphates_num", NULL, 
                                                               value = 0.5, 
                                                               min = feature_ranges$sulphates_min, max = feature_ranges$sulphates_max, step = 0.01)
                                           )
                                         ),
                                         fluidRow(
                                           column(8,
                                                  sliderInput("fixed_acidity_slider", "Fixed Acidity", 
                                                              min = feature_ranges$fixed_acidity_min, max = feature_ranges$fixed_acidity_max, 
                                                              value = mean(c(feature_ranges$fixed_acidity_min, feature_ranges$fixed_acidity_max)), step = 0.1)
                                           ),
                                           column(4,
                                                  numericInput("fixed_acidity_num", NULL, 
                                                               value = mean(c(feature_ranges$fixed_acidity_min, feature_ranges$fixed_acidity_max)), 
                                                               min = feature_ranges$fixed_acidity_min, max = feature_ranges$fixed_acidity_max, step = 0.1)
                                           )
                                         ),
                                         fluidRow(
                                           column(8,
                                                  sliderInput("pH_slider", "pH", 
                                                              min = feature_ranges$pH_min, max = feature_ranges$pH_max, 
                                                              value = 3.2, step = 0.01)
                                           ),
                                           column(4,
                                                  numericInput("pH_num", NULL, 
                                                               value = 3.2, 
                                                               min = feature_ranges$pH_min, max = feature_ranges$pH_max, step = 0.01)
                                           )
                                         ),
                                         fluidRow(
                                           column(8,
                                                  selectInput("wine_colour", "Wine Colour", choices = c("Red", "White"))
                                           ),
                                           column(4,
                                                  verbatimTextOutput("type_text")
                                           )
                                         )
                                     )
                              )
                     ),
                     
                     # --- Prediction Buttons ---
                     fluidRow(class = "mt-3 mb-4",
                              column(6, align = "center",
                                     actionButton("predict", "🔍 Predict Wine Quality", class = "btn btn-success predict-button mt-2")
                              ),
                              column(6, align = "center",
                                     actionButton("reset", "Reset Prediction", class = "btn btn-warning predict-button mt-2")
                              )
                     ),
                     
                     # --- Prediction Output ---
                     fluidRow(
                       column(12, uiOutput("result_box")),
                       #column(12, uiOutput("quality_summary_text")),
                       column(12, uiOutput("percentile_text")),
                       column(12, plotOutput("quality_plot", height = "250px", width = "100%"))  
                     )
            ),
            
            
            # ----------------------------------------------------------
            # Tab 6: Filtered Results
            # ----------------------------------------------------------
            tabPanel("Filtered Results",
                     fluidRow(
                       column(12, class = "mt-4",
                              selectInput("quality_filter", "Filter Wines by Quality:",
                                          choices = c("All", "Top Quality", "Average Quality", "Poor Quality"),
                                          selected = "All")
                       )
                     ),
                     fluidRow(
                       column(12, class = "mt-4", tableOutput("filteredTable"))
                     )
            ),
            
            # ----------------------------------------------------------
            # Tab 7: Prediction History
            # ----------------------------------------------------------
            tabPanel("Prediction History",
                     fluidRow(
                       column(12, align = "center", class = "mt-4",
                              actionButton("reset_history", "Reset History", class = "btn btn-warning")
                       ),
                       br(),
                       fluidRow(
                         column(12, downloadButton("download_history", "Download History"))
                       ),
                       br(),
                       column(12, tableOutput("prediction_history"))
                     )
            ),
            
            # ----------------------------------------------------------
            # Tab 8: Data Dictionaries
            # ----------------------------------------------------------
            tabPanel("Data Dictionaries",
                     fluidRow(
                       column(6, class = "mt-4 mb-4",
                              h4("Vinho Verde Wine Quality"),
                              tableOutput("dict_vinho_verde")
                       ),
                       column(6, class = "mt-4 mb-4",
                              h4("Wine Reviews"),
                              tableOutput("dict_reviews")
                       )
                     )
            )
          ),  # end tabsetPanel
          
          div(id = "footer", "Created by 310247934 | University of Sydney")
      )  # end content-wrap
  )  # end page-container
)