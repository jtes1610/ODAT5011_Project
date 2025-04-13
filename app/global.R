# ----------------------------------------------------------
# Setup: Environment Configuration
# ----------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
rm(list = ls())
gc()

# ----------------------------------------------------------
# Load Required Libraries
# ----------------------------------------------------------

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
library(here)            # Used for portability of locating data in project

# Set seed for reproducibility
set.seed(123)  # This initial seed is sufficient for reproducibility

# ----------------------------------------------------------
# Load the UCI Wine Quality and Wine Reviews Datasets
# ----------------------------------------------------------


vinho_verde_data <- readRDS("app/app_data/vinho_verde_data.rds")
wine_reviews_portugal_clean <- readRDS("app/app_data/wine_reviews_portugal_clean.rds")



# ----------------------------------------------------------
# Correlation between Price(USD) and Review Score (Raw and Log)
# ----------------------------------------------------------

# Create a new column with log-transformed price to normalise distribution
wine_reviews_portugal_clean$log_price <- log(wine_reviews_portugal_clean$price_USD)

# ----------------------------------------------------------
# Raw Price Correlation with Score
# ----------------------------------------------------------

# Calculate Pearson correlation coefficient between raw price and score
corr_coef_raw <- cor(wine_reviews_portugal_clean$price_USD, wine_reviews_portugal_clean$score, use = "complete.obs")


# Perform hypothesis test for correlation between raw price and score
corr_test_raw <- cor.test(wine_reviews_portugal_clean$price_USD, wine_reviews_portugal_clean$score)

# ----------------------------------------------------------
# Log-Transformed Price Correlation with Score
# ----------------------------------------------------------

# Calculate Pearson correlation coefficient between log(price) and score
corr_coef_log <- cor(wine_reviews_portugal_clean$log_price, wine_reviews_portugal_clean$score, use = "complete.obs")


# Perform hypothesis test for correlation between log-transformed price and score
corr_test_log <- cor.test(wine_reviews_portugal_clean$log_price, wine_reviews_portugal_clean$score)

# ----------------------------------------------------------
# Convert Quality Columns for Modeling and Plotting
# ----------------------------------------------------------
# Convert quality to an ordered factor for ordinal modeling
vinho_verde_data$quality <- factor(vinho_verde_data$quality, ordered = TRUE)
# Create a numeric version of quality for regression evaluation
vinho_verde_data$quality_num <- as.numeric(as.character(vinho_verde_data$quality))
# Create a factor version for plotting purposes
vinho_verde_data$quality_factor <- factor(as.character(vinho_verde_data$quality))
# Ensure wine_colour is a factor with consistent level order
vinho_verde_data$wine_colour <- factor(vinho_verde_data$wine_colour, levels = c("Red", "White"))


# ----------------------------------------------------------
# Correlation Matrix of Numeric Features
# ----------------------------------------------------------
# Extract numeric features and rename quality for correlation analysis
numeric_features <- vinho_verde_data %>% 
    select_if(is.numeric) %>% 
    rename(Quality = quality_num)

# ----------------------------------------------------------
# Train-Test Split for Modeling
# ----------------------------------------------------------
# Generate random index for 80% training split
vinho_verde_index <- sample(seq_len(nrow(vinho_verde_data)), size = 0.8 * nrow(vinho_verde_data))
# Create training set (80% of data)
vinho_verde_train_set <- vinho_verde_data[vinho_verde_index, ]
# Create test set (remaining 20% of data)
vinho_verde_test_set <- vinho_verde_data[-vinho_verde_index, ]

# Subset training set for numeric modeling
vinho_verde_train_numeric <- vinho_verde_train_set %>% dplyr::select(quality, fixed_acidity:alcohol)
# Subset test set for numeric modeling
vinho_verde_test_numeric <- vinho_verde_test_set %>% dplyr::select(quality, fixed_acidity:alcohol)

# Store ordered quality levels for consistency
quality_levels <- levels(vinho_verde_data$quality)
# Apply ordered factor to training set
vinho_verde_train_numeric$quality <- factor(vinho_verde_train_numeric$quality, levels = quality_levels, ordered = TRUE)
# Apply ordered factor to test set
vinho_verde_test_numeric$quality <- factor(vinho_verde_test_numeric$quality, levels = quality_levels, ordered = TRUE)

# ----------------------------------------------------------
# Fit Ordinal Forest Model
# ----------------------------------------------------------
# Determine mtry parameter for ordinal forest
mtry_val <- floor(sqrt(ncol(vinho_verde_train_numeric) - 1))
of_model <- ordfor(depvar = "quality", data = as.data.frame(vinho_verde_train_numeric),
                   nsets = 100, ntreeperdiv = 100, ntreefinal = 500, mtry = mtry_val)

# Generate ordinal forest predictions on the test set
preds <- predict(of_model, newdata = as.data.frame(vinho_verde_test_numeric))[[1]]
# Recode predicted class indices to original quality levels
preds_recoded <- factor(quality_levels[as.numeric(preds)], ordered = TRUE, levels = quality_levels)




# ----------------------------------------------------------
# Compute Feature Ranges for UI Inputs
# ----------------------------------------------------------
feature_ranges <- vinho_verde_data %>%
    summarise(
        fixed_acidity_min    = min(fixed_acidity),
        fixed_acidity_max    = max(fixed_acidity),
        volatile_acidity_min = min(volatile_acidity),
        volatile_acidity_max = max(volatile_acidity),
        citric_acid_min      = min(citric_acid),
        citric_acid_max      = max(citric_acid),
        residual_sugar_min   = min(residual_sugar),
        residual_sugar_max   = max(residual_sugar),
        chlorides_min        = min(chlorides),
        chlorides_max        = max(chlorides),
        free_sulfur_dioxide_min   = min(free_sulfur_dioxide),
        free_sulfur_dioxide_max   = max(free_sulfur_dioxide),
        total_sulfur_dioxide_min  = min(total_sulfur_dioxide),
        total_sulfur_dioxide_max  = max(total_sulfur_dioxide),
        density_min          = min(density),
        density_max          = max(density),
        pH_min               = min(pH),
        pH_max               = max(pH),
        sulphates_min        = min(sulphates),
        sulphates_max        = max(sulphates),
        alcohol_min          = min(alcohol),
        alcohol_max          = max(alcohol)
    )
print(feature_ranges)

# -------------------------------
# Define Global Column Order for Outputs
# -------------------------------
desired_order_history <- c("Timestamp", "Alcohol", "Density", "Volatile Acidity", 
                           "Chlorides", "Free Sulfur Dioxide", "Residual Sugar", 
                           "Total Sulfur Dioxide", "Citric Acid", "Sulphates", 
                           "Fixed Acidity", "pH", "Wine Colour", "Predicted Quality")

desired_order_filtered <- c("Alcohol", "Volatile Acidity", "Density",  
                            "Chlorides", "Free Sulfur Dioxide", "Residual Sugar", 
                            "Total Sulfur Dioxide", "Citric Acid", "Sulphates", 
                            "Fixed Acidity", "pH", "Wine Colour", "Quality")


# -------------------------------
# Cache the Ranger Model Once at Startup
# -------------------------------
predictors_used <- c("fixed_acidity", "volatile_acidity", "citric_acid", "residual_sugar",
                     "chlorides", "free_sulfur_dioxide", "total_sulfur_dioxide", "density",
                     "pH", "sulphates", "alcohol", "Wine Colour")
model_cache <- reactiveVal({
    set.seed(123)
    ranger(quality_num ~ fixed_acidity + volatile_acidity + citric_acid +
               residual_sugar + chlorides + free_sulfur_dioxide +
               total_sulfur_dioxide + density + pH + sulphates + alcohol + wine_colour,
           data = vinho_verde_train_set, num.trees = 500, importance = "impurity")
})

