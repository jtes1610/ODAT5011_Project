# ----------------------------------------------------------
# Setup: Environment Configuration
# ----------------------------------------------------------

rm(list = ls())
gc()

# ----------------------------------------------------------
# Load Required Libraries
# ----------------------------------------------------------

library(tidyverse)       # Core data manipulation and visualisation tools 
library(corrplot)        # Visualising correlation matrices
library(randomForest)    # Traditional Random Forest modelling
library(broom)           # Tidying model outputs into tibbles
library(shiny)           # Building interactive web applications
library(bslib)           # Theming and layout tools for Shiny apps
library(tm)              # Text mining (tokenising + cleaning)
library(wordcloud)       # Word cloud visualisation
library(RColorBrewer)    # Colour palettes
library(gridExtra)       # For arranging ggplot objects in grid
library(ordinalForest)   # Ordinal random forest modelling
library(MASS)            # Classic stats (e.g. LDA)
library(irr)             # Inter-rater reliability stats
library(caret)           # Training framework
library(ggplot2)         # Grammar of graphics
library(stringr)         # String manipulation
library(ranger)          # Fast random forest implementation
library(readr)           # Fast CSV reading
library(Polychrome)      # Distinct colour palettes

# ----------------------------------------------------------
# Set Seed for Reproducibility
# ----------------------------------------------------------
set.seed(123)

# ----------------------------------------------------------
# Load Cleaned Datasets
# ----------------------------------------------------------
vinho_verde_data <- read_csv("app_data/vinho_verde_data_clean.csv", show_col_types = FALSE)
wine_reviews_portugal_clean <- read_csv("app_data/wine_reviews_portugal_clean.csv", show_col_types = FALSE)

# ----------------------------------------------------------
# Load Pretrained Models
# ----------------------------------------------------------
of_model <- readRDS("models/of_model.rds")
rf_model <- readRDS("models/rf_model.rds")

# ----------------------------------------------------------
# Prepare Wine Reviews Data (Log Price)
# ----------------------------------------------------------
wine_reviews_portugal_clean$log_price <- log(wine_reviews_portugal_clean$price_USD)

corr_coef_raw <- cor(wine_reviews_portugal_clean$price_USD, wine_reviews_portugal_clean$score, use = "complete.obs")
corr_test_raw <- cor.test(wine_reviews_portugal_clean$price_USD, wine_reviews_portugal_clean$score)

corr_coef_log <- cor(wine_reviews_portugal_clean$log_price, wine_reviews_portugal_clean$score, use = "complete.obs")
corr_test_log <- cor.test(wine_reviews_portugal_clean$log_price, wine_reviews_portugal_clean$score)

# ----------------------------------------------------------
# Preprocess Vinho Verde Data
# ----------------------------------------------------------
vinho_verde_data$quality <- factor(vinho_verde_data$quality, ordered = TRUE)
vinho_verde_data$quality_num <- as.numeric(as.character(vinho_verde_data$quality))
vinho_verde_data$quality_factor <- factor(as.character(vinho_verde_data$quality))
vinho_verde_data$wine_colour <- factor(vinho_verde_data$wine_colour, levels = c("Red", "White"))

# ----------------------------------------------------------
# Split into Train/Test (for Predictions & Evaluation)
# ----------------------------------------------------------
vinho_verde_index <- sample(seq_len(nrow(vinho_verde_data)), size = 0.8 * nrow(vinho_verde_data))
vinho_verde_train_set <- vinho_verde_data[vinho_verde_index, ]
vinho_verde_test_set  <- vinho_verde_data[-vinho_verde_index, ]

# Prepare numeric train/test sets
vinho_verde_train_numeric <- vinho_verde_train_set %>% select(quality, fixed_acidity:alcohol)
vinho_verde_test_numeric  <- vinho_verde_test_set  %>% select(quality, fixed_acidity:alcohol)

quality_levels <- levels(vinho_verde_data$quality)
vinho_verde_train_numeric$quality <- factor(vinho_verde_train_numeric$quality, levels = quality_levels, ordered = TRUE)
vinho_verde_test_numeric$quality  <- factor(vinho_verde_test_numeric$quality, levels = quality_levels, ordered = TRUE)

# ----------------------------------------------------------
# Ordinal Forest Predictions (Test Set)
# ----------------------------------------------------------
preds <- predict(of_model, newdata = as.data.frame(vinho_verde_test_numeric))[[1]]
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

# ----------------------------------------------------------
# Column Order Settings
# ----------------------------------------------------------
desired_order_history <- c("Timestamp", "Alcohol", "Density", "Volatile Acidity", 
                           "Chlorides", "Free Sulfur Dioxide", "Residual Sugar", 
                           "Total Sulfur Dioxide", "Citric Acid", "Sulphates", 
                           "Fixed Acidity", "pH", "Wine Colour", "Predicted Quality")

desired_order_filtered <- c("Alcohol", "Volatile Acidity", "Density",  
                            "Chlorides", "Free Sulfur Dioxide", "Residual Sugar", 
                            "Total Sulfur Dioxide", "Citric Acid", "Sulphates", 
                            "Fixed Acidity", "pH", "Wine Colour", "Quality")