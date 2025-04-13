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

# Set seed for reproducibility
set.seed(123)  # This initial seed is sufficient for reproducibility

# ----------------------------------------------------------
# Load and Combine UCI Wine Quality Datasets
# ----------------------------------------------------------


wine_red <- read_delim("../data/winequality-red.csv", delim = ";")
wine_white <- read_delim("../data/winequality-white.csv", delim = ";")

wine_red$wine_colour <- "Red"
wine_white$wine_colour <- "White"

# Combine red and white datasets into a unified dataframe and clean column names
vinho_verde_data <- bind_rows(wine_red, wine_white) %>%
    rename_with(~ gsub(" ", "_", .))

# ----------------------------------------------------------
# Load Wine Reviews Dataset
# ----------------------------------------------------------
wine_reviews <- read_delim("../data/winemag.csv")

# ----------------------------------------------------------
# Provide logic for wine variety colours and Filter for Portugal
# ----------------------------------------------------------

# Define red and white varieties
red_varieties <- c(
    "Cabernet Sauvignon", "Merlot", "Pinot Noir", "Syrah", "Malbec", "Tempranillo", 
    "Sangiovese", "Zinfandel", "Grenache", "Mourvèdre", "Touriga Nacional", "Baga", 
    "Aragonez", "Tinta Roriz", "Touriga Franca", "Bobal", "Alfrocheiro", "Vinhão", 
    "Argaman", "Graciano", "Garnacha Tintorera", "Tinta Amarela", "Alicante Bouschet", 
    "Aragonês", "Baga-Touriga Nacional", "Bastardo", "Bordeaux-style Red Blend", 
    "Cabernet Sauvignon and Tinta Roriz", "Cabernet Sauvignon-Syrah", "Castelão", 
    "Espadeiro", "Jaen", "Madeira Blend", "Merlot-Syrah", "Moscatel Roxo", "Petit Verdot", 
    "Petite Verdot", "Port", "Portuguese Red", "Portuguese Rosé", "Red Blend", 
    "Rhône-style Red Blend", "Rosé", "Shiraz", "Sousão", "Tinta Barroca", "Tinta Francisca", 
    "Tinta Negra Mole", "Touriga Nacional Blend", "Touriga Nacional-Cabernet Sauvignon", 
    "Trincadeira"
)
white_varieties <- c(
    "Chardonnay", "Sauvignon Blanc", "Riesling", "Pinot Grigio", "Viognier", "Gewürztraminer", 
    "Alvarinho", "Encruzado", "Arinto", "Antão Vaz", "Loureiro", "Fernão Pires", "Albana", 
    "Alvarinho-Chardonnay", "Avesso", "Azal", "Bical", "Bual", "Côdega do Larinho", "Cerceal", 
    "Chenin Blanc", "Gewürztraminer-Riesling", "Gouveio", "Malmsey", "Malvasia", 
    "Malvasia Fina", "Moscatel", "Moscatel Graúdo", "Muscat", "Pinot Blanc", 
    "Portuguese Sparkling", "Portuguese White", "Rabigato", "Sémillon", "Sercial", "Siria", 
    "Sparkling Blend", "Verdelho", "White Blend", "White Port", "Códega do Larinho"
)



# Filter for Portuguese wines and classify colour based on variety
wine_reviews_portugal_clean <- wine_reviews %>% 
    filter(country == "Portugal") %>%
    mutate(wine_colour = case_when(
        variety %in% red_varieties ~ "Red",
        variety %in% white_varieties ~ "White",
        TRUE ~ "Unknown"
    )) %>%
    rename(score = points, price_USD = price) %>%
    dplyr::select(country, variety, wine_colour, score, price_USD, description) %>%
    filter(!is.na(score), !is.na(price_USD))

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


