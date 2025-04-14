# --------------------------------------------
# train_models.R
# Pre-train wine quality models for Shiny app
# --------------------------------------------

# Load required libraries
library(tidyverse)
library(ordinalForest)
library(ranger)
library(readr)

# Set seed for reproducibility
set.seed(123)

# Load cleaned Vinho Verde data
vinho_verde_data <- read_csv("app/app_data/vinho_verde_data_clean.csv", show_col_types = FALSE)

# Preprocess data
vinho_verde_data$quality <- factor(vinho_verde_data$quality, ordered = TRUE)
vinho_verde_data$quality_num <- as.numeric(as.character(vinho_verde_data$quality))
vinho_verde_data$quality_factor <- factor(as.character(vinho_verde_data$quality))
vinho_verde_data$wine_colour <- factor(vinho_verde_data$wine_colour, levels = c("Red", "White"))

# Split into training and testing sets (80/20)
vinho_verde_index <- sample(seq_len(nrow(vinho_verde_data)), size = 0.8 * nrow(vinho_verde_data))
vinho_verde_train_set <- vinho_verde_data[vinho_verde_index, ]
vinho_verde_test_set  <- vinho_verde_data[-vinho_verde_index, ]

# Prepare numeric training sets
vinho_verde_train_numeric <- vinho_verde_train_set %>% select(quality, fixed_acidity:alcohol)
vinho_verde_test_numeric  <- vinho_verde_test_set  %>% select(quality, fixed_acidity:alcohol)

# Match quality levels
quality_levels <- levels(vinho_verde_data$quality)
vinho_verde_train_numeric$quality <- factor(vinho_verde_train_numeric$quality, levels = quality_levels, ordered = TRUE)
vinho_verde_test_numeric$quality  <- factor(vinho_verde_test_numeric$quality, levels = quality_levels, ordered = TRUE)

# -------------------------------------------------------
# Train Ordinal Forest Model
# -------------------------------------------------------
mtry_val <- floor(sqrt(ncol(vinho_verde_train_numeric) - 1))

of_model <- ordfor(
  depvar = "quality",
  data = as.data.frame(vinho_verde_train_numeric),
  nsets = 100,
  ntreeperdiv = 100,
  ntreefinal = 500,
  mtry = mtry_val
)

# -------------------------------------------------------
# Train Ranger Random Forest Model (Regression)
# -------------------------------------------------------
rf_model <- ranger(
  quality_num ~ fixed_acidity + volatile_acidity + citric_acid +
    residual_sugar + chlorides + free_sulfur_dioxide +
    total_sulfur_dioxide + density + pH + sulphates + alcohol + wine_colour,
  data = vinho_verde_train_set,
  num.trees = 500,
  importance = "impurity"
)

# -------------------------------------------------------
# Save the trained models to app/models/
# -------------------------------------------------------
# Create models directory if it doesn't exist
if (!dir.exists("app/models")) dir.create("app/models", recursive = TRUE)

saveRDS(of_model, file = "app/models/of_model.rds")
saveRDS(rf_model, file = "app/models/rf_model.rds")

cat("✅ Models trained and saved to 'app/models/'\n")

