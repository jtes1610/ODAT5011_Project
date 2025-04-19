# 🍷 Wine Quality Analysis & Prediction Dashboard

### University of Sydney — ODAT5011 Project 2

**Author:** 310247934\
**Date:** April 2025

------------------------------------------------------------------------

## 📋 Project Overview

This project is part of the ODAT5011 Master's program in Data Analytics at the University of Sydney. It presents a comprehensive analysis of wine quality and its predictors using multiple datasets, statistical modelling, and a user-friendly Shiny application.

The analysis is based on two primary datasets:

-   **UCI Wine Quality Dataset**\
    Contains physicochemical measurements and expert quality ratings for red and white *Vinho Verde* wines.\
    📎 [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Wine+Quality)

-   **Wine Reviews Dataset (Kaggle via Winemag)**\
    Consists of critic reviews, wine descriptions, prices (USD), and ratings for Portuguese wines from multiple varieties.\
    📎 [Kaggle Wine Reviews Dataset](https://www.kaggle.com/datasets/zynicide/wine-reviews)

Together, these datasets are used to explore the relationship between chemical attributes, critic reviews, and wine quality, applying techniques such as:

-   📊 Data visualisation\
-   📈 Correlation analysis\
-   🌲 Ordinal Random Forest modelling\
-   ☁️ Word clouds and text mining\
-   🧠 Interactive predictions via a custom-built Shiny app

------------------------------------------------------------------------

## 📁 Folder Structure

``` text
ODAT5011_Project/
├── app/                        # 🔹 Main Shiny application folder (deploy this folder)
│   ├── app.R                  # Entry point: calls shinyApp(ui, server)
│   ├── global.R               # Global setup: load data, models, constants
│   ├── server.R               # Server logic: reactive expressions, outputs
│   ├── ui.R                   # User interface layout: input controls, layout
│   ├── app_data/              # ✅ Pre-cleaned and transformed datasets
│   │   ├── vinho_verde_train_set.rds          # Training set for modelling
│   │   ├── vinho_verde_test_set.rds           # Test set for evaluation
│   │   ├── wine_all.rds                        # Combined red and white wine data
│   │   └── wine_reviews_portugal_clean.rds    # Cleaned Portuguese reviews
│   └── models/                # ✅ Pre-trained models (loaded at runtime)
│       ├── rf_model.rds                         # Ranger regression model
│       └── of_model.rds                         # Ordinal Forest model
│
├── data/                      # 📦 Raw unmodified datasets for provenance
│   ├── winequality-red.csv                    # UCI red wine quality dataset
│   ├── winequality-white.csv                  # UCI white wine quality dataset
│   └── winemag.csv                            # Kaggle wine reviews
│
├── scripts/                   # 📑 Reports and offline processing scripts
│   ├── ODAT5011_Wine_Report_Analysis.qmd      # Quarto report for academic submission
│   ├── prepare_data_for_app.qmd               # Script for generating .rds datasets
│   └── train_models.R                         # 🔁 Offline model training script
│
├── README.md                  # 📘 Project overview and deployment instructions
└── ODAT5011_Project.Rproj     # 🔧 RStudio project file (opens full environment)
```

------------------------------------------------------------------------

## 📊 Data Sources

-   **winequality-red.csv** & **winequality-white.csv**\
    📎 UCI Machine Learning Repository\
    🔗 <https://archive.ics.uci.edu/ml/datasets/Wine+Quality>

-   **winemag.csv**\
    📎 Kaggle Wine Reviews Dataset\
    🔗 <https://www.kaggle.com/datasets/zynicide/wine-reviews>\
    *(Filtered for Portuguese wines)*

------------------------------------------------------------------------

## 📦 R Packages Used

This project uses the following R packages:

-   `tidyverse` – Data wrangling and plotting\
-   `randomForest`, `ranger`, `ordinalForest` – Machine learning models\
-   `ggplot2`, `corrplot`, `gridExtra` – Visualisation tools\
-   `shiny`, `bslib` – Interactive dashboard and UI theming\
-   `tm`, `wordcloud`, `RColorBrewer` – Text mining and visualisation for critic reviews\
-   `caret`, `MASS`, `irr` – Statistical modelling and evaluation tools
