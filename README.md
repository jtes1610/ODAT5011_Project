# 🍷 Wine Quality Analysis & Prediction Dashboard

### University of Sydney — ODAT5011 Project 2

**Author:** James Tesoriero\
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
├── app/                    # Shiny application files
│   ├── app.R              # Launch script (shinyApp(ui, server))
│   ├── global.R           # Global setup: data, models, variables
│   ├── server.R           # Server logic
│   └── ui.R               # User interface layout
├── data/                  # Raw datasets
│   ├── winequality-red.csv
│   ├── winequality-white.csv
│   └── winemag.csv
├── scripts/               # Analytical report and supporting R code
│   └── ODAT5011_Wine_Report_Analysis.qmd
├── README.md              # Project documentation
└── ODAT5011_Project.Rproj # RStudio project file
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
