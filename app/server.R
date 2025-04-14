
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
library(grid)            # Make sure to load the grid package

function(input, output, session) {
    
    # -------------------------------
    # Tab 1: Welcome
    # -------------------------------
    output$welcome_text <- renderUI({
        div(
            class = "card",
            h3("Welcome to the Wine Quality Prediction App"),
            p("This interactive Shiny application provides a comprehensive view into the quality of Portuguese wines, using machine learning models to predict quality, visualisations to explore key patterns, and summaries of critics' reviews."),
            p("Navigate through the tabs to: examine data distribution, understand predictive features, generate custom predictions, filter key samples, review prediction history, and consult the data dictionaries.")
        )
    })
    
    # -------------------------------
    # Tab 2: Critics Review (Word Cloud and Log-Regression)
    # -------------------------------
    
    # Word Cloud for Top Descriptions
    output$wordcloud_plot <- renderPlot({
        
        # --- Preprocess text ---
        top_descriptions <- wine_reviews_portugal_clean %>%
            filter(score > 90) %>%
            pull(description)
        
        corpus <- Corpus(VectorSource(top_descriptions))
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "wine"))
        
        # --- Compute word frequencies ---
        tdm <- TermDocumentMatrix(corpus)
        m <- as.matrix(tdm)
        word_freq <- sort(rowSums(m), decreasing = TRUE)
        word_df <- data.frame(word = names(word_freq), freq = word_freq)
        
        # --- Set up clean plot margin ---
        par(mar = c(1, 1, 4, 1))  # Increase top margin for title space
        
        # --- Generate the word cloud ---
        wordcloud(
            words = word_df$word, freq = word_df$freq, min.freq = 3,
            max.words = 200, random.order = FALSE, rot.per = 0.35,
            scale = c(6, 1.5), colors = brewer.pal(8, "Dark2")
        )
        
        # --- Add the heading ABOVE the word cloud ---
        title(main = "Word Cloud of Highly Rated Portuguese Wines (Score > 90)", 
              cex.main = 1.6, font.main = 2, line = 2)
        
    }, height = 1000)
    
    # Log Regression Plot of Score vs. Log(Price USD)
    output$log_regression_plot <- renderPlot({
        # Filter out rows with NA or zero price to avoid log(0) errors
        filtered_data <- wine_reviews_portugal_clean %>%
            filter(!is.na(price_USD), price_USD > 0)
        
        ggplot(wine_reviews_portugal_clean, aes(x = score, y = log(price_USD))) +
            
            # Light red points for red wine
            geom_jitter(
                data = subset(wine_reviews_portugal_clean, wine_colour == "Red"),
                colour = "#E57373", alpha = 0.5, width = 0.3, height = 0, size = 1.8
            ) +
            
            # Light blue points for white wine
            geom_jitter(
                data = subset(wine_reviews_portugal_clean, wine_colour == "White"),
                colour = "#64B5F6", alpha = 0.5, width = 0.3, height = 0, size = 1.8
            ) +
            
            # Regression line for red wine
            geom_smooth(
                data = subset(wine_reviews_portugal_clean, wine_colour == "Red"),
                aes(colour = "Red"),
                method = "lm", se = FALSE, size = 1
            ) +
            
            # Regression line for white wine
            geom_smooth(
                data = subset(wine_reviews_portugal_clean, wine_colour == "White"),
                aes(colour = "White"),
                method = "lm", se = FALSE, size = 1
            ) +
            
            # Define line colours for legend
            scale_colour_manual(
                name = "Wine Colour",
                values = c("Red" = "firebrick", "White" = "dodgerblue4")
            ) +
            
            labs(
                title = "Log(Price USD) vs. Wine Review Score by Colour",
                x = "Review Score",
                y = "Log(Price USD)",
                caption =  "Figure: Regression analysis showing the relationship between critic review scores and the logarithm of wine prices."
            ) +
            
            theme_minimal(base_size = 12) +  # reduce base text size
            theme(
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                axis.title = element_text(size = 16),
                axis.text = element_text(size = 14),
                legend.title = element_text(size = 15),
                legend.text = element_text(size = 13),
                legend.background = element_rect(fill = "transparent", colour = NA),
                legend.key = element_rect(fill = "transparent", colour = NA)
            )
    }, height = 800)
    
    
    # -------------------------------
    # Prediction History Reactive Object (Renamed to predHistory)
    # -------------------------------
    predHistory <- reactiveValues(data = data.frame(
        Timestamp = character(),
        Alcohol = numeric(),
        Density = numeric(),
        `Volatile Acidity` = numeric(),
        Chlorides = numeric(),
        `Free Sulfur Dioxide` = numeric(),
        `Residual Sugar` = numeric(),
        `Total Sulfur Dioxide` = numeric(),
        `Citric Acid` = numeric(),
        Sulphates = numeric(),
        `Fixed Acidity` = numeric(),
        `pH` = numeric(),
        `Wine Colour` = character(),
        `Predicted Quality` = character(),
        stringsAsFactors = FALSE
    ))
    
    # -------------------------------
    # Tab 3: Distribution Of Wines Plot
    # -------------------------------
    
    #Distributio by Wine Quality 
    output$dist_quality_plot <- renderPlot({
        ggplot(vinho_verde_data, aes(x = quality_factor, fill = wine_colour)) +
            geom_bar(position = "dodge") +
            labs(
                title = "Wine Quality Ratings Distribution by Wine Colour",
                x = "Quality Rating",
                y = "Count of Wines",
                fill = "Wine Colour",
                caption = "Figure: Distribution of wine quality scores by wine colour."
            ) +
            scale_fill_manual(values = c("Red" = "darkred", "White" = "goldenrod")) +
            theme_minimal(base_size = 15) +
            theme(
                plot.title = element_text(hjust = 0.5, face = "bold",size = 24),
                axis.title = element_text(face = "bold"),
                legend.position = "top"
            )
    })
    
    #Distributio by Wine Review
    output$dist_score_plot <- renderPlot({
        ggplot(wine_reviews_portugal_clean, aes(x = score, fill = wine_colour)) +
            geom_histogram(binwidth = 1, position = "dodge") +
            labs(
                title = "Wine Review Scores Distribution by Wine Colour",
                x = "Review Score",
                y = "Count of Reviews",
                fill = "Wine Colour",
                caption = "Figure: Distribution of review scores by wine colour (Portugal)."
            ) +
            scale_fill_manual(values = c("Red" = "darkred", "White" = "goldenrod")) +
            theme_minimal(base_size = 15) +
            theme(
                plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
                axis.title = element_text(face = "bold"),
                legend.position = "top"
            )
    })  
    
    # -------------------------------
    # Tab 4: Predictors Plots
    # -------------------------------
    output$corr_plot <- renderPlot({
        # Extract numeric columns and rename for clarity
        numeric_features <- vinho_verde_data %>%
            select_if(is.numeric) %>%
            rename(Quality = quality_num)
        
        # Format column names to be more human-readable
        colnames(numeric_features) <- str_to_title(gsub("_", " ", colnames(numeric_features)))
        
        # Save current graphical parameters
        old_par <- par(no.readonly = TRUE)
        
        # Set outer margin to allow space for top title
        par(oma = c(0, 0, 3, 0))  # top margin = 3 lines
        
        # Plot the correlation matrix
        corrplot(
            cor(numeric_features),
            method = "color",
            tl.cex = 1.2,
            tl.col = "black"  # Set all text to black
        )
        
        # Add a centred heading above the plot
        mtext("Correlation Matrix of Wine Data", 
              outer = TRUE, cex = 1.5, col = "black", side = 3, line = 1, adj = 0.5)
        
        # Restore original graphics parameters
        par(old_par)
    })
    
    #Generate variable importance plot
    
    output$varimp_plot <- renderPlot({
        mod <- rf_model
        importance_vector <- mod$variable.importance
        varimp_df <- data.frame(Predictor = names(importance_vector),
                                Importance = as.numeric(importance_vector),
                                stringsAsFactors = FALSE)
        varimp_df$Predictor <- str_to_title(gsub("_", " ", varimp_df$Predictor))
        varimp_df <- varimp_df %>% arrange(Importance)
        varimp_df$Predictor <- factor(varimp_df$Predictor, levels = varimp_df$Predictor)
        ggplot(varimp_df, aes(x = Predictor, y = Importance)) +
            geom_col(fill = "steelblue") +
            coord_flip() +
            labs(title = "Variable Importance",
                 x = "", y = "Importance") +
            theme_minimal(base_size = 16) +
            theme(
                axis.text = element_text(color = "black"),
                axis.title = element_text(color = "black"),
                plot.title = element_text(color = "black", hjust = 0.5)
            )
    })
    
    # -------------------------------
    # Tab 5: Prediction Tool
    # -------------------------------
    output$dynamic_inputs <- renderUI({
        req(feature_ranges)
        r <- feature_ranges
        tagList(
            fluidRow(
                column(4,
                       div(class = "card",
                           h4("Top Predictors"),
                           fluidRow(
                               column(8,
                                      sliderInput("alcohol_slider", "Alcohol", 
                                                  min = r$alcohol_min, max = r$alcohol_max, 
                                                  value = mean(c(r$alcohol_min, r$alcohol_max)), step = 0.1)
                               ),
                               column(4,
                                      numericInput("alcohol_num", NULL, 
                                                   value = mean(c(r$alcohol_min, r$alcohol_max)), 
                                                   min = r$alcohol_min, max = r$alcohol_max, step = 0.1)
                               )
                           ),
                           fluidRow(
                               column(8,
                                      sliderInput("volatile_acidity_slider", "Volatile Acidity", 
                                                  min = r$volatile_acidity_min, max = r$volatile_acidity_max, 
                                                  value = 0.5, step = 0.01)
                               ),
                               column(4,
                                      numericInput("volatile_acidity_num", NULL, 
                                                   value = 0.5, 
                                                   min = r$volatile_acidity_min, max = r$volatile_acidity_max, step = 0.01)
                               )
                           ),
                           fluidRow(
                               column(8,
                                      sliderInput("density_slider", "Density", 
                                                  min = r$density_min, max = r$density_max, 
                                                  value = mean(c(r$density_min, r$density_max)), step = 0.0001)
                               ),
                               column(4,
                                      numericInput("density_num", NULL, 
                                                   value = mean(c(r$density_min, r$density_max)), 
                                                   min = r$density_min, max = r$density_max, step = 0.0001)
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
                                                  min = r$chlorides_min, max = r$chlorides_max, 
                                                  value = 0.05, step = 0.001)
                               ),
                               column(4,
                                      numericInput("chlorides_num", NULL, 
                                                   value = 0.05, 
                                                   min = r$chlorides_min, max = r$chlorides_max, step = 0.001)
                               )
                           ),
                           fluidRow(
                               column(8,
                                      sliderInput("free_sulfur_slider", "Free Sulfur Dioxide", 
                                                  min = r$free_sulfur_dioxide_min, max = r$free_sulfur_dioxide_max, 
                                                  value = 30, step = 1)
                               ),
                               column(4,
                                      numericInput("free_sulfur_num", NULL, 
                                                   value = 30, 
                                                   min = r$free_sulfur_dioxide_min, max = r$free_sulfur_dioxide_max, step = 1)
                               )
                           ),
                           fluidRow(
                               column(8,
                                      sliderInput("residual_sugar_slider", "Residual Sugar", 
                                                  min = r$residual_sugar_min, max = r$residual_sugar_max, 
                                                  value = 5.0, step = 0.1)
                               ),
                               column(4,
                                      numericInput("residual_sugar_num", NULL, 
                                                   value = 5.0, 
                                                   min = r$residual_sugar_min, max = r$residual_sugar_max, step = 0.1)
                               )
                           ),
                           fluidRow(
                               column(8,
                                      sliderInput("total_sulfur_slider", "Total Sulfur Dioxide", 
                                                  min = r$total_sulfur_dioxide_min, max = r$total_sulfur_dioxide_max, 
                                                  value = 115, step = 1)
                               ),
                               column(4,
                                      numericInput("total_sulfur_num", NULL, 
                                                   value = 115, 
                                                   min = r$total_sulfur_dioxide_min, max = r$total_sulfur_dioxide_max, step = 1)
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
                                                  min = r$citric_acid_min, max = r$citric_acid_max, 
                                                  value = 0.3, step = 0.01)
                               ),
                               column(4,
                                      numericInput("citric_acid_num", NULL, 
                                                   value = 0.3, 
                                                   min = r$citric_acid_min, max = r$citric_acid_max, step = 0.01)
                               )
                           ),
                           fluidRow(
                               column(8,
                                      sliderInput("sulphates_slider", "Sulphates", 
                                                  min = r$sulphates_min, max = r$sulphates_max, 
                                                  value = 0.5, step = 0.01)
                               ),
                               column(4,
                                      numericInput("sulphates_num", NULL, 
                                                   value = 0.5, 
                                                   min = r$sulphates_min, max = r$sulphates_max, step = 0.01)
                               )
                           ),
                           fluidRow(
                               column(8,
                                      sliderInput("fixed_acidity_slider", "Fixed Acidity", 
                                                  min = r$fixed_acidity_min, max = r$fixed_acidity_max, 
                                                  value = mean(c(r$fixed_acidity_min, r$fixed_acidity_max)), step = 0.1)
                               ),
                               column(4,
                                      numericInput("fixed_acidity_num", NULL, 
                                                   value = mean(c(r$fixed_acidity_min, r$fixed_acidity_max)), 
                                                   min = r$fixed_acidity_min, max = r$fixed_acidity_max, step = 0.1)
                               )
                           ),
                           fluidRow(
                               column(8,
                                      sliderInput("pH_slider", "pH", 
                                                  min = r$pH_min, max = r$pH_max, 
                                                  value = 3.2, step = 0.01)
                               ),
                               column(4,
                                      numericInput("pH_num", NULL, 
                                                   value = 3.2, 
                                                   min = r$pH_min, max = r$pH_max, step = 0.01)
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
            )
        )
    })
    
    # ----------------------------------
    # Tab 6: Data Dictionaries
    # ----------------------------------
    output$dict_vinho_verde <- renderTable({
        tribble(
            ~Variable, ~Description,
            "fixed_acidity", "Tartaric acid content — affects taste, stability",
            "volatile_acidity", "Acetic acid content — can lead to vinegar taste",
            "citric_acid", "Citric acid adds freshness and balance",
            "residual_sugar", "Sugar remaining after fermentation; indicates sweetness",
            "chlorides", "Salt content of the wine",
            "free_sulfur_dioxide", "Free SO₂ available for microbial and oxidation control",
            "total_sulfur_dioxide", "Combined SO₂ (free + bound)",
            "density", "Density of wine; tied to sugar and alcohol",
            "pH", "Inverse measure of acidity",
            "sulphates", "Preservation and longevity influence",
            "alcohol", "Alcohol by volume",
            "quality", "Sensory quality score (0–10)",
            "wine_colour", "Wine Colour: Red or White",
            "quality_num", "Numeric representation of quality",
            "quality_factor", "Factor representation for plotting"
        )
    })
    
    output$dict_reviews <- renderTable({
        tribble(
            ~Variable, ~Description,
            "country", "Country where the wine was produced",
            "variety", "Grape variety or blend used in the wine",
            "wine_colour", "Categorical variable indicating if the wine is Red, White, or Unknown",
            "score", "Wine rating or score assigned by the reviewer (typically 80–100)",
            "price_USD", "Price of the wine in US Dollars",
            "description", "Textual review containing tasting notes, aromas, and impressions"
        )
    })
    
    # ----------------------------------
    # Synchronize Slider and Numeric Inputs Using Tolerance
    # ----------------------------------
    syncInputs <- function(sliderID, numericID, tol = 1e-4) {
        observeEvent(input[[sliderID]], {
            req(input[[sliderID]])
            if (!isTRUE(all.equal(input[[sliderID]], input[[numericID]], tolerance = tol))) {
                updateNumericInput(session, numericID, value = input[[sliderID]])
            }
        }, ignoreInit = TRUE)
        observeEvent(input[[numericID]], {
            req(input[[numericID]])
            if (!isTRUE(all.equal(input[[numericID]], input[[sliderID]], tolerance = tol))) {
                updateSliderInput(session, sliderID, value = input[[numericID]])
            }
        }, ignoreInit = TRUE)
    }
    
    syncInputs("fixed_acidity_slider", "fixed_acidity_num")
    syncInputs("alcohol_slider", "alcohol_num")
    syncInputs("volatile_acidity_slider", "volatile_acidity_num")
    syncInputs("residual_sugar_slider", "residual_sugar_num")
    syncInputs("free_sulfur_slider", "free_sulfur_num")
    syncInputs("total_sulfur_slider", "total_sulfur_num")
    syncInputs("pH_slider", "pH_num")
    syncInputs("sulphates_slider", "sulphates_num")
    syncInputs("citric_acid_slider", "citric_acid_num")
    syncInputs("chlorides_slider", "chlorides_num")
    syncInputs("density_slider", "density_num")
    
    output$type_text <- renderText({ input$wine_colour })
    
    # ----------------------------------
    # Prediction Using Cached Ranger Model
    # ----------------------------------
    observeEvent(input$predict, {
        print("Predict button pressed.")
        tryCatch({
            user_data <- data.frame(
                fixed_acidity = input$fixed_acidity_num,
                volatile_acidity = input$volatile_acidity_num,
                citric_acid = input$citric_acid_num,
                residual_sugar = input$residual_sugar_num,
                chlorides = input$chlorides_num,
                free_sulfur_dioxide = input$free_sulfur_num,
                total_sulfur_dioxide = input$total_sulfur_num,
                density = input$density_num,
                pH = input$pH_num,
                sulphates = input$sulphates_num,
                alcohol = input$alcohol_num,
                wine_colour = factor(input$wine_colour, levels = c("Red", "White"))
            )
            user_data$quality <- NA
            
            cached_model <- rf_model
            pred_numeric <- predict(cached_model, data = user_data)$predictions
            pred_numeric_2dec <- round(pred_numeric, 2)
            
            output$result_box <- renderUI({
                div(class = "result-box",
                    HTML(paste("🍇 <b>Predicted Wine Quality is </b> ", pred_numeric_2dec))
                )
            })
            
            output$quality_summary_text <- renderUI({
                div(style = "text-align: center; font-size: 1.1em; color: #444;",
                    HTML(paste0("🍇 Based on your inputs, the predicted quality is <b>", 
                                pred_numeric_2dec, "</b>."))
                )
            })
            
            output$percentile_text <- renderUI({
                if (pred_numeric_2dec > 6.4) {
                    message <- "Top quality wine!"
                    emoji <- "🥇"
                    color <- "green"
                } else if (pred_numeric_2dec >= 5.2 && pred_numeric_2dec <= 6.4) {
                    message <- "Average quality wine."
                    emoji <- "🍷"
                    color <- "orange"
                } else {
                    message <- "Below quality wine."
                    emoji <- "⚠️"
                    color <- "red"
                }
                div(style = paste0("text-align: center; font-size: 1.5em; font-weight: 600; color: ", color, ";"),
                    HTML(paste0(emoji, " <b>", message, "</b>"))
                )
            })
            
            output$quality_plot <- renderPlot({
                quality_numeric <- as.numeric(as.character(vinho_verde_data$quality))
                mu <- mean(quality_numeric, na.rm = TRUE)
                sigma <- sd(quality_numeric, na.rm = TRUE)
                x_vals <- seq(mu - 3 * sigma, mu + 3 * sigma, length.out = 100)
                plot(x_vals, dnorm(x_vals, mean = mu, sd = sigma), type = "l", 
                     main = "Wine Quality Distribution (Normal Curve)",
                     xlab = "Quality Rating", ylab = "Density", col = "blue", lwd = 2)
                abline(v = pred_numeric_2dec, col = "red", lwd = 3, lty = 2)
                legend("topright", legend = paste("Your Prediction:", pred_numeric_2dec),
                       col = "red", lty = 2, lwd = 2, box.lty = 0)
            })
            
            # Create a new entry for the prediction history.
            new_entry <- data.frame(
                Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                Alcohol = input$alcohol_num,
                `Volatile Acidity` = input$volatile_acidity_num,
                Density = input$density_num,
                Chlorides = input$chlorides_num,
                `Free Sulfur Dioxide` = input$free_sulfur_num,
                `Residual Sugar` = input$residual_sugar_num,
                `Total Sulfur Dioxide` = input$total_sulfur_num,
                `Citric Acid` = input$citric_acid_num,
                Sulphates = input$sulphates_num,
                `Fixed Acidity` = input$fixed_acidity_num,
                `pH` = input$pH_num,
                `Wine Colour` = input$wine_colour,
                `Predicted Quality` = as.character(pred_numeric_2dec),
                stringsAsFactors = FALSE
            )
            # Ensure new_entry has the same column order as predHistory$data by filling missing columns if any.
            missing_cols <- setdiff(names(predHistory$data), names(new_entry))
            if(length(missing_cols) > 0){
                for(col in missing_cols){
                    new_entry[[col]] <- NA
                }
            }
            new_entry <- new_entry[, names(predHistory$data), drop = FALSE]
            predHistory$data <- bind_rows(predHistory$data, new_entry)
            
        }, error = function(e) {
            showNotification(paste("Error in prediction:", e$message), type = "error")
            print(e)
        })
    })
    
    # ----------------------------------
    # Reset Prediction Outputs
    # ----------------------------------
    observeEvent(input$reset, {
        print("Reset button pressed.")
        output$result_box <- renderUI({ NULL })
        output$quality_summary_text <- renderUI({ NULL })
        output$percentile_text <- renderUI({ NULL })
        output$quality_plot <- renderPlot({ NULL })
    })
    
    # ----------------------------------
    # Prediction History Tab: Display and Download
    # ----------------------------------
    output$prediction_history <- renderTable({
        local_data <- predHistory$data
        if (nrow(local_data) == 0) return(data.frame("No prediction history available."))
        local_data[, names(predHistory$data), drop = FALSE]
    })
    
    observeEvent(input$reset_history, {
        predHistory$data <- data.frame(
            Timestamp = character(),
            Alcohol = numeric(),
            `Volatile Acidity` = numeric(),      
            Density = numeric(),
            Chlorides = numeric(),
            `Free Sulfur Dioxide` = numeric(),
            `Residual Sugar` = numeric(),
            `Total Sulfur Dioxide` = numeric(),
            `Citric Acid` = numeric(),
            Sulphates = numeric(),
            `Fixed Acidity` = numeric(),
            `pH` = numeric(),
            `Wine Colour` = character(),
            `Predicted Quality` = character(),
            stringsAsFactors = FALSE
        )
    })
    
    output$download_history <- downloadHandler(
        filename = function() { paste("prediction_history_", Sys.Date(), ".csv", sep = "") },
        content = function(file) {
            write.csv(predHistory$data, file, row.names = FALSE)
        }
    )
    
    # ----------------------------------
    # Filtered Results Tab: Filter Test Set and Reorder Columns (Limited to 10 Red and 10 White per Filter)
    # ----------------------------------
    filtered_test_set <- reactive({
        filtered <- if (input$quality_filter == "All") {
            vinho_verde_test_set
        } else if (input$quality_filter == "Top Quality") {
            vinho_verde_test_set[vinho_verde_test_set$quality %in% c("8", "9"), ]
        } else if (input$quality_filter == "Average Quality") {
            vinho_verde_test_set[vinho_verde_test_set$quality %in% c("5", "6", "7"), ]
        } else if (input$quality_filter == "Poor Quality") {
            vinho_verde_test_set[vinho_verde_test_set$quality %in% c("3", "4"), ]
        }
        reds <- filtered %>% filter(wine_colour == "Red") %>% head(10)
        whites <- filtered %>% filter(wine_colour == "White") %>% head(10)
        bind_rows(reds, whites)
    })
    
    output$filteredTable <- renderTable({
        req(filtered_test_set())
        fd <- filtered_test_set() %>%
            rename(
                `Fixed Acidity` = fixed_acidity,
                `Volatile Acidity` = volatile_acidity,
                `Citric Acid` = citric_acid,
                `Residual Sugar` = residual_sugar,
                Chlorides = chlorides,
                `Free Sulfur Dioxide` = free_sulfur_dioxide,
                `Total Sulfur Dioxide` = total_sulfur_dioxide,
                Density = density,
                `pH` = pH,
                Sulphates = sulphates,
                Alcohol = alcohol,
                `Wine Colour` = wine_colour,
                Quality = quality
            )
        fd <- fd[, desired_order_filtered, drop = FALSE]
        fd
    })
}