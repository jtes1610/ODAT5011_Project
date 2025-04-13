# ----------------------------------------------------------
# Wine Quality App Entry Point
# ----------------------------------------------------------
# This script serves as the main launcher for the Shiny App.
# It loads the global settings, user interface (UI), and server logic
# from their respective files inside the 'app' folder.

# ----------------------------------------------------------
# Load Global Configurations and Dependencies
# ----------------------------------------------------------
# This includes library imports, data loading, and any global variables
source("app/global.R")

# ----------------------------------------------------------
# Load User Interface Definitions
# ----------------------------------------------------------
# This script defines the UI layout and tabs for the application
source("app/ui.R")

# ----------------------------------------------------------
# Load Server Logic
# ----------------------------------------------------------
# This handles user inputs, outputs, reactivity, and model predictions
source("app/server.R")

# ----------------------------------------------------------
# Launch the Shiny Application
# ----------------------------------------------------------
# Runs the app using the defined UI and server components
shinyApp(ui = ui, server = server)