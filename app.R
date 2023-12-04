# Final Project : Strategic Pricing Using Machine Learning

# Load libraries
library("shiny")

# Executing the `app_ui.R` and `app_server.R` files with source().
# Define the UI value and server function respectively.
source("app_ui.R")
source("app_server.R")

# `app_ui.R` file will create the layout.
# Run the app through this file.

# Create a new `shinyApp()` using the loaded `ui` and `server` variables
shinyApp(ui = ui, server = server)