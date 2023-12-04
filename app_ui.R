# app_ui.R

# Define library
library(shiny)


suppressPackageStartupMessages({
  library(ggplot2)
  library(ggthemes)
  library(tidyverse)
  library(broom)
  library(knitr)
  library(janitor)
  library(plotly)
  library(dplyr)
})

options(dplyr.summarise.inform = FALSE) # turns off an annoying dplyr behavior

set.seed(487)

co2_emission_data <- 
  read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

oj_df <- read_csv("oj.csv", show_col_types = FALSE) %>% clean_names()

# Extracting the data by country and feature
countries <- co2_emission_data %>% distinct(country) %>% pull(country)

features <- colnames(co2_emission_data)

datamod <- co2_emission_data %>% filter(year >= 1900)

date_range <- range(datamod$year)

# Setting the feature and widget
sidebar_country <- selectInput(inputId = "varcountry",
                               label = "Pick a country",
                               choices = countries)


sidebar_feature <- selectInput(
  inputId = "varfeature",
  label = "Pick one of the feature",
  choices =
    list("Annual production of Carbon Dioxide" = "co2",
         "Consumption-based Carbon Dioxide" = "consumption_co2",
         "Cumulative Emission of Carbon Dioxide" = "cumulative_co2",
         "Carbon Dioxide per Capita" = "co2_per_capita",
         "Difference of Carbon Dioxide between Import and Export" = "trade_co2")
)


sidebar_date <- sliderInput(
  inputId = "date",
  label = "Date range",
  min = date_range[1],
  max = date_range[2],
  value = c(1950, 2015),
  sep = ""
)


graph_main_content <- mainPanel(plotlyOutput(
  outputId = "graph",
  width = "150%")
)

graph_panel <- tabPanel(
  "Graphical View",
  titlePanel("Emission of CO2 by country based on feature and years"),
  p(textOutput("intro_graph")),
  sidebarLayout(
    sidebarPanel(sidebar_country,
                 sidebar_feature,
                 sidebar_date,
                 width = 20),
    mainPanel(graph_main_content, width = 100)),
  p(textOutput("summary"))
)

### Data Overview Panel

overview_panel <- tabPanel(
  "Data Overview",
  titlePanel("Introduction"),
  p(
    "In this project, I aim to analyze the strategic pricing of orange juice brands using linear machine learning models.
    Leveraging the Dominick's dataset from the",
    a("Chicago Booth",
      href = "https://www.chicagobooth.edu/research/kilts/research-data/dominicks"
    ),
    " School of Business, which encompasses weekly sales data of orange juice from 1989 to 1992,
    I plan to examine how various factors influence pricing strategies.
    These factors include the prices of competing brands (Tropicana, Minute Maid, and Dominicks),
    promotional activities, and store demographic information.
    My key question is: 'How are the prices of orange juice brands influenced by their competitors' pricing, promotional efforts, and store demographics?'
    To answer this, I will employ a range of linear models, evaluating their effectiveness through cross-validation and comparing their Mean Squared Error.
    This approach will not only help in understanding the pricing dynamics in the orange juice market but also in comparing the predictive power of different machine learning models in a real-world business context.
    I hope that this interactive Shiny application could provide a useful tool in giving an introduction to use data science in strategic pricing."
  ),
  
  h2("Dataset Information"),
  
  p(
    "The dataset contains 3,907 observations and 14 variables.
    The variables include the following:
    'logmove' (log of sales volume),
    'price' (price of the product),
    'feat' (whether the product was featured in the store),
    'age60' (percentage of the population over 60 years old),
    'educ' (average years of education of the population),
    'eth' (percentage of the population that is African American),
    'hval150' (median housing value in $1,000s),
    'hval450' (median housing value in $100,000s),
    'income' (median household income in $1,000s),
    'loginc' (log of median household income in $1,000s),
    'price' (price of the product),
    'pctmin80' (percentage of the population that is a minority),
    'wic' (percentage of the population on welfare),
    'year' (year of the observation)."
  ),
  
  titlePanel("Dataset Preview"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("visualization_type", "Choose Type of Visualization:",
                  choices = c("Scatter Plot of log(Quantity) vs. Price by brand",
                              "Scatter Plot of log(Quantity) vs. log(Price) by brand",
                              "Boxplot of Price by brand",
                              "Boxplot of log(Price) by brand"))
    ),
    mainPanel(
      plotOutput("dfVisualization")
    )
  )
  
  )

### Linear Regression Panel

  linear_regression <- tabPanel(
    "Linear Regression",
    p(
      "We will use a simple linear regression model to show how the price elasticities vary by brand.
      The question is how would we choose the variables in our linear model?"
    ),
    
    titlePanel("Model Complexity Selection"),
  
    sidebarLayout(
      sidebarPanel(
        selectInput("model_complexity", "Choose Model Complexity:",
                    choices = c("linear regression by brand",
                                "linear regression by brand with interaction"))
      ),
      mainPanel(
        plotOutput("lmPlot"),
        uiOutput("modelFormula")
      )
    )
  )
  
### K-fold Cross Validation Panel
  
  k_fold_CV <- tabPanel(
    "K-fold Cross Validation",
    p(
      "Now, instead of using all of our dataset to predict our quantity sold by each brand,
      we will split our dataset into train and test data set. In this page, the user could choose the number of folds for the cross validation.
      And then, we can see the average MSE of all the folds choosen.
      The question is how many fold will be sufficient for our cross validation?"
    ),
    
    titlePanel("Model complexity")
  )

### Regression Tree Panel
  
  regression_tree <- tabPanel(
    "Regression Tree",
    p(
      "In this section of our analysis, we will focus on building a regression tree model to predict a specific target variable.
      To ensure the robustness of our model, we'll divide our dataset into training and testing subsets.
      Users will have the flexibility to select the depth of the tree, which influences the complexity of the model.
      Additionally, we will implement cross-validation to evaluate the model's performance.
      The key aspect here is determining the optimal number of folds for cross-validation to obtain a reliable estimate of the model's performance.
      The interface will display the mean squared error (MSE) across different folds,
      aiding in deciding the most effective number of folds for our regression tree model"
    ),
    
    titlePanel("Regression Tree")
  )
  
### LASSO and Ridge Panel
  
  lasso_ridge <- tabPanel(
    "LASSO and Ridge regression",
    p(
      "In this section of our analysis, we will focus on building a."
    ),
    
    titlePanel("LASSO"),
    titlePanel("Ridge")
  )

### Double Machine Learning Panel
  
  double_machine_learning <- tabPanel(
    "Double Machine Learning",
    p(
      "In this section of our analysis, we will focus on building a."
    ),
    
    titlePanel("Double Machine Learning")
  )
  
# Define `ui` storing a `navbarPage()` element containing the pages above
ui <- navbarPage(
  "Strategic Pricing",
  overview_panel,
  linear_regression,
  k_fold_CV,
  regression_tree,
  lasso_ridge,
  double_machine_learning
  #graph_panel
)