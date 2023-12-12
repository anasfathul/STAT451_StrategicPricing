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


oj_df <- read_csv("oj.csv", show_col_types = FALSE) %>% clean_names()


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
      "We will use a simple linear regression model to demonstrate how price elasticities vary across different brands.
      In selecting variables for our linear model, we consider factors that significantly impact price elasticity.
      To ensure the robustness of our model, we employ a 5-fold cross-validation technique.
      This approach involves splitting the data into five parts, using each part in turn for validation while training the model on the remaining four parts.
      By doing so, we can effectively evaluate the model's performance and calculate the mean squared error (MSE) across different subsets of data,
      ensuring a more reliable and generalizable assessment of our model's accuracy.
      The question is how would we choose the variables in our linear model and how would our model perform in term of MSE?"
    ),
    
    titlePanel("Model Complexity Selection"),
  
    sidebarLayout(
      sidebarPanel(
        selectInput("model_complexity", "Choose Model Complexity:",
                    choices = c("linear regression by brand",
                                "linear regression by brand with interaction",
                                "general linear regression",
                                "linear regression by brand with features"))
      ),
      mainPanel(
        uiOutput("modelFormula"),
        plotOutput("lmPlot"),
        htmlOutput("lmTable")
      )
    )
  )
  
  ### LASSO and Ridge Panel
  
  lasso_ridge <- tabPanel(
    "LASSO and Ridge regression",
    p(
      "This segment of our analysis introduces LASSO (Least Absolute Shrinkage and Selection Operator) and Ridge regression techniques,
      both pivotal in refining our linear regression models. LASSO, renowned for its ability to perform feature selection,
      applies L1 regularization which can shrink some coefficients to zero, effectively removing less significant variables from the model.
      This trait makes LASSO an excellent tool for models plagued by high-dimensionality or when aiming to enhance model interpretability by reducing the number of predictors.
      On the other hand, Ridge regression employs L2 regularization, which penalizes the size of the coefficients but doesn't set them to zero.
      This approach is particularly beneficial in handling multicollinearity, where predictor variables are highly correlated.
      In this interactive module, users can experiment with varying degrees of LASSO and Ridge penalties to observe their impact on feature selection and model accuracy.
      The central question we address here is:
      'How do LASSO and Ridge regression affect the selection of features in our linear model, and what balance of these techniques yields the most effective and interpretable model for predicting orange juice sales?'"
    ),
    
    titlePanel("Ridge"),
    sidebarLayout(
      sidebarPanel(
        selectInput("selectedPlot", "Choose Plot:",
                    choices = c("Cross-Validation Plot" = "cv_plot",
                                "Coefficient Path Plot" = "glm_plot"))
      ),
      mainPanel(
        plotOutput("lassoRidge"),
        uiOutput("plotTextLasso")
      )
    ),
    titlePanel("LASSO Analysis"),
    p(
      "In this section, we delve into the LASSO (Least Absolute Shrinkage and Selection Operator) model to identify which parameters are ",
      strong("excluded"), " as a result of LASSO regularization.
      Pinpointing these parameters allows us to gain insights into the most impactful predictors, enabling the construction of a more efficient and targeted model.
      This analysis facilitates the exclusion of non-contributing parameters, thereby enhancing the overall model effectiveness."
    ),
    htmlOutput("lassoTable")
  )
  
  ### Regression Tree Panel
  
  regression_tree <- tabPanel(
    "Regression Tree",
    p(
      "This section of our analysis explores the use of Regression Trees, a powerful non-linear modeling technique that is part of the decision tree family.
      Unlike linear models, Regression Trees segment the predictor space into distinct and non-overlapping regions, making them particularly adept at capturing complex, non-linear relationships between variables.
      Each 'split' in a Regression Tree represents a decision rule that is intuitively understandable, which makes these models highly interpretable.
      A key aspect of Regression Trees is their ability to handle a mix of numerical and categorical variables and automatically model interactions between predictors.
      However, one challenge with Regression Trees is determining the optimal complexity.
      Too simple, and the model may underfit; too complex, and it risks overfitting.
      Question to explore: How does the complexity parameter influence the structure and predictions of the regression tree model for our dataset?
      Experiment with different values of the complexity parameter using the slider. 
      Observe how the tree structure changes with varying levels of complexity. 
      Can you identify a complexity level that provides a balance between a too simplistic and an overly complex tree? 
      Consider how the depth and number of splits in the tree impact its ability to accurately predict the target variable without overfitting to the training data"
    ),
    
    titlePanel("Regression Tree"),
    
    sidebarLayout(
      sidebarPanel(
        # Slider for selecting tree complexity
        sliderInput("treeComplexity", "Select Tree Complexity:",
                    min = 0.001, max = 0.009, value = 0.007, step = 0.001)
      ),
      mainPanel(
        plotOutput("regressionTreePlot")
      )
    ),
    titlePanel("Regression Tree MSE Calculation"),
    p(
      "Feel free to choose a variable and a split point to calculate the MSE of the regression tree model."
    ),
    sidebarLayout(
      sidebarPanel(
        # Dropdown for selecting the variable
        selectInput("treeVariable", "Select Variable:",
                    choices = c("age60", "educ", "ethnic", "hhlarge", "workwom", "hval150")),
        
        # Input for specifying the split point
        numericInput("treeSplit", "Enter Split Point:", 
                     value = 0.007,
                     min = 0.001, 
                     max = 0.009,
                     step = 0.01) # Adjust step size as appropriate for your data
      ),
      
      mainPanel(
        textOutput("mse_regressionTree") # Placeholder for MSE output
      )
    ),
    titlePanel("Regression Tree Complexity Visualization"),
    p(
      "This visualization presents a series of lines, each corresponding to a different variable in the dataset, with varying colors for clarity.
      The y-axis represents the mean squared error (MSE), and the x-axis displays the range of potential split point values.
      This plot is instrumental in understanding how the regression tree model determines the optimal split points for different variables.
      It illustrates the impact of each variable's split point on the model's predictive accuracy, guiding us in identifying the most effective variables and their respective split points for tree construction."
    ),
    mainPanel(
      plotOutput("treeComplexityDesc")
    )
  )
  
### random_forest and xgboost
  
  random_forest <- tabPanel(
    "Random Forest Model & XGBoost",
    p(
      "In this section, we delve into the application of Random Forest models for predicting orange juice sales across different brands.
      Unlike a single decision tree, a Random Forest model combines multiple decision trees to improve prediction accuracy and reduce overfitting.
      Here, users can adjust key parameters of the Random Forest, such as the number of trees in the forest and the maximum depth of each tree.
      The goal is to find the optimal combination of these parameters that yields the lowest Mean Squared Error (MSE).
      By experimenting with these settings, users can observe how the complexity of the model affects its performance.
      The underlying question we aim to explore is:
      'What are the optimal parameters for a Random Forest model to accurately predict orange juice sales, and how do these parameters influence the model's accuracy and generalizability?"
    ),
    
    titlePanel("Model complexity")
  )

### Double Machine Learning Panel
  
  double_machine_learning <- tabPanel(
    "Double Machine Learning",
    p(
      "Double Machine Learning for causal inference."
    ),
    
    titlePanel("Double Machine Learning")
  )
  
# Define `ui` storing a `navbarPage()` element containing the pages above
ui <- navbarPage(
  "Strategic Pricing",
  overview_panel,
  linear_regression,
  lasso_ridge,
  regression_tree,
  random_forest,
  double_machine_learning
  #graph_panel
)