# Server.R

# Define library
library(tidyverse)
library(shiny)

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggthemes)
  library(tidyverse)
  library(maptree)
  library(glmnet)
  library(broom)
  library(knitr)
  library(dplyr)
  library(kableExtra)
})

options(dplyr.summarise.inform = FALSE) # turns off an annoying dplyr behavior

set.seed(487)

oj_df <- read_csv("oj.csv", show_col_types = FALSE) %>% clean_names()

server <- function(input, output) {
  
  # dataset visualizations
  output$dfVisualization <- renderPlot({
    
    scatterplot_price_df <- ggplot(data = oj_df) + 
      geom_point(aes(x = logmove, y = price, color = brand), alpha=.3) +
      labs(title = "Does Brand Influence the Price and Sales Volume of Orange Juice?",
           y = 'price',
           x = 'log(quantity)') + 
      theme_bw() +
      theme(plot.title = element_text(face = "bold")) +
      scale_color_colorblind()
    
    scatterplot_logprice_df <- ggplot(data = oj_df) + 
      geom_point(aes(x = logmove, y = log(price), color = brand), alpha=.3) +
      labs(title = "Does Brand Influence the log(Price) and Sales Volume of Orange Juice?",
           y = 'log(price)',
           x = 'log(quantity)') + 
      theme_bw() +
      theme(plot.title = element_text(face = "bold")) +
      scale_color_colorblind()
    
    boxplot_df <- ggplot(data = oj_df) + 
      geom_boxplot(aes(x = brand, y = price, fill = brand)) +
      labs(title = "How Do Orange Juice Prices Vary by Brand?",
           y = 'Price',
           x = 'Brand') + 
      theme_bw() +
      theme(plot.title = element_text(face = "bold")) +
      scale_fill_colorblind()
    
    boxplot_log_df <- ggplot(data = oj_df) + 
      geom_boxplot(aes(x = brand, y = log(price), fill = brand)) +
      labs(title = "How Do Orange Juice log(Prices) Vary by Brand?",
           y = 'Log-Price',
           x = 'Brand') + 
      theme_bw() +
      theme(plot.title = element_text(face = "bold")) +
      scale_fill_colorblind()
    
    if (input$visualization_type == "Scatter Plot of log(Quantity) vs. Price by brand") {
      scatterplot_price_df
    } else if (input$visualization_type == "Scatter Plot of log(Quantity) vs. log(Price) by brand"){
      scatterplot_logprice_df
    } else if (input$visualization_type == "Boxplot of Price by brand") {
      boxplot_df
    } else if (input$visualization_type == "Boxplot of log(Price) by brand") {
      boxplot_log_df
    } else {
      paste0("Please select a type of visualization")
    }
  })
  
  # Equation
  brand_reg <- lm(
    formula = logmove ~ log(price) + brand,
    data = oj_df
  )
  
  brand_int_reg <- lm(
    formula = logmove ~ log(price) + brand + brand:log(price),
    data = oj_df
  )
  
  basic_reg <- lm(
    formula = logmove ~ log(price),
    data = oj_df
  )
  
  feat_one_int_reg <- lm(
    formula = logmove ~ log(price) + feat + log(price):feat + brand + brand:log(price),
    data = oj_df
  )
  
  # lm table
  output$lmTable <- renderText({
    
    if (input$model_complexity == "linear regression by brand") {
      tidy(brand_reg) %>% kable() %>% kable_styling()
    } else if (input$model_complexity == "linear regression by brand with interaction"){
      tidy(brand_int_reg) %>% kable("html") %>% kable_styling()
    } else if (input$model_complexity == "general linear regression"){
      tidy(basic_reg) %>% kable("html") %>% kable_styling()
    } else if (input$model_complexity == "linear regression by brand with features"){
      tidy(feat_one_int_reg) %>% kable("html") %>% kable_styling()
    } else {
      paste0("Please select a model complexity")
    }
  })
  
  output$lmPlot <- renderPlot({
    
    by_brand <- oj_df %>% 
      mutate(fitted_y = predict(brand_reg, newdata=.)) %>% 
      ggplot(data = .) + 
      geom_point(aes(x = logmove, y = log(price), color = brand), alpha=.1) +
      geom_line(aes(x = fitted_y, y = log(price), color = brand), linewidth = 2) +
      labs(y = 'log(price)',
           x = 'log(quantity)') + 
      theme_bw() +
      scale_color_colorblind()
    
    brand_intercept <- oj_df %>% 
      mutate(fitted_y = predict(brand_int_reg, newdata=.)) %>% 
      ggplot(data = .) + 
      geom_point(aes(x = logmove, y = log(price), color = brand), alpha=.1) +
      geom_line(aes(x = fitted_y, y = log(price), color = brand), linewidth = 2) +
      labs(y = 'log(price)',
           x = 'log(quantity)') + 
      theme_bw() +
      scale_color_colorblind()
    
    basic_reg_plot <- oj_df %>% 
      mutate(fitted_y = predict(basic_reg, newdata=.)) %>% 
      ggplot(data = .) + 
      geom_point(aes(x = logmove, y = log(price), color = brand), alpha=.1) +
      geom_line(aes(x = fitted_y, y = log(price), color = brand), linewidth = 2) +
      labs(y = 'log(price)',
           x = 'log(quantity)') + 
      theme_bw() +
      scale_color_colorblind()
    
    reg_with_feat <- oj_df %>% 
      mutate(fitted_y = predict(feat_one_int_reg, newdata=.)) %>% 
      ggplot(data = .) + 
      geom_point(aes(x = logmove, y = log(price), color = brand, shape = factor(feat)), alpha=.1) +
      geom_line(aes(x = fitted_y, y = log(price), color = brand, linetype = factor(feat)), linewidth = 2) +
      labs(y = 'log(price)',
           x = 'log(quantity)') + 
      theme_bw() +
      scale_color_colorblind()
    
    if (input$model_complexity == "linear regression by brand") {
      by_brand
    } else if (input$model_complexity == "linear regression by brand with interaction"){
      brand_intercept
    } else if (input$model_complexity == "general linear regression"){
      basic_reg_plot
    } else if (input$model_complexity == "linear regression by brand with features"){
      reg_with_feat
    } else {
      paste0("Please select a model complexity")
    }
  })
  
  output$modelFormula <- renderUI({
    if (input$model_complexity == "linear regression by brand") {
      HTML("<strong>log(quantity) = log(price) + brand<strong><br>MSE=0.321")
    } else if (input$model_complexity == "linear regression by brand with interaction") {
      HTML("<strong>log(quantity) = log(price) + brand + brand:log(price)<strong><br>MSE=0.297")
    } else if (input$model_complexity == "general linear regression"){
      HTML("<strong>log(quantity) = log(price)<strong><br>MSE=0.37")
    } else if (input$model_complexity == "linear regression by brand with features"){
      HTML("<strong>log(quantity) = log(price) + feat + log(price):feat + brand + brand:log(price)<strong><br>MSE=0.27")
    } else {
      HTML("<strong>Please select a model complexity<strong>")
    }
  })

  ### Function for lasso and ridge
  oj_df_lasso <- oj_df %>%
    mutate(log_price = log(price)) %>% 
    arrange(week) %>% # sort the data by week
    group_by(store, brand) %>% # only lag within a store and brand
    mutate(lag_price = ifelse(lag(week) + 1 == week, lag(log_price), NA)) %>% # calculate lagged prices only if subsequent weeks
    ungroup() %>% 
    filter(!is.na(lag_price)) # remove null lagged prices
  
  lhs_vars <- oj_df_lasso %>% 
    select(-store, -week, -logmove, -price) %>% 
    colnames()
  
  reg_str_int <- str_c(
    '~(',
    str_c(
      lhs_vars,
      collapse = ' + '
    ),
    '-1)^2'
  )
  
  X <- model.matrix(formula(reg_str_int), oj_df_lasso)
  y <- oj_df_lasso$logmove
  
  cv_fit <- cv.glmnet(X, y, alpha = 1, nfolds = 5)
  glm_fit <- glmnet(X, y, alpha = 1)
  
  cv_coef_results <- coef(cv_fit, s = 'lambda.1se')
  lasso_mse_text <- cv_fit$cvm[which(cv_fit$lambda == cv_fit$lambda.1se)]
  
  cv_coef_results_df <- data.frame(
    'variable' = unlist(cv_coef_results@Dimnames[1]),
    'coefficient' = as.numeric(cv_coef_results)
  )
  
  ### LASSO and Ridge features selection
  output$lassoRidge <- renderPlot({
    
    if (input$selectedPlot == "cv_plot") {
      plot(cv_fit) # Assuming cv_fit is your cross-validation object
    } else if (input$selectedPlot == "glm_plot") {
      plot(glm_fit)
    } else {
      paste0("Please select a model plot")
    }
  })
  
  output$plotTextLasso <- renderUI({
    if (input$selectedPlot == "cv_plot") {
      print(str_interp('LASSO CV MSE: ${round(lasso_mse_text, 2)}. This plot illustrates the cross-validation results from a glmnet model.
      The y-axis shows the mean squared error (MSE) for each value of lambda, while the x-axis represents the logarithm of lambda.
      The dotted lines represent the MSE for each fold in the cross-validation process as lambda changes.
      The two vertical dotted lines mark the optimal values of lambda: one for the minimum MSE and the other for the most regularized model within one standard error of the minimum.
      This visualization is key for selecting the best lambda value for regularization, balancing model complexity and prediction accuracy.'))
    } else if (input$selectedPlot == "glm_plot") {
      print("This plot visualizes the coefficient paths of the glmnet model as the regularization parameter, lambda, varies.
            Each line represents a different predictor variable, and the plot shows how each coefficient is shrunk towards zero as lambda increases.
            This visualization is crucial for understanding the impact of regularization on the model and helps in identifying the most significant predictors.")
    }
  })
  
  output$lassoTable <- renderText({
    cv_coef_results_df %>% 
      filter(coefficient == 0) %>% 
      kable("html") %>% kable_styling()
  })
  
  # Regression Tree
  
  oj_tree <- oj_df_lasso %>% 
    mutate(q = exp(logmove)) %>% 
    group_by(store, week) %>% 
    mutate(weighted_mean = weighted.mean(price, q)) %>% 
    ungroup()
  
  reg_tree_data <- oj_tree %>% 
    select(weighted_mean, age60:cpwvol5)
  
  show_tree <- function(cp_val){
    fit<-rpart(as.formula(weighted_mean ~ .),
               data=reg_tree_data,
               method="anova",
               cp=cp_val)
    
    draw.tree(fit)
  }
  
  tree_data <- oj_tree %>% 
    select(weighted_mean, age60, educ, ethnic, hhlarge, workwom, hval150)
  
  mse_split <- function(var_name, split_point){
    tree_data %>% 
      mutate(above = !!sym(var_name) >= split_point) %>% 
      group_by(above) %>%
      mutate(group_mean = mean(weighted_mean)) %>%
      ungroup() %>%
      mutate(error_sq = (weighted_mean - group_mean)^2) %>%
      pull(error_sq) %>%
      mean(.)
  }
  
  get_all_splits <- function(var_name){
    data_vec <- tree_data %>% pull(var_name)
    
    min_var = ceiling(min(data_vec) * 100) / 100
    max_var = floor(max(data_vec) * 100) / 100
    grid = seq(min_var, max_var, .01)
    
    df = data.frame(
      'var' = var_name,
      'split_val' = grid,
      'mse' = sapply(grid, function(x){mse_split(var_name, x)})
    )
    return(df)
  }
  
  all_cols <- colnames(tree_data %>% select(-weighted_mean))
  full_splits <- bind_rows(lapply(all_cols, get_all_splits))
  
  output$treeComplexityDesc <- renderPlot({
    ggplot(data = full_splits) +
      geom_point(aes(x = split_val, y = mse, color = var)) + 
      geom_line(aes(x = split_val, y = mse, color = var)) +
      scale_x_continuous(expand = c(0,0), limits = c(0,1), n.breaks = 11) + 
      theme_bw() +
      scale_color_colorblind()
  })
  
  
  output$regressionTreePlot <- renderPlot({
    show_tree(input$treeComplexity)
  })
  
  output$mse_regressionTree <- renderText({
    mse_split(input$treeVariable, input$treeSplit)
  })
  
  
  
  ### Random Forest
  output$ranForest <- renderText({
    
    if (input$model_complexity == "linear regression by brand") {
      tidy(brand_reg) %>% kable() %>% kable_styling()
    } else if (input$model_complexity == "linear regression by brand with interaction"){
      tidy(brand_int_reg) %>% kable("html") %>% kable_styling()
    } else if (input$model_complexity == "general linear regression"){
      tidy(basic_reg) %>% kable("html") %>% kable_styling()
    } else if (input$model_complexity == "linear regression by brand with features"){
      tidy(feat_one_int_reg) %>% kable("html") %>% kable_styling()
    } else {
      paste0("Please select a model complexity")
    }
  })
}