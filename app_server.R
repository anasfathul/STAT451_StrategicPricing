# Server.R

# Define library
library(tidyverse)
library(shiny)

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggthemes)
  library(tidyverse)
  library(broom)
  library(knitr)
  library(dplyr)
})

options(dplyr.summarise.inform = FALSE) # turns off an annoying dplyr behavior

set.seed(487)

oj_df <- read_csv("oj.csv", show_col_types = FALSE) %>% clean_names()

co2_emission_data <-
  read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

server <- function(input, output) {
  
  # dataset visualizations
  output$dfVisualization <- renderPlot({
    
    scatterplot_price_df <- ggplot(data = oj_df) + 
      geom_point(aes(x = logmove, y = price, color = brand), alpha=.3) +
      labs(y = 'price',
           x = 'log(quantity)') + 
      theme_bw() +
      scale_color_colorblind()
    
    scatterplot_logprice_df <- ggplot(data = oj_df) + 
      geom_point(aes(x = logmove, y = log(price), color = brand), alpha=.3) +
      labs(y = 'log(price)',
           x = 'log(quantity)') + 
      theme_bw() +
      scale_color_colorblind()
    
    boxplot_df <- ggplot(data = oj_df) + 
      geom_boxplot(aes(x = brand, y = price, fill = brand)) +
      labs(y = 'Price',
           x = 'Brand') + 
      theme_bw() +
      scale_fill_colorblind()
    
    boxplot_log_df <- ggplot(data = oj_df) + 
      geom_boxplot(aes(x = brand, y = log(price), fill = brand)) +
      labs(y = 'Log-Price',
           x = 'Brand') + 
      theme_bw() +
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
  
  # linear regression
  output$lmPlot <- renderPlot({
    
    brand_reg <- lm(
      formula = logmove ~ log(price) + brand,
      data = oj_df
    )
    
    by_brand <- oj_df %>% 
      mutate(fitted_y = predict(brand_reg, newdata=.)) %>% 
      ggplot(data = .) + 
      geom_point(aes(x = logmove, y = log(price), color = brand), alpha=.1) +
      geom_line(aes(x = fitted_y, y = log(price), color = brand), linewidth = 2) +
      labs(y = 'log(price)',
           x = 'log(quantity)') + 
      theme_bw() +
      scale_color_colorblind()
    
    brand_int_reg <- lm(
      formula = logmove ~ log(price) + brand + brand:log(price),
      data = oj_df
    )
    
    brand_intercept <- oj_df %>% 
      mutate(fitted_y = predict(brand_int_reg, newdata=.)) %>% 
      ggplot(data = .) + 
      geom_point(aes(x = logmove, y = log(price), color = brand), alpha=.1) +
      geom_line(aes(x = fitted_y, y = log(price), color = brand), linewidth = 2) +
      labs(y = 'log(price)',
           x = 'log(quantity)') + 
      theme_bw() +
      scale_color_colorblind()
    
    if (input$model_complexity == "linear regression by brand") {
      by_brand
    } else if (input$model_complexity == "linear regression by brand with interaction"){
      brand_intercept
    } else {
      paste0("Please select a model complexity")
    }
  })
  
  output$modelFormula <- renderUI({
    if (input$model_complexity == "linear regression by brand") {
      HTML("<strong>log(quantity) = log(price) + brand<strong>")
    } else if (input$model_complexity == "linear regression by brand with interaction") {
      HTML("$<strong>log(quantity) = log(price) + brand + brand x log(price)<strong>")
    } else {
      HTML("<strong>Please select a model complexity<strong>")
    }
  })

  output$dataset_overview <- renderPlotly({
    plot_data <- co2_emission_data %>%
      filter(year >= input$date[1], year <= input$date[2]) %>%
      filter(country == input$varcountry) %>%
      select(year, input$varfeature) %>% rename(Year = year)
    
    scatter <- ggplot(data = plot_data) +
      geom_line(mapping = aes_string(x = "Year",
                                     y = input$varfeature)) +
      geom_point(mapping = aes_string(x = "Year", y = input$varfeature)) +
      labs(title = "Carbon Dioxide Emissions data",
           x = "Year",
           y = "Carbon Dioxide in MT")
    
    scatter
  })
  
  output$lowest <- renderText({
    lowest_country <- co2_emission_data %>%
      select(year, country, co2) %>% filter(year == 2020) %>%
      filter(co2 > 0) %>% filter(co2 == min(co2)) %>% pull(country)
    
    lowest_amt <- co2_emission_data %>%
      select(year, country, co2) %>% filter(year == 2020) %>% filter(co2 > 0) %>%
      filter(co2 == min(co2)) %>% pull(co2)
    
    return(paste0(
      "We have found a few key highlights from this dataset.
      In the latest year of 2020 in this dataset, the country with the lowest producing annual Carbon Dioxide is ", lowest_country,
      ", with the least amount of ", lowest_amt, "MT. An interesting fact is that Tuvalu only contained a population of 12 000
      which contained less amount of industry and make zero-emissions a feasible goal to be achieve,"
    ))
  })
  
  output$average <- renderText({
    average <- co2_emission_data %>%
      filter(country == "United States") %>%
      select(year, country, co2) %>% filter(year >= 1800) %>%
      summarise(average = mean(co2, na.rm = T)) %>% round(digits = 2)
    
    return(paste("Now, we found The average Carbon Dioxide Emissions in the United States
                 since the availabilty of the data year of 1800 is ", average, "MT."))
  })
  
  output$top5_2020 <- renderText({
    top5 <- co2_emission_data %>%
      group_by(country) %>% select(year, country, cumulative_co2) %>%
      filter(year == 2020) %>% arrange(-cumulative_co2) %>% pull(country)
    
    
    return(paste0(
      "Next, in the latest year of 2020 available in the dataset, we found the top
      5 countries with the highest annual cumulative of Carbon Dioxide emission.
      Those countries 5 countries are: lead by ",
      top5[5], ", ", top5[10], ", ", top5[12], ", ",
      top5[13], ", and ", top5[15], "."
    ))
  })
  
  output$singapore <- renderText({
    sg_year <- co2_emission_data %>%
      filter(country == "Singapore") %>% select(year, co2_growth_prct) %>%
      filter(co2_growth_prct == max(co2_growth_prct, na.rm = T)) %>% pull(year)
    
    sg_pct <- co2_emission_data %>%
      filter(country == "Singapore") %>% select(year, co2_growth_prct) %>%
      filter(co2_growth_prct == max(co2_growth_prct, na.rm = T)) %>% 
      pull(co2_growth_prct) %>% round(digits = 1)
    
    
    return(paste0(
      "Now, I choose the most developed and wealthiest country in Southeast Asia,
      Singapore to observe the year which the highest percentage change of
      carbon dioxide emission in relative to the previous year and the amount 
      of the percentage change.",
      " Here, we can see that the year was ", sg_year,
      ", with the percentage growth of ",
      sg_pct, "%. That's a big number!"
    ))
  })
  
  output$intro_graph <- renderText({
    paste0("The widget below will give the user to choose the specific country
            with the feature choosen. The slider widget is use so that the user
            could pick the choosen period of time which they wished to observe.")
  })
  
  output$graph <- renderPlotly({
    plot_data <- co2_emission_data %>%
      filter(year >= input$date[1], year <= input$date[2]) %>%
      filter(country == input$varcountry) %>%
      select(year, input$varfeature) %>% rename(Year = year)
    
    scatter <- ggplot(data = plot_data) +
      geom_line(mapping = aes_string(x = "Year",
                                     y = input$varfeature)) +
      geom_point(mapping = aes_string(x = "Year", y = input$varfeature)) +
      labs(title = "Carbon Dioxide Emissions data",
           x = "Year",
           y = "Carbon Dioxide in MT")
    
    scatter
  })
  
  output$summary <- renderText({
    paste0("The chart above is intended so that the user could see the
            trend and growth of Carbon Dioxide over the time for the choosen
            country. The slider widget is use so that the user could pick the
            choosen period of time which they wished to observe. This is crucial
            as the chart could potrait a deeper trend based of the year given.
            In general, I observe that the trend of the consumption of the Carbon
            Dioxide for most countries are increasing. This is true as the human
            population increases and we are consuming more and more energey over
            the time. This is more than worrisome as the emission of co2 which 
            a part of greenhouse gas could damage more of our atmosphere and
            further cause towards an irreversible effect. As for that, I ended 
            this assignment with a call out that we should move towards a green
            and renewable energy before we reach the no turning back point!")
  })
}