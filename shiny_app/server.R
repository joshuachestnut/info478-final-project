source("ui.R")

# 1. Allocation of money spent on food, how that relates to food insecurity?

# 2. Map of Adult vs. Child Food Insecurity

# 3. Trends of Food Insecurity based on different demogrpahics
trends_data <- read.csv("data/usda_data/prepped/food_security_data.csv",
                        header = T,
                        stringsAsFactors = F)

output$house_plot <- renderPlotly({
  if (input$home_types == "All Types") {
    overall_data <- trends_data %>% filter(Category == "All households")
    return(plot_ly(overall_data,
                   x = ~Year,
                   y = ~Food.insecure.Percent,
                   type = "scatter",
                   mode = 'lines') %>%
             layout(title = 'Food Insecurity For All Households',
                    xaxis = list(title = "Year"),
                    yaxis = list (title = "Food Insecurity %")))
  } 
  if (input$home_types == "With children < 18 yrs"|
      input$home_types == "With no children < 18 yrs" |
      input$home_types == "With elderly") {
    overall_data <- trends_data %>% filter(SubCategory == input$home_types)
    return(plot_ly(overall_data,
                   x = ~Year,
                   y = ~Food.insecure.Percent,
                   type = "scatter",
                   mode = 'lines') %>%
             layout(title = paste0("Food Insecurity For", input$home_types, "Households"),
                    xaxis = list(title = "Year"),
                    yaxis = list (title = "Food Insecurity %")))
  } else {
    new_data <- trends_data %>% filter(SubSubCategory == input$home_types)
    return(plot_ly(new_data,
                   x = ~Year,
                   y = ~Food.insecure.Percent,
                   type = "scatter",
                   mode = 'lines') %>%
             layout(title = paste0("Food Insecurity For", input$home_types, "Households"),
                    xaxis = list(title = "Year"),
                    yaxis = list (title = "Food Insecurity %")))
  }
})

output$race_plot <- renderPlotly({
  other_data <- trends_data %>% filter(SubCategory == input$chosen_race) 
  return(plot_ly(overall_data,
                 x = ~Year,
                 y = ~Food.insecure.Percent,
                 type = "scatter",
                 mode = 'lines') %>%
           layout(title = paste0("Food Insecurity For", input$chosen_race, "Households"),
                  xaxis = list(title = "Year"),
                  yaxis = list (title = "Food Insecurity %")))
})