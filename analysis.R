# 1. Allocation of money spent on food, how that relates to food insecurity?

# 2. Map of Adult vs. Child Food Insecurity

library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(dplyr)
library(leaflet)
library(scales)
library(tidyr)

state_fi_data <- read.csv("data/feed_america_data/prepped/state_data_final.csv", stringsAsFactors = FALSE)
county_fi_data <- read.csv("data/feed_america_data/prepped/county_state_data_final.csv", stringsAsFactors = FALSE)

county_fi_data <- county_fi_data %>% 
  separate(county_state, c("county", "state"), ", ")

county_fi_data$county <- tolower(county_fi_data$county)

colnames(county_fi_data)[colnames(county_fi_data) == "FIPS"] <- "GEOID"
colnames(state_fi_data)[colnames(state_fi_data) == "FIPS"] <- "GEOID"

county_fi_data$GEOID <- formatC(county_fi_data$GEOID, width = 5, format = "d", flag = "0")

state_fi_data$GEOID[1] <- "01"
state_fi_data$GEOID[2] <- "02"
state_fi_data$GEOID[3] <- "04"
state_fi_data$GEOID[4] <- "05"
state_fi_data$GEOID[5] <- "06"
state_fi_data$GEOID[6] <- "08"
state_fi_data$GEOID[7] <- "09"


setwd("data/feed_america_data/cb_2018_us_county_500k")

us.map <- readOGR(dsn = ".", layer = "cb_2018_us_county_500k", stringsAsFactors = FALSE)

setwd("../cb_2018_us_state_500k")
us.state.map <- readOGR(dsn = ".", layer = "cb_2018_us_state_500k", stringsAsFactors = FALSE)

us.map <- us.map[!us.map$STATEFP %in% c("72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]
us.state.map <- us.state.map[!us.state.map$STATEFP %in% c("72", "66", "78", "60", "69",
                                                          "64", "68", "70", "74"),]

us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]
us.state.map <- us.state.map[!us.state.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                                          "95", "79"),]

leafmap <- merge(us.map, county_fi_data, by=c("GEOID"))
leafmap_state <- merge(us.state.map, state_fi_data, by = c("GEOID"))

popup_dat <- paste0("<strong>State: </strong>", 
                    leafmap$state, 
                    "<br><strong>County: </strong>", 
                    leafmap$NAME, 
                    "<br><strong>Overall Food Insecurity Percentage: </strong>", 
                    leafmap$fi_rate*100,
                    "<br><strong>Number of Food Insecure People: </strong>", 
                    leafmap$number_fi_persons,
                    "<br><strong>Child Food Insecurity Percentage: </strong>", 
                    leafmap$child_fi_rate*100,
                    "<br><strong>Number of Food Insecure Children: </strong>", 
                    leafmap$child_fi_persons,
                    "<br><strong>Cost Per Meal ($): </strong>", 
                    leafmap$cost_per_meal,
                    "<br><strong>Weighted Annual Food Budget Shortfall ($): </strong>", 
                    leafmap$weighted_annual_food_budget_shortfall)

popup_state <-paste0("<strong>State: </strong>", 
                     leafmap_state$NAME, 
                     "<br><strong>Overall Food Insecurity Percentage: </strong>", 
                     leafmap_state$fi_rate*100,
                     "<br><strong>Number of Food Insecure People: </strong>", 
                     leafmap_state$number_fi_persons,
                     "<br><strong>Child Food Insecurity Percentage: </strong>", 
                     leafmap_state$child_fi_rate*100,
                     "<br><strong>Number of Food Insecure Children: </strong>", 
                     leafmap_state$child_fi_persons,
                     "<br><strong>Cost Per Meal ($): </strong>", 
                     leafmap_state$X,
                     "<br><strong>Weighted Annual Food Budget Shortfall ($): </strong>", 
                     leafmap_state$X.1)

bins <- c(0,0.05,0.1,0.15,0.2,0.25, 0.3, 0.35, 0.4)
state_bins <-c(0, .02, .04, .06, .08, .10, .12, .14, .16, .18, .20)
pal <- colorBin("YlOrRd", domain = leafmap$fi_rate, bins = bins)
pal_state <- colorBin("YlOrRd", domain = leafmap_state$fi_rate, bins = state_bins)

setwd("../../Health_Data")

physical_inactive <- read.csv("physical_inactivity_prevalence.csv", stringsAsFactors = FALSE)
obesity <- read.csv("obesity_prevalence.csv", stringsAsFactors = FALSE)
diabetes <- read.csv("diabetes_prevalence.csv", stringsAsFactors = FALSE)
hypertension <- read.csv("hypertension_prevalence.csv", stringsAsFactors = FALSE)

obesity_popup <- paste0("<strong>State: </strong>", 
                        obesity$state, 
                        "<br><strong>Obesity Percentage (%): </strong>", 
                        obesity$obesity_percent)

physical_inactive_popup <- paste0("<strong>State: </strong>", 
                                  physical_inactive$state, 
                                  "<br><strong>Physical Inactivity Percentage (%): </strong>", 
                                  physical_inactive$physical_inactivity_percentage)

diabetes_popup <- paste0("<strong>State: </strong>", 
                         diabetes$LocationDesc, 
                         "<br><strong>Diabetes Percentage (%): </strong>", 
                         diabetes$diabetes_percent)

hypertension_popup <- paste0("<strong>State: </strong>", 
                             hypertension$LocationDesc, 
                             "<br><strong>Hypertension Percentage (%): </strong>", 
                             hypertension$hypertension_percent)

final_map <- leaflet() %>% 
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(
    data = leafmap_state,
    fillColor = ~pal_state(leafmap_state$fi_rate),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = FALSE
    ),
    popup = popup_state,
    popupOptions = popupOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    ),
    group = "State Level"
  ) %>% 
  addPolygons(
    data = leafmap,
    fillColor = ~pal(leafmap$fi_rate),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = FALSE
    ),
    popup = popup_dat,
    popupOptions = popupOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    ),
    group = "County Level"
  ) %>% 
  addCircles(
    data = physical_inactive,
    lng = ~long, lat = ~lat, 
    weight = 1,
    radius = ~physical_inactive$physical_inactivity_percentage*1000, 
    popup = physical_inactive_popup, 
    color = "black",
    group = "Physical Inactivity",
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>% 
  addCircles(
    data = obesity,
    lng = ~long, lat = ~lat, 
    weight = 1,
    radius = ~obesity$obesity_percent*2000, 
    popup = obesity_popup, 
    color = "blue",
    group = "Obesity",
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>% 
  addCircles(
    data = hypertension,
    lng = ~long, lat = ~lat, 
    weight = 1,
    radius = ~hypertension$hypertension_percent*2000, 
    popup = hypertension_popup, 
    color = "purple",
    group = "Hypertension",
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>% 
  addCircles(
    data = diabetes,
    lng = ~long, lat = ~lat, 
    weight = 1,
    radius = ~diabetes$diabetes_percent*5000, 
    popup = diabetes_popup, 
    color = "green",
    group = "Diabetes",
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE
    )
  ) %>% 
  addLegend(pal = pal,
            values  = leafmap$fi_rate,
            position = "bottomright",
            group = "County Level",
            title = "County Level Food Insecurity Rate") %>% 
  addLegend(pal = pal_state,
            values = leafmap_state$fi_rate,
            position = "bottomleft",
            group = "State Level",
            title = "State Level Food Insecurity Rate") %>% 
  addLayersControl(
    baseGroups = c("State Level", "County Level"),
    overlayGroups = c("Physical Inactivity", "Obesity", "Hypertension", "Diabetes"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Physical Inactivity", "Obesity", "Hypertension", "Diabetes"))

final_map

# 3. Trends of Food Insecurity based on different demogrpahics

library(shiny)

my_ui <- fluidPage(
  titlePanel("Trends of Food Insecurity Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("chosen_race", label = "Select The Ethnicity of Interest", choices = list("White", "Black", "Hispanic", "Other")),
      checkboxGroupInput("home_types", "Please select home types:",
                         c(
                           "All Types" = "All Types", "With Children < 18" = "With Children < 18",
                           "With Children < 18 & With Children < 6" = "With Children < 6", 
                           "With Children < 18 & Married-couple families" = "Married-couple families",
                           "With Children < 18 & Female head, no spouse" = "Female head, no spouse",
                           "With Children < 18 & Male head, no spouse" = "Male head, no spouse",
                           "With Children < 18 & Other household with child" = "Other household with child", 
                           "With no children < 18 yrs" = "With no children < 18 yrs",
                           "With no children < 18 yrs & More than one adult" = "More than one adult", 
                           "With no children < 18 yrs & Women living alone" = "Women living alone",
                           "With no children < 18 yrs & Men living alone" = "Men living alone",
                           "With no children < 18 yrs & With elderly" = "With elderly",
                           "With no children < 18 yrs & Elderly living alone" = "Elderly living alone"
                         ),
                         selected = "All Types"
      ),
      p("This tool is designed to show the trends of food insecurity in the US
        from 2001 to 2017 for specific groups. Through these visualizations,
        viewers will be able to see the differences between groups and
        hopefully identify which groups need the most assistance.")
    ),
    mainPanel(
      plotlyOutput("house_plot"),
      br(),
      plotlyOutput("race_plot")
    )
  )
)

trends_data <- read.csv("data/usda_data/prepped/food_security_data.csv",
                        header = T,
                        stringsAsFactors = F)

my_server <- shinyServer(function(input, output) {

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
}) 




