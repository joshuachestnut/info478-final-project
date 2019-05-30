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

setwd("~/Desktop/info478-final-project")

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


setwd("~/Desktop/info478-final-project/data/feed_america_data/cb_2018_us_county_500k")

us.map <- readOGR(dsn = ".", layer = "cb_2018_us_county_500k", stringsAsFactors = FALSE)

setwd("~/Desktop/info478-final-project/data/feed_america_data/cb_2018_us_state_500k")
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
      bringToFront = TRUE
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
      bringToFront = TRUE
      ),
    popup = popup_dat,
    popupOptions = popupOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    ),
    group = "County Level"
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
    options = layersControlOptions(collapsed = FALSE)
  )

final_map


# 3. Trends of Food Insecurity based on different demographics
tabPanel(
  "Food Insecurity Trends",
  titlePanel("Trends of Food Insecurity Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "chosen_race",
        label = "Select The Ethnicity of Interest",
        choices = list(
          "White",
          "Black",
          "Hispanic",
          "Other"
        )
      ),
      checkboxGroupInput("home_types",
                         "Please select home types:",
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
