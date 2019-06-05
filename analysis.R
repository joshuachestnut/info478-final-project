library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(dplyr)
library(leaflet)
library(scales)
library(tidyr)
library(ggplot2)
library(plotly)
library(shiny)
library(highcharter)
library(treemap)

# 1. Allocation of money spent on food, how that relates to food insecurity?
state_data <- read.csv("data/feed_america_data/prepped/state_data.csv")

pce_data <- read.csv("data/pce_data/pce1018.csv") %>%
  select(State, Total.Personal.Consumption.Expenditures, 
         Housing.and.utilities,
         Off.premises.food.and.beverages) %>% head(51)

joined_data <- merge(state_data, pce_data, by.x = "State.Name", by.y="State", all=T)
joined_data$Off.premises.food.and.beverages <- as.numeric(sub(",", "", joined_data$Off.premises.food.and.beverages))
joined_data$Total.Personal.Consumption.Expenditures <- as.numeric(sub(",", "", joined_data$Total.Personal.Consumption.Expenditures))
joined_data$X2017.Food.Insecurity.Rate <- as.numeric(sub("%", "", joined_data$X2017.Food.Insecurity.Rate))

colnames(joined_data)[4] <- 'Food_Insecurity_Rate_2017'
colnames(joined_data)[21] <- "Money_Spent_on_Food_Per_Capita"

plot <- ggplot(joined_data, aes(y=Food_Insecurity_Rate_2017, x=Money_Spent_on_Food_Per_Capita)) +
  geom_point(aes(size=Total.Personal.Consumption.Expenditures,
                 colour="#DF6747",
                 text=paste(joined_data$State.Name, "<br> Total Money Spent Per Capita: ",
                            joined_data$Total.Personal.Consumption.Expenditures)), alpha=.7) +
  scale_radius(range= c(3, 15)) +
  geom_smooth(method="loess", se=FALSE) +
  guides(colour=FALSE) +
  theme(legend.position = "none") +
  ggtitle("Money Spent on Food Per Capita vs Food Insecurity Rate For 51 States") +
  xlab("Money Spent on Food Per Capita ") +
  ylab("Food Insecurity Rate (%)")


final_plot <- ggplotly(plot, tooltip=c("x", "y", "text"))

# 2. Map of Adult vs. Child Food Insecurity

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

us.map <- readOGR(dsn = "data/feed_america_data/cb_2018_us_county_500k", layer = "cb_2018_us_county_500k", stringsAsFactors = FALSE)

us.state.map <- readOGR(dsn = "data/feed_america_data/cb_2018_us_state_500k", layer = "cb_2018_us_state_500k", stringsAsFactors = FALSE)

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

physical_inactive <- read.csv("data/Health_Data/physical_inactivity_prevalence.csv", stringsAsFactors = FALSE)
obesity <- read.csv("data/Health_Data/obesity_prevalence.csv", stringsAsFactors = FALSE)
diabetes <- read.csv("data/Health_Data/diabetes_prevalence.csv", stringsAsFactors = FALSE)
hypertension <- read.csv("data/Health_Data/hypertension_prevalence.csv", stringsAsFactors = FALSE)

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
    overlayGroups = c("Obesity", "Hypertension", "Diabetes"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(c("Obesity", "Hypertension", "Diabetes"))

# 3. Correlation Health Risk Plot

health_risk <- read.csv("data/health_risk/health_risk.csv", stringsAsFactors = FALSE)

health_risk <- health_risk %>%
  mutate(
    fi_rate = fi_rate*100
  )

colnames(health_risk)[colnames(health_risk) == "fi_rate"] <- "Food_Insecurity_Percentage"
colnames(health_risk)[colnames(health_risk) == "diabetes_percent"] <- "Diabetes"
colnames(health_risk)[colnames(health_risk) == "hypertension_percent"] <- "Hypertension"
colnames(health_risk)[colnames(health_risk) == "obesity_percent"] <- "Obesity"

my_ui <- fluidPage(

  # Give the UI a Title
  titlePanel("Correlation Between Food Insecurity and Health Risks"),

  # Add a sidebarLayout
  sidebarLayout(

    # Add a sidebarPanel within the sidebarLayout
    sidebarPanel(
      # This is a radioButtons input for the feature choices.
      radioButtons("Feature", "Select a Health Risk:", choices = c("Obesity", "Hypertension", "Diabetes"))
    ),
    # Add the mainPanel to the fluid page.
    mainPanel(
      plotlyOutput("correlationPlot")
    )
  ))


my_server <- shinyServer(function(input, output) {
  # This is the correlation plot that corresponds the the given selections and filters of the data table.
  output$correlationPlot <- renderPlotly({
    selected_data <- health_risk %>%
      select(state, Food_Insecurity_Percentage, input$Feature)
    health_risk_plot <- ggplot(data = health_risk) +
      geom_point(mapping = aes_string(y = input$Feature, x = "Food_Insecurity_Percentage", text = "state"), color = "red") +
      geom_smooth(mapping = aes_string(y = input$Feature, x = "Food_Insecurity_Percentage"), method = "lm") +
      labs(
        title = paste("Correlation between Food Insecurity and", input$Feature),
        x = "Food Insecurity Percentage (%)",
        y = paste(input$Feature, "Percent (%)")
      )
    # In order to produce the plot, need to call it at the end of the function.
    health_risk_plot
  })
})

# 4. Trends of Food Insecurity based on different demogrpahics

my_ui_two <- fluidPage(
  titlePanel("Trends of Food Insecurity Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("chosen_race", label = "Select The Ethnicity of Interest",
                  choices = list("White" = "White non-Hispanic",
                                 "Black" = "Black non-Hispanic",
                                 "Hispanic",
                                 "Other")),
      selectInput("home_types", label = "Please select household composition type:",
                  choices = list(
                    "All Households Combined" = "All Types", "With Children < 18" = "With children < 18 yrs",
                    "With Children < 18 & With Children < 6 yrs" = "With children < 6 yrs", 
                    "With Children < 18 & Married-couple Families" = "Married-couple families",
                    "With Children < 18 & Female Head, No Spouse" = "Female head, no spouse",
                    "With Children < 18 & Male Head, No Spouse" = "Male head, no spouse",
                    "With Children < 18 & Other Household With Child" = "Other household with child", 
                    "With No Children < 18 yrs" = "With no children < 18 yrs",
                    "With No Children < 18 yrs & More Than One Adult" = "More than one adult", 
                    "With No Children < 18 yrs & Women Living Alone" = "Women living alone",
                    "With No Children < 18 yrs & Men Living Alone" = "Men living alone",
                    "With Elderly" = "With elderly",
                    "With Elderly & Elderly living alone" = "Elderly living alone"
                  )
      ),
      p("This tool is designed to show the trends of food insecurity in the US
        from 2001 to 2017 for specific groups. Through these visualizations,
        viewers will be able to see the differences between groups and
        hopefully identify which groups need the most assistance. The first 
        plot focuses on a variety of ethnicities while the second focuses on the
        different types of households in America. The overall food insecurity
        trend of All Households shows a steady trend upwards from 2001 until 2003
        and went downwards for 3 years. In 2007, there was a huge spike upwards
        in food insecurity and hit its peak in 2012. From then on, there has
        been a steady trend down but has yet to be at the levels seen in 2001.")
    ),
    mainPanel(
      plotlyOutput("race_plot"),
      br(),
      plotOutput("house_plot")
    )
  )
)

trends_data <- read.csv("data/usda_data/prepped/food_security_data.csv",
                        header = T,
                        stringsAsFactors = F)

my_server_two <- shinyServer(function(input, output) {
  output$house_plot <- renderPlot({
    p <- ggplot() + labs(x = "Year", y = "Food Insecurity %", 
                         title = paste0("Food Insecurity % for Households ", input$home_types))
    if(input$home_types == "All Types") {
      overall_data <- trends_data %>% filter(Category == "All households")
      p <- p + geom_line(data = overall_data, aes(x = Year, y = Food.insecure.Percent)) + 
        labs(x = "Year", y = "Food Insecurity %", title = "Food Insecurity % for All Households Combined")
    } 
    if ("With children < 18 yrs" %in% input$home_types) {
      first_data <- trends_data %>% filter(SubCategory == input$home_types)
      first_data[is.na(first_data)] <- 0 
      first_data <- first_data %>% filter(SubSubCategory == 0)
      p <- p + geom_line(data = first_data, aes(x = Year, y = Food.insecure.Percent))
    }
    if ("With children < 6 yrs" %in% input$home_types) {
      a_data <- trends_data %>% filter(SubSubCategory == "With children < 6 yrs")
      p <- p + geom_line(data = a_data, aes(x = Year, y = Food.insecure.Percent)) + 
        labs(title = paste0("Food Insecurity % for Households With children < 18 yrs & ", input$home_types))
    }
    if ("Married-couple families" %in% input$home_types) {
      b_data <- trends_data %>% filter(SubSubCategory == "Married-couple families")
      p <- p + geom_line(data = b_data, aes(x = Year, y = Food.insecure.Percent)) +
        labs(title = paste0("Food Insecurity % for Households With children < 18 yrs & ", input$home_types))
    }
    if ("Female head, no spouse" %in% input$home_types) {
      c_data <- trends_data %>% filter(SubSubCategory == input$home_types)
      p <- p + geom_line(data = c_data, aes(x = Year, y = Food.insecure.Percent)) +
        labs(title = paste0("Food Insecurity % for Households With children < 18 yrs & ", input$home_types))
    }
    if ("Male head, no spouse" %in% input$home_types) {
      d_data <- trends_data %>% filter(SubSubCategory == input$home_types)
      p <- p + geom_line(data = d_data, aes(x = Year, y = Food.insecure.Percent)) +
        labs(title = paste0("Food Insecurity % for Households With children < 18 yrs & ", input$home_types))
    }
    if ("Other household with child" %in% input$home_types) {
      e_data <- trends_data %>% filter(SubSubCategory == input$home_types)
      p <- p + geom_line(data = e_data, aes(x = Year, y = Food.insecure.Percent)) +
        labs(title = paste0("Food Insecurity % for Households With children < 18 yrs & ", input$home_types))
    }
    if ("With no children < 18 yrs" %in% input$home_types) {
      second_data <- trends_data %>% filter(SubCategory == input$home_types)
      second_data[is.na(second_data)] <- 0 
      second_data <- second_data %>% filter(SubSubCategory == 0)
      p <- p + geom_line(data = second_data, aes(x = Year, y = Food.insecure.Percent))
    }
    if ("More than one adult" %in% input$home_types) {
      f_data <- trends_data %>% filter(SubSubCategory == input$home_types)
      p <- p + geom_line(data = f_data, aes(x = Year, y = Food.insecure.Percent)) +
        labs(title = paste0("Food Insecurity % for Households With no children < 18 yrs & ", input$home_types))
    }
    if ("Women living alone" %in% input$home_types) {
      g_data <- trends_data %>% filter(SubSubCategory == input$home_types)
      p <- p + geom_line(data = g_data, aes(x = Year, y = Food.insecure.Percent)) +
        labs(title = paste0("Food Insecurity % for Households With no children < 18 yrs & ", input$home_types))
    }
    if ("Men living alone" %in% input$home_types) {
      h_data <- trends_data %>% filter(SubSubCategory == input$home_types)
      p <- p + geom_line(data = h_data, aes(x = Year, y = Food.insecure.Percent)) +
        labs(title = paste0("Food Insecurity % for Households With no children < 18 yrs & ", input$home_types))
    }
    if ("With elderly" %in% input$home_types) {
      third_data <- trends_data %>% filter(SubCategory == input$home_types)
      third_data[is.na(third_data)] <- 0 
      third_data <- third_data %>% filter(SubSubCategory == 0)
      p <- p + geom_line(data = third_data, aes(x = Year, y = Food.insecure.Percent))
    }
    if ("Elderly living alone" %in% input$home_types) {
      j_data <- trends_data %>% filter(SubSubCategory == input$home_types)
      p <- p + geom_line(data = j_data, aes(x = Year, y = Food.insecure.Percent)) +
        labs(title = paste0("Food Insecurity % for Households With Elderly & ", input$home_types))
    }
    return(p)
  })
  
  output$race_plot <- renderPlotly({
    other_data <- trends_data %>% filter(SubCategory == input$chosen_race) 
    return(plot_ly(other_data,
                   x = ~Year,
                   y = ~Food.insecure.Percent,
                   type = "scatter",
                   mode = 'lines') %>%
             layout(title = paste0("Food Insecurity For ", input$chosen_race, " Households"),
                    xaxis = list(title = "Year"),
                    yaxis = list (title = "Food Insecurity %")))
  })
})

# 5. Bar Shiny

food_data <- read.csv("data/food_programs_data/food_data.csv", stringsAsFactors = F)

food_data <- food_data %>% 
  select(State, County, PCT_SNAP16,PCT_NSLP15,PCT_SBP15, PCT_SFSP15)

food_data <- food_data %>% 
  group_by(State) %>% 
  summarise(
    `SNAP Coverage` = median(PCT_SNAP16),
    `School Lunch Participants` = median(PCT_NSLP15),
    `School Breakfast Participants` = median(PCT_SBP15),
    `Summer Food Service Program Participants` = median(PCT_SFSP15)
  )

us_states <- read.csv("data/food_programs_data/us_states.csv", stringsAsFactors = FALSE)

food_data_states <- left_join(us_states, food_data, by = "State")

food_data_states <- food_data_states %>% 
  select(-State)


food_data_states <- add_row(food_data_states, state = "Nation", `SNAP Coverage` = 13.52769, `School Lunch Participants` = 9.000934, `School Breakfast Participants` = 3.87997, `Summer Food Service Program Participants` = 0.787438)

food_data_states <- food_data_states[c(52,1:51),]

food_data_states <- food_data_states %>% 
  gather(key="program", value = "percent", -state)

