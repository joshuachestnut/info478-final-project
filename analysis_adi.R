library(dplyr)
library(ggplot2)
library(foreign)
library(survey)
library(Hmisc)
library(plotly)

state_data <- read.csv("./data/feed_america_data/prepped/state_data.csv")

pce_data <- read.csv("./data/pce_data/pce1018.csv") %>%
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
  ggtitle("Money Spent on Food Per Capita vs Food insecurity Rate For 51 States") +
  xlab("Money Spent on Food Per Capita ") +
  ylab("Food Insecurity Rate (%)")


final_plot <- ggplotly(plot, tooltip=c("x", "y", "text"))








# p <- plot_ly(data = joined_data, x=~Off.premises.food.and.beverages, y=~X2017.Food.Insecurity.Rate, 
#         type="scatter", mode="markers", text=paste(joined_data$State.Name, "<br> Money Spent on Food Per Capita: ",
#                                          joined_data$Off.premises.food.and.beverages,"<br> Total Money SpentPer Capita",
#                                          joined_data$Total.Personal.Consumption.Expenditures, "<br> Food Insecurity Rate: ",
#                                          joined_data$X2017.Food.Insecurity.Rate), 
#         hoverinfo="text", marker=list(color="#DF6747", size=~Total.Personal.Consumption.Expenditures / 1500))
# 
# 
# highlight(p, on="plotly_click", color="#0000FF")
