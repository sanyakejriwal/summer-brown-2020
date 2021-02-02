rm(list = ls())
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)

shootings <- read.csv("data/shootings-2018.csv")

# Summary Information
total_num_shootings <- nrow(shootings)

total_lives_lost <- sum(shootings$num_killed)

city_most_shootings <- shootings %>%
  group_by(city) %>%
  summarise(num_per_city = n()) %>%
  filter(num_per_city == max(num_per_city)) %>%
  pull(city)
  
total_injuries <- sum(shootings$num_injured)

state_most_shootings <- shootings %>%
  group_by(state) %>%
  summarise(num_per_state = n()) %>%
  filter(num_per_state == max(num_per_state, na.rm = TRUE)) %>%
  pull(state)

# Summary Table
state_summaries <- shootings %>%
  group_by(state) %>%
  summarise(
    total_killed = sum(num_killed, na.rm = TRUE),
    total_injured = sum(num_injured, na.rm = TRUE),
    total_effected = sum(num_killed, num_injured, na.rm = TRUE)
  ) %>%
  arrange(desc(total_effected))

# Description of a Particular Shooting
parkland_shooting <- shootings %>%
  filter(num_killed == max(num_killed, na.rm = TRUE))

ps_num_killed <- parkland_shooting %>% pull(num_killed)
ps_injuries <- parkland_shooting %>% pull(num_injured)
ps_date <- parkland_shooting %>% pull(date)
ps_state <- parkland_shooting %>% pull(state)
ps_city <- parkland_shooting %>% pull(city)
ps_address <- parkland_shooting %>% pull(address)

# Interactive Map
shooting_map <- leaflet(data = shootings) %>%
  addTiles() %>%
  addCircleMarkers(radius = ~ num_killed,
                   popup = paste("Date:", shootings$date,
                                 "<br>Address:", shootings$address,
                                 "<br>Number Killed:", shootings$num_killed,
                                 "<br>Number Injured:", shootings$num_injured))

# Plot
total_harmed <- shootings %>%
  mutate(num_harmed = num_injured + num_killed)

killed_and_injured_bar_plot <- plot_ly(total_harmed,
                                       x = ~state,
                                       y = ~num_injured,
                                       type = "bar",
                                       name = "Number Injured",
                                       title = "is this right") %>%
  add_trace(y = ~num_killed, name = "Number Killed") %>%
  layout(title = "Number of People Killed & Injured in Each State",
         yaxis = list(title = "Number of People"),
         xaxis = list(title = "State"),
         barmode = "group")
