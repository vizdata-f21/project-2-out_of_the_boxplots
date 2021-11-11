###Test

#Packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(janitor)
library(tidyverse)
library(lubridate)
library(scales)

## DATA ##
semester <- read_csv(here::here("data", "semester.csv"))
usage_chart <- read_csv(here::here("data", "usage_chart.csv"))
food_points <- read_csv(here::here("data", "matt_food_point.csv")) %>%
  clean_names()

## DATA WRANGLING ##
food_points <- food_points %>%
  # separate date time information into two diff recoded variables
  separate(date_time, c("date", "time"), " ") %>%
  # still nee to convert the time variable in lubridate
  mutate(date = mdy(date)) %>%
  # filter out deposit of food points
  filter(!str_detect(location, "DukeCard Offices")) %>%
  # create variable restaurant based on where food points were spent
  # (need mcdonalds, freeman center still)
  mutate(restaurant = case_when(
    str_detect(location, "Bella Union") ~ "Bella Union",
    str_detect(location, "Beyu Blue") ~ "Beyu Blue",
    str_detect(location, "The Cafe") ~ "Cafe",
    str_detect(location, "Farmstead") ~ "Farmstead",
    str_detect(location, "Gussys|Poblanos") ~ "Food Truck at 300 Swift",
    str_detect(location, "Ginger and Soy") ~ "Ginger and Soy",
    str_detect(location, "Gyotaku") ~ "Gyotaku",
    str_detect(location, "Il Forno") ~ "Il Forno",
    str_detect(location, "JBS") ~ "JBs Roast and Chops",
    str_detect(location, "Marketplace") ~ "Marketplace",
    str_detect(location, "Nasher Cafe") ~ "Nasher Cafe",
    str_detect(location, "Panda Express") ~ "Panda Express",
    str_detect(location, "Panera") ~ "Panera",
    str_detect(location, "Pitchfork") ~ "Pitchfork's",
    str_detect(location, "Red Mango") ~ "Red Mango",
    str_detect(location, "Sazon") ~ "Sazon",
    str_detect(location, "Sprout") ~ "Sprout",
    str_detect(location, "Tandor") ~ "Tandoor",
    str_detect(location, "Devils Krafthouse") ~ "The Devil's Krafthouse",
    str_detect(location, "Lobby Shop") ~ "The Lobby Shop",
    str_detect(location, "Loop") ~ "The Loop",
    str_detect(location, "Skillet") ~ "The Skillet",
    str_detect(location, "300 Swift") ~ "Thrive Kitchen",
    str_detect(location, "Trinity Cafe") ~ "Trinity Cafe",
    str_detect(location, "Twinnies") ~ "Twinnies",
    str_detect(location, "Vending") ~ "Vending Machine",
    str_detect(location, "Perk") ~ "Vondy",
    TRUE ~ "Other"),
    # mutate cost variable to make it numeric
    cost = as.numeric(str_extract_all(amount, "[0-9]*\\.[0-9]*")))

food_points <- food_points %>%
  # make any old restaurants such as ABP or or non major eateries NA
  mutate(restaurant = ifelse(restaurant == "Other", NA, restaurant))

# calculate total points spent at each dining location
food_points_location <- food_points %>%
  group_by(restaurant) %>%
  summarise(total_spent = sum(cost))

## SAMPLE PLOTS ##

ggplot(data = food_points, aes(x = date, y = cost)) +
  geom_line() +
  geom_smooth(se = FALSE,
              linetype = "dashed",
              color = "red",
              span = .25) +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "Cost of Meal",
    title = "Money Spent on Meals Over Time"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    text = element_text(family = "Times New Roman")
  )

ggplot(data = food_points_location,
       aes(y = fct_reorder(restaurant, total_spent), x = total_spent)) +
  geom_col() +
  scale_x_continuous(labels = dollar_format()) +
  theme_minimal() +
  labs(
    y = "Dining Location",
    x = "Total Amount Spent",
    title = "Money Spent per Dining Location"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    text = element_text(family = "Times New Roman")
  )

## UI ##

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "Food Point Tracker"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Overview", tabName = "overview", icon = icon("utensils")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "overview",
        h2("Overview"),
        fluidRow(
          box(selectInput("plan_select", "Select a Food Plan:", choices = c("Plan A", "Plan B", "Plan C", "Plan D", "Plan F", "Plan I", "Plan J"))),
          box(tableOutput("table_plans"))
        )
      )
    )
  )
)


## SERVER ##

server <- function(input, output) {
  output$table_plans <- renderTable({
    table1 <- semester %>%
      filter(plan == str_extract(input$plan_select, "[:alpha:]$"))
    table1
  })
}

## RUN APP ##

shinyApp(ui, server)