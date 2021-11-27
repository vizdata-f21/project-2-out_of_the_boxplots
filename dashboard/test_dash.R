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
template <- read_csv(here::here("data", "input_food_points_data.csv"))

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
      ),
      menuItem(
        "Spending Over Time", tabName = "future", icon = icon("chart-line")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "overview",
        h2("Overview"),
        fluidRow(
          box(downloadButton("food_template", "Download Food Point Template"),
              h4(""),
              fileInput("student_data", "Upload Your Food Point Usage"),
              height = 200),
          box(align = "center",
              infoBoxOutput(width = 12, "plan_detected"),
              tableOutput("summary_table"), height = 200)
        ),
        fluidRow(
          column(12, align = "center", offset = 1,
                 box(align = "center", width = 10,
                     selectInput("top_5_input", "Select Which Measure You Want", c("Number of Swipes per Restaurant",
                                                                                   "Total Food Points Spent per Restaurant")),
                     plotOutput("plot_top_5")))
        )
      ),
      tabItem(
        tabName = "future",
        h2("Spending Over Time"),
        fluidRow(
          box(
            selectInput("select_sem", "Select Semester:", choices = c("Fall", "Spring")),
            selectInput("select_plan", "Select a Food Plan:", choices = c("Plan A", "Plan B", "Plan C", "Plan D", "Plan F", "Plan I", "Plan J"))
          ),
          box(
            plotOutput("overtime1"),
            plotOutput("overtime2")
          )
        )
      )
    )
  )
)


## SERVER ##

server <- function(input, output) {

#output food points template
  output$food_template <- downloadHandler(
    filename = function() {
      "food_points_template.csv"
    },
    content = function(file) {
      write.csv(template, file, row.names = FALSE)
    }
  )

#load in student's data upload
  raw <- reactive({
    req(input$student_data, file.exists(input$student_data$datapath))
    read.csv(input$student_data$datapath)
  })

#plan detect
  plan_detect <- reactive({raw() %>%
    clean_names() %>%
    filter(transaction_type == "Credit") %>%
    mutate(points = as.numeric(str_replace(str_extract(amount, "^\\d?,?\\d+.\\d+"), ",", ""))) %>%
    summarise(plan_total = sum(points)) %>%
    pull(plan_total)
  })

  user_plan_value <- reactive({
    user_plan <- ""
    for(i in 1:nrow(semester)){
      if(semester$total_value[i] == plan_detect()){
        user_plan = semester$plan[i]
      }
    }
    user_plan
  })

  output$plan_detected <- renderInfoBox(
    infoBox(
    "You Have Plan",
    value = user_plan_value(),
    icon = icon("utensils"))
  )

#wrangling of student's data upload
  food_points <- reactive({raw() %>%
      # clean names from template
      clean_names() %>%
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
  })

#code for summary table
  summary_table_code <- reactive({
    req(input$student_data)
    tibble("Plan Total" = semester %>%
             filter(plan == user_plan_value()) %>%
             pull(total_value),
           "Points Spent" = sum(food_points()$cost)) %>%
           mutate("Points Remaining" = `Plan Total` - `Points Spent`)
  })

#display summary table
  output$summary_table <- renderTable(
    summary_table_code(),
    align = 'c',
    bordered = TRUE
  )

#calculate total points spent at each dining location
  food_points_location_cost <- reactive({food_points() %>%
    group_by(restaurant) %>%
    summarise(total_spent = sum(cost)) %>%
    head(5)
  })

  food_points_location_freq <- reactive({food_points() %>%
    group_by(restaurant) %>%
    count() %>%
    arrange(desc(n)) %>%
    head(5) %>%
    rename(freq = n)
  })

  plot_top_costs <- reactive({
    ggplot(data = food_points_location_cost(),
           aes(y = fct_reorder(restaurant, total_spent),
               x = total_spent)) +
      geom_col() +
      theme_minimal() +
      scale_x_continuous(labels = dollar_format()) +
      labs(
        y = NULL,
        x = "Total Amount Spent",
        title = "Money Spent per Dining Location"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        text = element_text(family = "Times New Roman")
      )
  })

  plot_top_freq <- reactive({
    ggplot(data = food_points_location_freq(),
           aes(y = fct_reorder(restaurant, freq),
               x = freq)) +
      geom_col() +
      theme_minimal() +
      labs(
        y = NULL,
        x = "Total Number of Swipes",
        title = "Number of Card Swipes per Dining Location"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        text = element_text(family = "Times New Roman")
      )
  })

  plot_top_5 <- reactive({
    switch(input$top_5_input,
           "Number of Swipes per Restaurant" = plot_top_freq(),
           "Total Food Points Spent per Restaurant" = plot_top_costs()
    )
  })

  output$plot_top_5 <- renderPlot(
    plot_top_5()
  )

#time series plots
  time_df <- reactive({
    sem_choice <- str_to_lower(input$select_sem)
    assign("x", sem_choice)

    plan_choice_tmp <- str_to_lower(input$select_plan)
    plan_choice <- paste0("plan_", str_extract(plan_choice_tmp, "[:alpha:]$"))
    timedf <- usage_chart %>%
      select(x, plan_choice) %>%
      rename("date" = x, "plan_points" = plan_choice) %>%
      mutate(date = mdy(date),
             user_points_total = 0,
             user_points_week = 0)

    week_sum = 0
    total_sum = 0
    for (i in 1:(nrow(timedf)-1)){
      week_sum = 0
      for(x in 1:nrow(food_points())){
        if((food_points()$date[x] >= timedf$date[i]) & (food_points()$date[x] < timedf$date[i+1])){
          week_sum = week_sum + food_points()$cost[x]
          total_sum = total_sum + food_points()$cost[x]
        }
        timedf$user_points_week[i+1] = week_sum
        timedf$user_points_total[i+1] = total_sum
      }
    }
  timedf
  })

  output$overtime1 <- renderPlot({
    time_df() %>%
      mutate(user_points_total = ifelse((user_points_week == 0) & (user_points_total != 0), NA, user_points_total),
             points_remaining = plan_points[1] - user_points_total) %>%
      ggplot(aes(x = date, y = points_remaining)) +
      geom_line(aes(x = date, y = plan_points), color = "blue") +
      geom_point(color = "red") +
      geom_line(color = "red") +
      labs(title = "Plan Progression", x = "Weeks", y = "Points Remaining ($)") +
      stat_smooth(method = "lm", fullrange=TRUE, se = FALSE, color = "lightcoral", linetype="dashed") +
      scale_x_date(breaks = time_df()$date, date_labels = "%b-%d") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5,size=16))
  })

  output$overtime2 <- renderPlot({
    time_df() %>%
      ggplot(aes(x = date, y = user_points_week)) +
      geom_col() +
      geom_hline(aes(yintercept = semester %>% filter(plan == str_extract(input$select_plan, "[:alpha:]$")) %>% pull(weekly_avereage)), linetype = "dashed") +
      geom_label(aes(x = date[2],
                     y = semester %>% filter(plan == str_extract(input$select_plan, "[:alpha:]$")) %>% pull(weekly_avereage),
                     label = paste("Plan", str_extract(input$select_plan, "[:alpha:]$"), "Weekly Average"))) +
      labs(title = "Spending Per Week", x = "Weeks", y = "Points ($)") +
      scale_x_date(breaks = time_df()$date, date_labels = "%b-%d") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5,size=16))
  })

}

## RUN APP ##

shinyApp(ui, server)