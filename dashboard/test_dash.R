### Test

# Packages
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
library(khroma)
library(patchwork)
library(tools)
library(ggtext)

# SET TO TRUE AT THE END TO GET RID OF ALL POSSIBLE ERRORS
#options(shiny.sanitize.errors = FALSE)

## DATA ##
semester <- read_csv(here::here("data", "semester.csv"))
usage_chart <- read_csv(here::here("data", "usage_chart.csv"))
template <- read_csv(here::here("data", "input_food_points_data.csv"))

## SAMPLE PLOTS ##

# ggplot(data = food_points, aes(x = date, y = cost)) +
#   geom_line() +
#   geom_smooth(se = FALSE,
#               linetype = "dashed",
#               color = "red",
#               span = .100) +
#   scale_y_continuous(labels = dollar_format()) +
#   theme_minimal() +
#   labs(
#     x = "Date",
#     y = "Cost of Meal",
#     title = "Money Spent on Meals Over Time"
#   ) +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold"),
#     text = element_text(family = "Times New Roman")
#   )

## UI ##

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "Food Point Tracker"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Overview",
        tabName = "overview", icon = icon("dashboard")
      ),
      menuItem(
        "Upload Instructions",
        tabName = "upload", icon = icon("copy")
      ),
      menuItem(
        "Spending Over Time",
        tabName = "future", icon = icon("chart-line")
      ),
      menuItem(
        "Food Points Locations",
        tabName = "locations", icon = icon("map-marked")
      ),
      menuItem(
        "Restaurants",
        tabName = "restaurants", icon = icon("utensils")
      ),
      menuItem(
        "Write Up",
        tabName = "writeup", icon = icon("pencil-alt")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "upload",
        h2("How to Access and Upload Your Food Points:"),
        fluidRow(
          img(
            src = "food-points-instructions.png", height = 800, width = 700,
            style = "display: block; margin-left: auto; margin-right: auto;"
          ),
        )
      ),
      tabItem(
        tabName = "overview",
        h2("Food Points Overview"),
        fluidRow(
          box(downloadButton("food_template", "Download Food Point Template"),
              h4(""),
              fileInput("student_data", "Upload Your Food Point Usage\n(see upload instructions tab)"),
              height = 200,
              accept = ".csv"
          ),
          box(
            align = "center",
            infoBoxOutput(width = 12, "plan_detected"),
            tableOutput("summary_table"), height = 200
          )
        ),
        fluidRow(
          column(12,
                 align = "center", offset = 2,
                 box(
                   align = "center", width = 8,
                   DT::dataTableOutput("user_points_table")
                 )
          )
        ),
      ),
      tabItem(
        tabName = "future",
        h2("Spending Over Time"),
        fluidRow(
          box(
            selectInput("select_sem", "Select Semester:",
                        choices = c("Fall", "Spring")
            ),
            selectInput("select_plan", "Select a Food Plan:",
                        choices = c(
                          "Plan A", "Plan B", "Plan C", "Plan D",
                          "Plan F", "Plan I", "Plan J"
                        )
            ),
            checkboxInput(
              "negative_values",
              "Allow progression to display negative food points remaining?",
              value = FALSE
            ),
            height = 220, width = 4
          ),
          box(align = "center", h4("Selected Plan Characteristics\n \n "),
              tableOutput("plan_select_table"),
              height = 220, width = 4
          ),
          box(
            plotOutput("overtime_key"),
            height = 220, width = 4
          )
        ),
        fluidRow(
          box(
            plotOutput("overtime2")
          ),
          box(
            plotOutput("overtime1")
          )
        ),
      ),
      tabItem(
        tabName = "locations",
        h2("Where Food Points Are Spent"),
        fluidPage(
          box(
            plotOutput("top_5_locations")
          )
        )
      ),

      tabItem(
        tabName = "writeup",
        h2("Project Write Up:")
      ),

      tabItem(
        tabName = "restaurants",
        h2("Your Top 5 Restaurants"),
        fluidRow(
          column(12,
                 align = "center",
                 uiOutput("daterange2"),
          )
        ),
        fluidRow(
          column(12,
                 align = "center", offset = 1,
                 box(
                   align = "center", width = 10,
                   selectInput(
                     "top_5_input",
                     "Which Measure Would You Like Visualized?",
                     c(
                       "All Three",
                       "Total Number of Swipes per Restaurant",
                       "Total Food Points Spent per Restaurant",
                       "Average Food Points Spent per Restaurant"
                     )
                     )
                   )
                 )
        ),
          fluidRow(
            column(12,
                   align = "center",
                   wellPanel(plotOutput("plot_top_5")))

        )
      )
    )
  )
)


## SERVER ##

server <- function(input, output) {

  # output food points template
  output$food_template <- downloadHandler(
    filename = function() {
      "food_points_template.csv"
    },
    content = function(file) {
      write.csv(template, file, row.names = FALSE)
    }
  )

  # load in student's data upload
  raw <- reactive({
    req(input$student_data, file.exists(input$student_data$datapath))
    validate(
      need(str_detect(file_ext(input$student_data$name), "csv"),
           "Wrong File Format! Upload the Food Point Usage Template and Try Again."))
    read.csv(input$student_data$datapath)
  })

  # plan detect
  plan_detect <- reactive({
    raw() %>%
      clean_names() %>%
      filter(transaction_type == "Credit") %>%
      mutate(points = as.numeric(str_replace(
        str_extract(amount, "^\\d?,?\\d+.\\d+"), ",", ""
      ))) %>%
      summarise(plan_total = sum(points)) %>%
      pull(plan_total)
  })

  user_plan_value <- reactive({
    user_plan <- ""
    for (i in 1:nrow(semester)) {
      if (semester$total_value[i] == plan_detect()) {
        user_plan <- semester$plan[i]
      }
    }
    user_plan
  })

  output$plan_detected <- renderInfoBox(
    infoBox(
      "You Have Plan",
      value = user_plan_value(),
      icon = icon("utensils")
    )
  )

  # wrangling of student's data upload
  food_points <- reactive({
    raw() %>%
      # clean names from template
      clean_names() %>%
      # separate date time information into two diff recoded variables
      separate(date_time, c("date", "time"), " ") %>%
      mutate(date = mdy(date)) %>%
      # filter out deposit of food points
      filter(!str_detect(location, "DukeCard Offices")) %>%
      # create variable restaurant based on where food points were spent
      mutate(
        restaurant = case_when(
          str_detect(location, "Bella Union") ~ "Bella Union",
          str_detect(location, "Beyu Blue") ~ "Beyu Blue",
          str_detect(location, "The Cafe") ~ "Cafe",
          str_detect(location, "Farmstead") ~ "Farmstead",
          str_detect(location, "Gussys|Poblanos") ~ "Food Truck at 300 Swift",
          str_detect(location, "Ginger and Soy") ~ "Ginger and Soy",
          str_detect(location, "Gyotaku") ~ "Gyotaku",
          str_detect(location, "Il Forno") ~ "Il Forno",
          str_detect(location, "JBS") ~ "JBs Roast and Chops",
          str_detect(location, "McDonalds MCDReg2") ~ "McDonalds",
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
          TRUE ~ "Other"
        ),
        # mutate cost variable to make it numeric
        cost = as.numeric(str_extract_all(amount, "[0-9]*\\.[0-9]*"))
      )
  })

  # create logo image variable for bar plot
  label_logos <- c(
    "Bella Union" = "Bella Union",
    "Beyu Blue" = "<img src='www/beyu_blue.jpeg' width='100' />",
    "Cafe" = "<img src='www/cafe.png' width='80' />",
    "Farmstead" = "<img src='www/farmstead.jpeg' width='100' />",
    "Food Truck at 300 Swift" = "Food Truck at 300 Swift",
    "Ginger and Soy" = "<img src='www/ginger-and-soy.jpeg' width='100' />",
    "Gyotaku" = "<img src='www/gyotaku.jpeg' width='100' />",
    "Il Forno" = "<img src='www/il_forno.png' width='100' />",
    "JBs Roast and Chops" = "<img src='www/jbs.jpeg' width='100' />",
    "The Devil's Krafthouse" = "<img src='www/krafthouse.jpeg' width='100' />",
    "The Lobby Shop" = "The Lobby Shop",
    "McDonalds" = "<img src='www/mcdonalds.png' width='100' />",
    "Nasher Cafe" = "<img src='www/nasher-cafe.jpeg' width='100' />",
    "Panda Express" = "<img src='www/panda_express.png' width='100' />",
    "Panera" = "<img src='www/panera.png' width='100' />",
    "Pitchfork's" = "<img src='www/pitchforks.jpeg' width='80' />",
    "Red Mango" = "<img src='www/red_mango.png' width='100' />",
    "Sazon" = "<img src='www/sazon.png' width='100' />",
    "The Skillet" = "<img src='www/skillet.jpeg' width='100' />t",
    "Sprout" = "<img src='www/sprout.jpeg' width='100' />",
    "Tandoor" = "<img src='www/tandoor.jpeg' width='100' />",
    "The Loop" = "<img src='www/the_loop.jpeg' width='100' />",
    "Thrive Kitchen" = "<img src='www/thrive.png' width='100' />",
    "Trinity Cafe" = "<img src='www/trinity-cafe.jpeg' width='100' />",
    "Twinnies" = "<img src='www/twinnies.jpeg' width='100' />",
    "Vending Machine" = "Vending Machine",
    "Vondy" = "Vondy"
  )

  label_logos_small <- c(
    "Bella Union" = "Bella Union",
    "Beyu Blue" = "<img src='www/beyu_blue.jpeg' width='40' />",
    "Cafe" = "<img src='www/cafe.png' width='30' />",
    "Farmstead" = "<img src='www/farmstead.jpeg' width='40' />",
    "Food Truck at 300 Swift" = "Food Truck at 300 Swift",
    "Ginger and Soy" = "<img src='www/ginger-and-soy.jpeg' width='40' />",
    "Gyotaku" = "<img src='www/gyotaku.jpeg' width='40' />",
    "Il Forno" = "<img src='www/il_forno.png' width='40' />",
    "JBs Roast and Chops" = "<img src='www/jbs.jpeg' width='40' />",
    "The Devil's Krafthouse" = "<img src='www/krafthouse.jpeg' width='40' />",
    "The Lobby Shop" = "The Lobby Shop",
    "McDonalds" = "<img src='www/mcdonalds.png' width='40' />",
    "Nasher Cafe" = "<img src='www/nasher-cafe.jpeg' width='40' />",
    "Panda Express" = "<img src='www/panda_express.png' width='40' />",
    "Panera" = "<img src='www/panera.png' width='30' />",
    "Pitchfork's" = "<img src='www/pitchforks.jpeg' width='30' />",
    "Red Mango" = "<img src='www/red_mango.png' width='40' />",
    "Sazon" = "<img src='www/sazon.png' width='40' />",
    "The Skillet" = "<img src='www/skillet.jpeg' width='40' />t",
    "Sprout" = "<img src='www/sprout.jpeg' width='40' />",
    "Tandoor" = "<img src='www/tandoor.jpeg' width='40' />",
    "The Loop" = "<img src='www/the_loop.jpeg' width='40' />",
    "Thrive Kitchen" = "<img src='www/thrive.png' width='40' />",
    "Trinity Cafe" = "<img src='www/trinity-cafe.jpeg' width='40' />",
    "Twinnies" = "<img src='www/twinnies.jpeg' width='40' />",
    "Vending Machine" = "Vending Machine",
    "Vondy" = "Vondy"
  )

  # set colors for each dining location
  restaurant_colors <- c(
    "Bella Union" = "deeppink4",
    "Beyu Blue" = "#263770",
    "Cafe" = "#000000",
    "Farmstead" = "#ABE874",
    "Food Truck at 300 Swift" = "#56645F",
    "Ginger and Soy" = "#FFF80B",
    "Gyotaku" = "#8b0000",
    "Il Forno" = "#E6011D",
    "JBs Roast and Chops" = "#A6A167",
    "The Devil's Krafthouse" = "#081898",
    "The Lobby Shop" = "#00539B",
    "McDonalds" = "#FEBD07",
    "Nasher Cafe" = "#8EC62E",
    "Panda Express" = "#B3101B",
    "Panera" = "#566714",
    "Pitchfork's" = "#B0B3B4",
    "Red Mango" = "#3D2418",
    "Sazon" = "#D45029",
    "The Skillet" = "#F5C808",
    "Sprout" = "#84FF14",
    "Tandoor" = "#FD8716",
    "The Loop" = "#6A9A5C",
    "Thrive Kitchen" = "#182758",
    "Trinity Cafe" = "#0F635F",
    "Twinnies" = "#4c2c3c",
    "Vending Machine" = "#3f4a34",
    "Vondy" = "#371527"
  )

  #code for date ranges
  output$daterange2 <- renderUI({
    dateRangeInput(
      "daterange", "Please Select Your Desired Date Range:",
      start = as.character(min(food_points()$date)),
      end = as.character(max(food_points()$date)),
      min = as.character(min(food_points()$date)),
      max = as.character(max(food_points()$date))
    )
  })

  # code for summary table
  summary_table_code <- reactive({
    req(input$student_data)
    tibble(
      "plan_total" = semester %>%
        filter(plan == user_plan_value()) %>%
        pull(total_value),
      "points_spent" = sum(food_points()$cost)
    ) %>%
      mutate(points_remain = plan_total - points_spent,
             "Points Remaining" = paste0("$", points_remain),
             "Plan Total" = paste0("$", plan_total),
             "Points Spent" = paste0("$", points_spent)) %>%
      select(`Plan Total`, `Points Spent`, `Points Remaining`)
  })

  # display summary table
  output$summary_table <- renderTable(
    summary_table_code(),
    align = "c",
    bordered = TRUE
  )

  # display user's data upload
  output$user_points_table <- DT::renderDataTable({
    tmp <- food_points() %>%
      arrange(date) %>%
      mutate(points_remaining = plan_detect())

    for (i in 1:nrow(tmp)) {
      if (i == 1) {
        tmp$points_remaining[1] <- tmp$points_remaining[1] - tmp$cost[1]
      } else {
        tmp$points_remaining[i] <- tmp$points_remaining[i - 1] - tmp$cost[i]
      }
    }
    tmp %>%
      select(date, restaurant, cost, points_remaining) %>%
      arrange(points_remaining) %>%
      mutate(
        cost = paste0("$", format(cost, digits = 3)),
        points_remaining = paste0(
          "$",
          format(points_remaining,
                 digits = 5
          )
        )
      ) %>%
      rename(
        "Date" = "date",
        "Restaurant" = "restaurant",
        "Cost" = "cost",
        "Points Remaining" = "points_remaining"
      )
  })
# BAR PLOTS
  # calculate total points spent at each dining location
  food_points_location_cost <- reactive({
    food_points() %>%
      filter(date >= input$daterange[1] & date <= input$daterange[2]) %>%
      group_by(restaurant) %>%
      summarise(total_spent = sum(cost)) %>%
      arrange(desc(total_spent)) %>%
      head(5)
  })

  food_points_location_freq <- reactive({
    food_points() %>%
      filter(date >= input$daterange[1] & date <= input$daterange[2]) %>%
      group_by(restaurant) %>%
      count() %>%
      arrange(desc(n)) %>%
      head(5) %>%
      rename(freq = n)
  })

  food_points_location_avg <- reactive({
    food_points() %>%
      filter(date >= input$daterange[1] & date <= input$daterange[2]) %>%
      group_by(restaurant) %>%
      summarise_at(vars(cost), list(name = mean)) %>%
      head(5) %>%
      rename(avg = name)
  })

  plot_top_costs <- reactive({
    ggplot(
      data = food_points_location_cost(),
      aes(
        y = fct_reorder(restaurant, total_spent),
        x = total_spent,
        fill = restaurant,
        label = paste0("$", round(total_spent, 0))
      )
    ) +
      geom_col(show.legend = FALSE) +
      geom_text(hjust = -.15, size = 3.5) +
      theme_minimal() +
      scale_x_continuous(labels = dollar_format()) +
      scale_y_discrete(name = NULL, labels = label_logos) +
      #scale_fill_okabeito(reverse = TRUE) +
      scale_fill_manual(values = restaurant_colors) +
      labs(
        y = NULL,
        x = "\nTotal Food Points Spent\n",
        title = "\nTotal Food Points Spent\nper Dining Location"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_markdown(),
        text = element_text(family = "Times New Roman")
      )
  })

  plot_top_freq <- reactive({
    ggplot(
      data = food_points_location_freq(),
      aes(
        y = fct_reorder(restaurant, freq),
        x = freq,
        fill = restaurant,
        label = freq)
    ) +
      geom_col(show.legend = FALSE) +
      geom_text(hjust = -.25, size = 3.5) +
      theme_minimal() +
      labs(
        y = NULL,
        x = "\nTotal Number of Swipes\n",
        title = "\nTotal Number of Card Swipes\nper Dining Location"
      ) +
      scale_y_discrete(name = NULL, labels = label_logos) +
      #scale_fill_okabeito(reverse = TRUE) +
      scale_fill_manual(values = restaurant_colors) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_markdown(),
        text = element_text(family = "Times New Roman")
      )
  })

  plot_top_avg <- reactive({
    ggplot(
      data = food_points_location_avg(),
      aes(
        y = fct_reorder(restaurant, avg),
        x = avg,
        fill = restaurant,
        label = paste0("$", round(avg, 0))
      )
    ) +
      geom_col(show.legend = FALSE) +
      geom_text(hjust = -.15, size = 3.5) +
      theme_minimal() +
      scale_x_continuous(labels = dollar_format()) +
      scale_y_discrete(name = NULL, labels = label_logos) +
      #scale_fill_okabeito(reverse = TRUE) +
      scale_fill_manual(values = restaurant_colors) +
      labs(
        y = NULL,
        x = "\nAverage Food Points Spent per Transaction\n",
        title = "\nAverage Food Points Spent\nper Transaction at Dining Location"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_markdown(),
        text = element_text(family = "Times New Roman")
      )
  })

  all_three <- reactive(
    (
      {
        ggplot(
          data = food_points_location_freq(),
          aes(
            y = fct_reorder(restaurant, freq),
            x = freq,
            fill = restaurant)
        ) +
          geom_col(show.legend = FALSE) +
          theme_minimal() +
          labs(
            y = NULL,
            x = "\nTotal Number of Swipes",
            title = "Total Number of Card Swipes\nper Dining Location"
          ) +
          scale_y_discrete(name = NULL, labels = label_logos_small) +
          #scale_fill_okabeito(reverse = TRUE) +
          scale_fill_manual(values = restaurant_colors) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.y = element_markdown(),
            text = element_text(family = "Times New Roman")
          )
      }
      +
      {
        ggplot(
          data = food_points_location_cost(),
          aes(
            y = fct_reorder(restaurant, total_spent),
            x = total_spent,
            fill = restaurant
          )
        ) +
          geom_col(show.legend = FALSE) +
          theme_minimal() +
          scale_x_continuous(labels = dollar_format()) +
          scale_y_discrete(name = NULL, labels = label_logos_small) +
          #scale_fill_okabeito(reverse = TRUE) +
          scale_fill_manual(values = restaurant_colors) +
          labs(
            y = NULL,
            x = "\nTotal Food Points Spent",
            title = "Total Food Points Spent\nper Dining Location"
          ) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.y = element_markdown(),
            text = element_text(family = "Times New Roman")
          )
      }
    ) /
      {
        ggplot(
          data = food_points_location_avg(),
          aes(
            y = fct_reorder(restaurant, avg),
            x = avg,
            fill = restaurant
          )
        ) +
          geom_col(show.legend = FALSE) +
          theme_minimal() +
          scale_x_continuous(labels = dollar_format()) +
          scale_y_discrete(name = NULL, labels = label_logos_small) +
          #scale_fill_okabeito(reverse = TRUE) +
          scale_fill_manual(values = restaurant_colors) +
          labs(
            y = NULL,
            x = "\nAverage Food Points Spent per Transaction",
            title = "Average Food Points Spent\nper Transaction at Dining Location"
          ) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.y = element_markdown(),
            text = element_text(family = "Times New Roman")
          )
      }
  )

  plot_top_5 <- reactive({
    switch(input$top_5_input,
           "Total Number of Swipes per Restaurant" = plot_top_freq(),
           "Total Food Points Spent per Restaurant" = plot_top_costs(),
           "Average Food Points Spent per Restaurant" = plot_top_avg(),
           "All Three" = all_three()
    )
  })

  output$plot_top_5 <- renderPlot({
    req(food_points())
    req(input$daterange)
    plot_top_5()
  })

  # time series plots
  time_df <- reactive({
    sem_choice <- str_to_lower(input$select_sem)
    assign("x", sem_choice)

    plan_choice_tmp <- str_to_lower(input$select_plan)
    plan_choice <- paste0("plan_", str_extract(plan_choice_tmp, "[:alpha:]$"))
    timedf <- usage_chart %>%
      select(x, plan_choice) %>%
      rename("date" = x, "plan_points" = plan_choice) %>%
      mutate(
        date = mdy(date),
        user_points_total = 0,
        user_points_week = 0
      )

    week_sum <- 0
    total_sum <- 0
    for (i in 1:(nrow(timedf) - 1)) {
      week_sum <- 0
      for (x in 1:nrow(food_points())) {
        if ((food_points()$date[x] >= timedf$date[i]) &
            (food_points()$date[x] < timedf$date[i + 1])) {
          week_sum <- week_sum + food_points()$cost[x]
          total_sum <- total_sum + food_points()$cost[x]
        }
        timedf$user_points_week[i + 1] <- week_sum
        timedf$user_points_total[i + 1] <- total_sum
      }
    }
    timedf
  })

  #location map
  #add ability to increase number of top locations from 1-10
  #add checkboxes for switch based on swipes/total amount spent
  #add time scale for through the years or overall
  #output$top_5_locations <- renderPlot(
  #  {
  #    food_points_location_freq %>%
  #      ggplot(aes(x = ))
  #  }
  #)

  output$overtime1 <- renderPlot({
    if (input$negative_values == TRUE) {
      time1 <- time_df() %>%
        mutate(
          user_points_total = ifelse((user_points_week == 0) &
                                       (user_points_total != 0),
                                     NA, user_points_total
          ),
          points_remaining = plan_points[1] - user_points_total
        )

      ggplot(time1, aes(x = date, y = points_remaining)) +
        geom_line(aes(x = date, y = plan_points), color = "blue") +
        geom_point(color = "red") +
        geom_line(color = "red") +
        labs(title = "Plan Progression", x = "Weeks", y = "Points Remaining") +
        geom_smooth(
          method = "lm", fullrange = TRUE, se = FALSE,
          color = "lightcoral", linetype = "dashed"
        ) +
        scale_x_date(breaks = time_df()$date, date_labels = "%b-%d",
                     minor_breaks = NULL) +
        scale_y_continuous(labels = scales::dollar_format()) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16))
    } else {
      time2 <- time_df() %>%
        mutate(
          user_points_total = ifelse((user_points_week == 0) &
                                       (user_points_total != 0),
                                     NA, user_points_total
          ),
          points_remaining = plan_points[1] - user_points_total
        )

      ggplot(time2, aes(x = date, y = points_remaining)) +
        geom_line(aes(x = date, y = plan_points), color = "blue") +
        geom_point(color = "red") +
        geom_line(color = "red") +
        labs(title = "Plan Progression", x = "Weeks", y = "Points Remaining") +
        geom_smooth(
          method = "lm", fullrange = TRUE, se = FALSE,
          color = "lightcoral", linetype = "dashed"
        ) +
        scale_x_date(breaks = time_df()$date, date_labels = "%b-%d",
                     minor_breaks = NULL) +
        scale_y_continuous(limits = c(0, NA), labels = dollar_format()) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16))
    }
  })

  output$overtime2 <- renderPlot({
    time_df() %>%
      ggplot(aes(x = date, y = user_points_week)) +
      geom_col() +
      geom_hline(aes(yintercept = semester %>%
                       filter(plan == str_extract(
                         input$select_plan,
                         "[:alpha:]$"
                       )) %>%
                       pull(weekly_avereage)), linetype = "dashed") +
      geom_label(aes(
        x = date[3],
        y = semester %>%
          filter(plan == str_extract(
            input$select_plan,
            "[:alpha:]$"
          )) %>%
          pull(weekly_avereage),
        label = paste(
          "Plan",
          str_extract(input$select_plan, "[:alpha:]$"),
          "Weekly Average:",
          paste0("$",
                 semester %>%
                   filter(plan == str_extract(
                     input$select_plan,
                     "[:alpha:]$"
                   )) %>%
                   pull(weekly_avereage))
        )
      )) +
      labs(title = "Spending Per Week", x = "Weeks", y = "Points Spent") +
      scale_x_date(breaks = time_df()$date, date_labels = "%b-%d",
                   minor_breaks = NULL) +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16))
  })

  key <- tibble("plan_prog_x" = c(1,2,3,4,5,6,7,8),
                "plan_prog_y" = c(1,1,1,1,1,1,1,1),
                "user_x" = c(1,2,3,4,5,6,7,8),
                "user_y" = c(.5,.5,.5,.5,.5,.5,.5,.5),
                "reg_x" = c(1,2,3,4,5,6,7,8),
                "reg_y" = c(0,0,0,0,0,0,0,0))

  plot_key <- ggplot(key) +
    geom_line(aes(x = plan_prog_x, y = plan_prog_y), color = "blue") +
    geom_line(aes(x = user_x, y = user_y), color = "red") +
    geom_point(aes(x = user_x, y = user_y), color = "red") +
    geom_line(aes(x = reg_x, y = reg_y), color = "red", linetype = "dashed") +
    geom_text(aes(x = 4.5, y = 1.15),
              label = "Plan Progression (spending from chosen food point plan)",
              color = "blue") +
    geom_text(aes(x = 4.54, y = .65),
              label = "Actual Progression (spending from to uploaded data)",
              color = "red") +
    geom_text(aes(x = 4.55, y = .15),
              label = "Expected Progression (spending from a linear regression of uploaded data)",
              color = "red") +
    ylim(-0.5,1.5) +
    labs(title = "Plan Progression Key") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 16))

  output$overtime_key <- renderPlot(plot_key, height = 200)

  plan_select_table_code <- reactive({
    timedf2 <- time_df() %>%
      mutate(
        user_points_total = ifelse((user_points_week == 0) &
                                     (user_points_total != 0),
                                   NA, user_points_total
        ),
        points_remaining = plan_points[1] - user_points_total
      )

    tibble("Plan Total" = c(paste0("$", max(timedf2$plan_points))),
           "Points Spent" = c(paste0("$", round(max(timedf2$user_points_total, na.rm = TRUE), 2))),
           "Points Remaining" = c(paste0("$", round(min(timedf2$points_remaining, na.rm = TRUE), 2))))
  })

  output$plan_select_table <- renderTable(
    plan_select_table_code(),
    align = "c",
    bordered = TRUE
  )

}

## RUN APP ##

shinyApp(ui, server)