# Load Packages ----------------------------------------------------------------

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
library(patchwork)
library(tools)
library(ggtext)
library(png)
library(ggpubr)
library(leaflet)

# Errors Sanitized  ------------------------------------------------------------

options(shiny.sanitize.errors = FALSE)

# Load Data --------------------------------------------------------------------

semester <- read_csv(here::here("dashboard/data", "semester.csv"))
usage_chart <- read_csv(here::here("dashboard/data", "usage_chart.csv"))
template <- read_csv(here::here("dashboard/data", "input_food_points_data.csv"))
campus_map <- readPNG(here::here("dashboard/www", "duke_campus_map.png"))

# UI ---------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$a(
      href = "https://mralph15.shinyapps.io/dashboard/",
      tags$img(src = "food_point_logo.png", height = "40", width = "180")
    )
  ),
  ## Create Sidebar Tabs
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
        "Dining Locations",
        tabName = "locations", icon = icon("map-marked")
      ),
      menuItem(
        "Top 5 Restaurants",
        tabName = "restaurants", icon = icon("utensils")
      ),
      menuItem(
        "Food Point Tips",
        tabName = "spendingtips", icon = icon("money-check-alt")
      ),
      menuItem(
        "Write Up",
        tabName = "writeup", icon = icon("pencil-alt")
      )
    )
  ),
  dashboardBody(
    tabItems(
      ## Upload Instructions Tab
      tabItem(
        tabName = "upload",
        h2("How to Access and Upload Your Food Points"),
        fluidRow(
          img(
            src = "food-points-instructions.png", height = 800, width = 700,
            style = "display: block; margin-left: auto; margin-right: auto;"
          ),
        )
      ),
      ## Food Points Overview Tab
      tabItem(
        tabName = "overview",
        h2("Food Points Overview"),
        p("First upload your data by going to the upload instructions tab!"),
        fluidRow(
          box(downloadButton("food_template", "Download Food Point Template"),
            h4(""),
            fixedRow(
              fileInput(
                "student_data",
                "Upload Your Food Point Usage:"
              )
            ),
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
            align = "center", offset = 1,
            box(
              align = "center", width = 9,
              DT::dataTableOutput("user_points_table")
            )
          )
        ),
      ),
      ## Spending Over Time Tab
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
          box(
            align = "center", h4(strong("Selected Plan Characteristics\n \n ")),
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
      ## Dining Location Tabs
      tabItem(
        tabName = "locations",
        h2("Where Food Points Are Spent"),
        fluidRow(
          column(12,
            align = "center", offset = 3,
            box(
              align = "center", width = 6,
              uiOutput("location_date_range"),
            )
          )
        ),
        fluidRow(
          column(12,
            align = "center", offset = 1,
            box(
              align = "center", width = 10, height = 500,
              h3(strong("Campus Dining Locations")),
              leafletOutput("leafmap")
            )
          )
        )
      ),
      ## Project Write-Up Tab
      tabItem(
        tabName = "writeup",
        h2("Project Write Up:")
      ),
      ## Food Point Tips Tab
      tabItem(
        tabName = "spendingtips",
        h2("Food Point Spending Tips"),
        fluidRow(column(12,
          align = "center", offset = 3,
          box(
            align = "center", width = 6,
            selectInput("tips_options",
              "How Are You Doing With Your Food Points?",
              choices = c(
                "I'm Running Low!",
                "I Have Too Many Remaining!"
              )
            )
          )
        )),
        uiOutput("tips_needed")
      ),
      ## Top 5 Restaurants Tab
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
                "Which Measure(s) Would You Like Visualized?",
                c(
                  "All Three: Total Swipes, Total Spent, & Avg. Spent",
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
            wellPanel(plotOutput("plot_top_5"))
          )
        ),
        fluidRow(
          column(12,
            align = "center", offset = 2,
            box(
              align = "center", width = 8,
              DT::dataTableOutput("food_points_all_info_table")
            )
          )
        )
      )
    )
  )
)

# Server -----------------------------------------------------------------------

server <- function(input, output) {

  ## Output Food Point Template
  output$food_template <- downloadHandler(
    filename = function() {
      "food_points_template.csv"
    },
    content = function(file) {
      write.csv(template, file, row.names = FALSE)
    }
  )

  ## Load In Student's Data Upload
  raw <- reactive({
    req(input$student_data, file.exists(input$student_data$datapath))
    validate(
      need(
        str_detect(file_ext(input$student_data$name), "csv"),
        "Wrong File Format! Upload the Food Point Usage Template and Try Again."
      )
    )
    read.csv(input$student_data$datapath)
  })

  ## Detect Student's Plan
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

  ## Data Wrangling of Student's Uploaded Data
  food_points <- reactive({
    raw() %>%
      ### Clean Variables on Template
      clean_names() %>%
      ### Separate Date and Time Information
      separate(date_time, c("date", "time"), " ") %>%
      mutate(date = mdy(date)) %>%
      ### Filter Out Deposit Of Food Points
      filter(!str_detect(location, "DukeCard Offices")) %>%
      ### Create Restaurant Variable Based On Location
      mutate(
        restaurant = case_when(
          str_detect(location, "Bella Union") ~ "Bella Union",
          str_detect(location, "Beyu Blue") ~ "Beyu Blue",
          str_detect(location, "The Cafe") ~ "Cafe",
          str_detect(location, "Farmstead") ~ "Farmstead",
          str_detect(location, "Gussys|Poblanos") ~ "Food Trucks at 300 Swift",
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
        ### Create Overall Location Variable Based On Restaurant
        campus_location = case_when(
          restaurant == "Bella Union" ~ "McClendon Tower",
          restaurant == "Beyu Blue" ~ "Bryan Center",
          restaurant == "Cafe" ~ "West Union",
          restaurant == "Farmstead" ~ "West Union",
          restaurant == "Food Trucks at 300 Swift" ~ "300 Swift",
          restaurant == "Ginger and Soy" ~ "West Union",
          restaurant == "Gyotaku" ~ "West Union",
          restaurant == "Il Forno" ~ "West Union",
          restaurant == "JBs Roast and Chops" ~ "West Union",
          restaurant == "McDonalds" ~ "Bryan Center",
          restaurant == "Nasher Cafe" ~ "The Nasher",
          restaurant == "Panda Express" ~ "Bryan Center",
          restaurant == "Panera" ~ "West Union",
          restaurant == "Pitchfork's" ~ "McClendon Tower",
          restaurant == "Red Mango" ~ "Wilson Gym",
          restaurant == "Sazon" ~ "West Union",
          restaurant == "Sprout" ~ "West Union",
          restaurant == "Tandoor" ~ "West Union",
          restaurant == "The Devil's Krafthouse" ~ "West Union",
          restaurant == "The Lobby Shop" ~ "Bryan Center",
          restaurant == "The Loop" ~ "Bryan Center",
          restaurant == "The Skillet" ~ "West Union",
          restaurant == "Thrive Kitchen" ~ "300 Swift",
          restaurant == "Trinity Cafe" ~ "East Campus",
          restaurant == "Twinnies" ~ "E-Quad",
          restaurant == "Vending Machine" ~ "Around Duke's Campus",
          restaurant == "Vondy" ~ "Perkins",
          restaurant == "Other" ~ "Around Duke's Campus",
          TRUE ~ "Around Duke's Campus"
        ),
        ### Mutate Cost Variable To Make Numeric
        cost = as.numeric(str_extract_all(amount, "[0-9]*\\.[0-9]*")),
        ### Hard Code Overall Location Coordinates
        x_coord = case_when(
          campus_location == "West Union" ~ 190,
          campus_location == "Wilson Gym" ~ 130,
          campus_location == "Bryan Center" ~ 120,
          campus_location == "Perkins" ~ 230,
          campus_location == "McClendon Tower" ~ 250,
          campus_location == "300 Swift" ~ 900,
          campus_location == "The Nasher" ~ 700,
          campus_location == "E-Quad" ~ 150,
          campus_location == "East Campus" ~ 1050,
          campus_location == "Around Duke's Campus" ~ 300,
          TRUE ~ dim(campus_map)[2] / 2
        ),
        y_coord = case_when(
          campus_location == "West Union" ~ 300,
          campus_location == "Wilson Gym" ~ 30,
          campus_location == "Bryan Center" ~ 330,
          campus_location == "Perkins" ~ 380,
          campus_location == "McClendon Tower" ~ 170,
          campus_location == "300 Swift" ~ 440,
          campus_location == "The Nasher" ~ 170,
          campus_location == "E-Quad" ~ 500,
          campus_location == "East Campus" ~ 20,
          campus_location == "Around Duke's Campus" ~ 270,
          TRUE ~ dim(campus_map)[1] / 2
        )
      ) %>%
      relocate(campus_location, .after = restaurant)
  })

  ## Create Logo Image Labels
  label_logos <- c(
    "Bella Union" = "<img src='www/bella-union.png' width='110' />",
    "Beyu Blue" = "<img src='www/beyu_blue.jpeg' width='100' />",
    "Cafe" = "<img src='www/cafe.png' width='75' />",
    "Farmstead" = "<img src='www/farmstead.jpeg' width='110' />",
    "Food Trucks at 300 Swift" = "<img src='www/food-trucks-300-swift.png'
    width='110' />",
    "Ginger and Soy" = "<img src='www/ginger-and-soy.jpeg' width='110' />",
    "Gyotaku" = "<img src='www/gyotaku.jpeg' width='100' />",
    "Il Forno" = "<img src='www/il_forno.png' width='100' />",
    "JBs Roast and Chops" = "<img src='www/jbs.jpeg' width='110' />",
    "The Devil's Krafthouse" = "<img src='www/krafthouse.jpeg' width='100' />",
    "The Lobby Shop" = "<img src='www/lobby-shop.png' width='110' />",
    "McDonalds" = "<img src='www/mcdonalds.png' width='100' />",
    "Nasher Cafe" = "<img src='www/nasher-cafe.jpeg' width='100' />",
    "Panda Express" = "<img src='www/panda_express.png' width='100' />",
    "Panera" = "<img src='www/panera.png' width='100' />",
    "Pitchfork's" = "<img src='www/pitchforks.jpeg' width='100' />",
    "Red Mango" = "<img src='www/red_mango.png' width='100' />",
    "Sazon" = "<img src='www/sazon.png' width='100' />",
    "The Skillet" = "<img src='www/skillet.jpeg' width='100' />t",
    "Sprout" = "<img src='www/sprout.jpeg' width='100' />",
    "Tandoor" = "<img src='www/tandoor.jpeg' width='100' />",
    "The Loop" = "<img src='www/the_loop.jpeg' width='100' />",
    "Thrive Kitchen" = "<img src='www/thrive.png' width='100' />",
    "Trinity Cafe" = "<img src='www/trinity-cafe.jpeg' width='100' />",
    "Twinnies" = "<img src='www/twinnies.jpeg' width='100' />",
    "Vending Machine" = "<img src='www/vending.png' width='110' />",
    "Vondy" = "<img src='www/vondy.png' width='100' />",
    "Other" = "<img src='www/other.png' width='110' />"
  )

  ## Create Medium Logo Image Labels
  label_logos_medium <- c(
    "Bella Union" = "<img src='www/bella-union.png' width='90' />",
    "Beyu Blue" = "<img src='www/beyu_blue.jpeg' width='80' />",
    "Cafe" = "<img src='www/cafe.png' width='60' />",
    "Farmstead" = "<img src='www/farmstead.jpeg' width='90' />",
    "Food Trucks at 300 Swift" = "<img src='www/food-trucks-300-swift.png'
    width='90' />",
    "Ginger and Soy" = "<img src='www/ginger-and-soy.jpeg' width='90' />",
    "Gyotaku" = "<img src='www/gyotaku.jpeg' width='70' />",
    "Il Forno" = "<img src='www/il_forno.png' width='75' />",
    "JBs Roast and Chops" = "<img src='www/jbs.jpeg' width='70' />",
    "The Devil's Krafthouse" = "<img src='www/krafthouse.jpeg' width='70' />",
    "The Lobby Shop" = "<img src='www/lobby-shop.png' width='90' />",
    "McDonalds" = "<img src='www/mcdonalds.png' width='70' />",
    "Nasher Cafe" = "<img src='www/nasher-cafe.jpeg' width='70' />",
    "Panda Express" = "<img src='www/panda_express.png' width='70' />",
    "Panera" = "<img src='www/panera.png' width='70' />",
    "Pitchfork's" = "<img src='www/pitchforks.jpeg' width='70' />",
    "Red Mango" = "<img src='www/red_mango.png' width='70' />",
    "Sazon" = "<img src='www/sazon.png' width='70' />",
    "The Skillet" = "<img src='www/skillet.jpeg' width='70' />t",
    "Sprout" = "<img src='www/sprout.jpeg' width='70' />",
    "Tandoor" = "<img src='www/tandoor.jpeg' width='70' />",
    "The Loop" = "<img src='www/the_loop.jpeg' width='70' />",
    "Thrive Kitchen" = "<img src='www/thrive.png' width='70' />",
    "Trinity Cafe" = "<img src='www/trinity-cafe.jpeg' width='70' />",
    "Twinnies" = "<img src='www/twinnies.jpeg' width='70' />",
    "Vending Machine" = "<img src='www/vending.png' width='90' />",
    "Vondy" = "<img src='www/vondy.png' width='80' />",
    "Other" = "<img src='www/other.png' width='90' />"
  )

  ## Create Small Logo Image Labels
  label_logos_small <- c(
    "Bella Union" = "<img src='www/bella-union.png' width='60' />",
    "Beyu Blue" = "<img src='www/beyu_blue.jpeg' width='50' />",
    "Cafe" = "<img src='www/cafe.png' width='40' />",
    "Farmstead" = "<img src='www/farmstead.jpeg' width='50' />",
    "Food Trucks at 300 Swift" = "<img src='www/food-trucks-300-swift.png'
    width='60' />",
    "Ginger and Soy" = "<img src='www/ginger-and-soy.jpeg' width='50' />",
    "Gyotaku" = "<img src='www/gyotaku.jpeg' width='50' />",
    "Il Forno" = "<img src='www/il_forno.png' width='55' />",
    "JBs Roast and Chops" = "<img src='www/jbs.jpeg' width='50' />",
    "The Devil's Krafthouse" = "<img src='www/krafthouse.jpeg' width='50' />",
    "The Lobby Shop" = "<img src='www/lobby-shop.png' width='60' />",
    "McDonalds" = "<img src='www/mcdonalds.png' width='50' />",
    "Nasher Cafe" = "<img src='www/nasher-cafe.jpeg' width='50' />",
    "Panda Express" = "<img src='www/panda_express.png' width='50' />",
    "Panera" = "<img src='www/panera.png' width='50' />",
    "Pitchfork's" = "<img src='www/pitchforks.jpeg' width='50' />",
    "Red Mango" = "<img src='www/red_mango.png' width='50' />",
    "Sazon" = "<img src='www/sazon.png' width='50' />",
    "The Skillet" = "<img src='www/skillet.jpeg' width='50' />t",
    "Sprout" = "<img src='www/sprout.jpeg' width='50' />",
    "Tandoor" = "<img src='www/tandoor.jpeg' width='50' />",
    "The Loop" = "<img src='www/the_loop.jpeg' width='50' />",
    "Thrive Kitchen" = "<img src='www/thrive.png' width='50' />",
    "Trinity Cafe" = "<img src='www/trinity-cafe.jpeg' width='50' />",
    "Twinnies" = "<img src='www/twinnies.jpeg' width='50' />",
    "Vending Machine" = "<img src='www/vending.png' width='50' />",
    "Vondy" = "<img src='www/vondy.png' width='60' />",
    "Other" = "<img src='www/other.png' width='60' />"
  )

  ## Set Colors For Each Restaurant
  restaurant_colors <- c(
    "Bella Union" = "deeppink4",
    "Beyu Blue" = "#263770",
    "Cafe" = "#000000",
    "Farmstead" = "#ABE874",
    "Food Trucks at 300 Swift" = "#56645F",
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
    "Vondy" = "darkslategray1",
    "Other" = "gray"
  )

  ## Create Calender Date Range For Top 5
  output$daterange2 <- renderUI({
    dateRangeInput(
      "daterange", "Select Range of Dates",
      start = as.character(min(food_points()$date)),
      end = as.character(max(food_points()$date)),
      min = as.character(min(food_points()$date)),
      max = as.character(max(food_points()$date))
    )
  })

  ## Dining Location Plot

  ### Create Date Range Slider For Dining Location Tab
  output$location_date_range <- renderUI({
    req(input$student_data)
    req(food_points())
    sliderInput("dates_slider",
      "Select Range of Dates",
      min = min(food_points()$date),
      max = max(food_points()$date),
      value = c(
        min(food_points()$date),
        max(food_points()$date)
      ),
      timeFormat = "%m-%d-%Y",
      step = (max(food_points()$date) - min(food_points()$date)) / 6
    )
  })

  ## Create Summary Table On Spending Over Time Tab
  summary_table_code <- reactive({
    req(input$student_data)
    tibble(
      "plan_total" = semester %>%
        filter(plan == user_plan_value()) %>%
        pull(total_value),
      "points_spent" = sum(food_points()$cost)
    ) %>%
      mutate(
        points_remain = plan_total - points_spent,
        "Points Remaining" = paste0("$", points_remain),
        "Plan Total" = paste0("$", plan_total),
        "Points Spent" = paste0("$", points_spent)
      ) %>%
      select(`Plan Total`, `Points Spent`, `Points Remaining`)
  })

  ## Display Summary Table On Spending Over Time Tab
  output$summary_table <- renderTable(
    summary_table_code(),
    align = "c",
    bordered = TRUE
  )

  ## Display User's Data Upload On Overview Tab
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
    DT::datatable(tmp %>%
      select(date, restaurant, campus_location, cost, points_remaining) %>%
      arrange(points_remaining) %>%
      rename(
        "Date (Y-M-D)" = "date",
        "Restaurant" = "restaurant",
        "Dining Location" = "campus_location",
        "Cost" = "cost",
        "Points Remaining" = "points_remaining"
      )) %>%
      DT::formatCurrency(c(4:5))
  })

  ## Top 5 Bar Plots
  ### Calculate Total Points Spent at Each Restaurant
  food_points_location_cost <- reactive({
    food_points() %>%
      filter(date >= input$daterange[1] & date <= input$daterange[2]) %>%
      group_by(restaurant) %>%
      summarise(total_spent = sum(cost)) %>%
      arrange(desc(total_spent)) %>%
      head(5)
  })

  ### Calculate Total Swipes at at Each Restaurant
  food_points_location_freq <- reactive({
    food_points() %>%
      filter(date >= input$daterange[1] & date <= input$daterange[2]) %>%
      group_by(restaurant) %>%
      count() %>%
      arrange(desc(n)) %>%
      head(5) %>%
      rename(freq = n) %>%
      mutate(freq = as.integer(freq))
  })

  ### Calculate Average Points Spent at Each Restaurant
  food_points_location_avg <- reactive({
    food_points() %>%
      filter(date >= input$daterange[1] & date <= input$daterange[2]) %>%
      group_by(restaurant) %>%
      summarise_at(vars(cost), list(name = mean)) %>%
      head(5) %>%
      rename(avg = name)
  })

  ### Create DF With All Info
  food_points_all_info <- reactive({
    food_points() %>%
      filter(date >= input$daterange[1] & date <= input$daterange[2]) %>%
      group_by(restaurant) %>%
      summarize(
        freq = n(),
        total_cost = sum(cost),
        avg_cost = total_cost / freq
      ) %>%
      arrange(desc(total_cost)) %>%
      rename(
        "Restaurant" = "restaurant",
        "Frequency" = "freq",
        "Total Cost" = "total_cost",
        "Average Cost" = "avg_cost"
      )
  })

  ### Output DF With All Info
  output$food_points_all_info_table <- DT::renderDataTable({
    req(input$daterange)
    DT::datatable(food_points_all_info()) %>%
      DT::formatCurrency(c(3:4))
  })

  ## Create DFs for Dining Location Tab
  ### Create Total Frequency DF for Dining Location Tab
  dining_location_freq <- reactive({
    req(input$dates_slider)
    food_points() %>%
      filter(date >= input$dates_slider[1] & date <= input$dates_slider[2]) %>%
      group_by(campus_location) %>%
      count() %>%
      rename(freq = n)
  })

  ### Create Total Spent DF for Dining Location Tab
  dining_location_spend <- reactive({
    req(input$dates_slider)
    food_points() %>%
      filter(date >= input$dates_slider[1] & date <= input$dates_slider[2]) %>%
      group_by(campus_location) %>%
      summarise("spending" = sum(cost))
  })

  ## Output Dining Location Map
  output$leafmap <- renderLeaflet({
    req(food_points())
    req(dining_location_freq())
    req(dining_location_spend())
    req(input$dates_slider)
    validate(
      need(input$dates_slider, message = FALSE)
    )
    tmp <- left_join(dining_location_freq(), dining_location_spend(),
      by = "campus_location"
    )

    mapdf <- tmp %>%
      mutate(
        lng = case_when(
          campus_location == "West Union" ~ -78.939454,
          campus_location == "Wilson Gym" ~ -78.941449,
          campus_location == "Bryan Center" ~ -78.941014,
          campus_location == "Perkins" ~ -78.938645,
          campus_location == "McClendon Tower" ~ -78.937178,
          campus_location == "300 Swift" ~ -78.922188,
          campus_location == "The Nasher" ~ -78.929123,
          campus_location == "E-Quad" ~ -78.940290,
          campus_location == "East Campus" ~ -78.914628,
          TRUE ~ -78.933194
        ),
        lat = case_when(
          campus_location == "West Union" ~ 36.000774,
          campus_location == "Wilson Gym" ~ 35.997383,
          campus_location == "Bryan Center" ~ 36.001009,
          campus_location == "Perkins" ~ 36.002389,
          campus_location == "McClendon Tower" ~ 35.999341,
          campus_location == "300 Swift" ~ 36.002853,
          campus_location == "The Nasher" ~ 35.999024,
          campus_location == "E-Quad" ~ 36.003635,
          campus_location == "East Campus" ~ 36.007619,
          TRUE ~ 36.002414
        ),
        pop = paste(
          sep = "<br/>",
          paste0(
      "<b><a href='https://app.studentaffairs.duke.edu/dining/menus-hours/'>",
      campus_location, "</a></b>"),
          paste0(
            "Swipes: ",
            freq,
            " (", round(100 * freq / sum(tmp$freq)), "%)"
          ),
          paste0(
            "Spending: $", round(spending, 2),
            " (", round(100 * spending / sum(tmp$spending)), "%)"
          )
        )
      )

    leaflet(data = mapdf) %>%
      addTiles() %>%
      addMarkers(~lng, ~lat, popup = ~pop)
  })

  ## Output Text on Food Point Tip Tab
  output$tips_needed <- renderUI({
    switch(input$tips_options,
      "I'm Running Low!" = h4(tags$ul(
        tags$li(
          "Look at the Top 5 Restaurant bar plots and consider frequenting
             your top average spending locations less often. If you really enjoy
             these restaurants, consider ordering their $5 Daily Devil Deals,
             instead. If these top locations have a food in common, such as
             coffee, consider getting the monthly Panera coffee card"
        ),
        br(),
        tags$li(
          "View the data table on the Top 5 tab and frequent the location
             with the smallest average spending more often."
        ),
        br(),
        tags$li(
          "View the spending per week visualization and consider how your
             spending each week compares to your plan’s weekly average. Were
             there particular weeks where your spending was notably above the
             average amount? Consider what was going on during these weeks, and
             how you can use knowledge of this in the future."
        ),
        br(),
        tags$li(
          "Look at how your plan progression compares to your own spending.
             Would a different plan be more suitable for you in future
             semesters?"
        )
      )),
      "I Have Too Many Remaining!" = h4(tags$ul(
        tags$li(
          "Look at the Top 5 Restaurant bar plots and consider frequenting
             your top average spending locations more often."
        ),
        br(),
        tags$li(
          "Remember food_points roll upon completion of Fall Semester so you
          can chose a smaller meal plan for Spring Semester"
        ),
        br(),
        tags$li(
          "View the spending per week visualization and consider how your
             spending each week compares to your plan’s weekly average. Were
             there particular weeks where your spending was notably below the
             average amount? Consider what was going on during these weeks, and
             how you can use knowledge of this in the future."
        ),
        br(),
        tags$li(
          "Look at how your plan progression compares to your own spending.
              Would a different plan be more suitable for you in future
              semesters?"
        ),
        br(),
        tags$li(
          "Consider going to different campus locations to stock up on snacks,
          or get food from dining locations you have never visited before.
          You can also use Merchants on Points, which offers more expensive,
             high-quality food."
        )
      ))
    )
  })

  ## Create Top 5 Bar Plots
  ### Top Costs Plot
  plot_top_costs <- reactive({
    ggplot(
      data = food_points_location_cost(),
      aes(
        x = fct_reorder(restaurant, desc(total_spent)),
        y = total_spent,
        fill = restaurant,
        label = paste0("$", format(round(total_spent, 2), nsmall = 2))
      )
    ) +
      geom_col(show.legend = FALSE) +
      geom_text(vjust = -1, size = 4) +
      theme_minimal() +
      scale_y_continuous(labels = dollar_format()) +
      scale_x_discrete(name = NULL, labels = label_logos) +
      scale_fill_manual(values = restaurant_colors) +
      labs(
        x = NULL,
        y = "\nTotal Food Points Spent\n",
        title = "\nTotal Food Points Spent\nper Restaurant"
      ) +
      coord_cartesian(clip = "off") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_markdown()
      )
  })

  ### Top Frequency Plot
  plot_top_freq <- reactive({
    ggplot(
      data = food_points_location_freq(),
      aes(
        x = fct_reorder(restaurant, desc(freq)),
        y = freq,
        fill = restaurant,
        label = freq
      )
    ) +
      geom_col(show.legend = FALSE) +
      geom_text(vjust = -1, size = 4) +
      theme_minimal() +
      labs(
        x = NULL,
        y = "\nTotal Number of Swipes\n",
        title = "\nTotal Number of Card Swipes\nper Restaurant"
      ) +
      scale_x_discrete(name = NULL, labels = label_logos) +
      scale_y_continuous(labels = label_number(accuracy = 1)) +
      scale_fill_manual(values = restaurant_colors) +
      coord_cartesian(clip = "off") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_markdown()
      )
  })

  ### Top Average Plot
  plot_top_avg <- reactive({
    ggplot(
      data = food_points_location_avg(),
      aes(
        x = fct_reorder(restaurant, desc(avg)),
        y = avg,
        fill = restaurant,
        label = paste0("$", format(round(avg, 2), nsmall = 2))
      )
    ) +
      geom_col(show.legend = FALSE) +
      geom_text(vjust = -1, size = 4) +
      theme_minimal() +
      scale_y_continuous(labels = dollar_format()) +
      scale_x_discrete(name = NULL, labels = label_logos) +
      scale_fill_manual(values = restaurant_colors) +
      labs(
        x = NULL,
        y = "\nAverage Food Points Spent\nper Transaction\n",
        title = "\nAverage Food Points Spent\nper Transaction at Restaurant"
      ) +
      coord_cartesian(clip = "off") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_markdown()
      )
  })

  ### Patchwork Of All Three Plots
  all_three <- reactive(
    ({
      ggplot(
        data = food_points_location_freq(),
        aes(
          x = fct_reorder(restaurant, desc(freq)),
          y = freq,
          fill = restaurant,
          label = freq
        )
      ) +
        geom_col(show.legend = FALSE) +
        geom_text(vjust = -1, size = 3) +
        theme_minimal() +
        labs(
          x = NULL,
          y = "\nTotal Number of Swipes",
          title = "Total Number of Card Swipes\nper Restaurant"
        ) +
        scale_x_discrete(name = NULL, labels = label_logos_small) +
        scale_y_continuous(labels = label_number(accuracy = 1)) +
        scale_fill_manual(values = restaurant_colors) +
        coord_cartesian(clip = "off") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_markdown()
        )
    }
    + {
        ggplot(
          data = food_points_location_cost(),
          aes(
            x = fct_reorder(restaurant, desc(total_spent)),
            y = total_spent,
            fill = restaurant,
            label = paste0("$", format(round(total_spent, 2), nsmall = 2))
          )
        ) +
          geom_col(show.legend = FALSE) +
          geom_text(vjust = -1, size = 3) +
          theme_minimal() +
          scale_y_continuous(labels = dollar_format()) +
          scale_x_discrete(name = NULL, labels = label_logos_small) +
          scale_fill_manual(values = restaurant_colors) +
          labs(
            x = NULL,
            y = "\nTotal Food Points Spent",
            title = "Total Food Points Spent\nper Restaurant"
          ) +
          coord_cartesian(clip = "off") +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text.x = element_markdown()
          )
      }) / {
      ggplot(
        data = food_points_location_avg(),
        aes(
          x = fct_reorder(restaurant, desc(avg)),
          y = avg,
          fill = restaurant,
          label = paste0("$", format(round(avg, 2), nsmall = 2))
        )
      ) +
        geom_col(show.legend = FALSE) +
        geom_text(vjust = -1, size = 3) +
        theme_minimal() +
        scale_y_continuous(labels = dollar_format()) +
        scale_x_discrete(name = NULL, labels = label_logos_medium) +
        scale_fill_manual(values = restaurant_colors) +
        labs(
          x = NULL,
          y = "\nAverage Food Points Spent\nper Transaction",
          title = "Average Food Points Spent\nper Transaction at Restaurant"
        ) +
        coord_cartesian(clip = "off") +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_markdown()
        )
    }
  )

  ### Choose Which Plot To View
  plot_top_5 <- reactive({
    switch(input$top_5_input,
      "Total Number of Swipes per Restaurant" = plot_top_freq(),
      "Total Food Points Spent per Restaurant" = plot_top_costs(),
      "Average Food Points Spent per Restaurant" = plot_top_avg(),
      "All Three: Total Swipes, Total Spent, & Avg. Spent" = all_three()
    )
  })

  ### Output Bar Plot Choice
  output$plot_top_5 <- renderPlot({
    req(food_points())
    req(input$daterange)
    plot_top_5()
  })

  ## Create Time Series Spending Over Time Plots
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

  ### Output Plan Progression Plot
  output$overtime1 <- renderPlot({
    if (input$negative_values == TRUE) {
      time1 <- time_df() %>%
        mutate(
          user_points_total = ifelse(date > max(food_points()$date),
            NA, user_points_total
          ),
          points_remaining = plan_points[1] - user_points_total
        )

      ggplot(time1, aes(x = date, y = points_remaining)) +
        geom_line(aes(x = date, y = plan_points), color = "blue", size = .9) +
        geom_point(color = "red", size = 2) +
        geom_line(color = "red", size = .75) +
        labs(title = "Plan Progression", x = "Weeks", y = "Points Remaining") +
        geom_smooth(
          method = "lm", fullrange = TRUE, se = FALSE,
          color = "lightcoral", linetype = "dashed", size = .9
        ) +
        scale_x_date(
          breaks = time_df()$date[c(TRUE, FALSE)], date_labels = "%b-%d",
          minor_breaks = NULL
        ) +
        scale_y_continuous(labels = scales::dollar_format()) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold")
          ) +
        coord_cartesian(clip = "off")
    } else {
      time2 <- time_df() %>%
        mutate(
          user_points_total = ifelse(date > max(food_points()$date),
            NA, user_points_total
          ),
          points_remaining = plan_points[1] - user_points_total
        )

      ggplot(time2, aes(x = date, y = points_remaining)) +
        geom_line(aes(x = date, y = plan_points), color = "blue", size = .9) +
        geom_point(color = "red", size = 2) +
        geom_line(color = "red", size = .75) +
        labs(title = "Plan Progression", x = "Weeks", y = "Points Remaining") +
        geom_smooth(
          method = "lm", fullrange = TRUE, se = FALSE,
          color = "lightcoral", linetype = "dashed", size = .9
        ) +
        scale_x_date(
          breaks = time_df()$date[c(TRUE, FALSE)], date_labels = "%b-%d",
          minor_breaks = NULL
        ) +
        scale_y_continuous(limits = c(0, NA), labels = dollar_format()) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold")
          ) +
        coord_cartesian(clip = "off")
    }
  })

  ### Output Spending Per Week Plot
  output$overtime2 <- renderPlot({
    time_df() %>%
      ggplot(aes(x = date, y = user_points_week)) +
      geom_col() +
      geom_text(aes(
        x = date, y = user_points_week,
        label = paste0("$", round(user_points_week, 0))
      ),
      vjust = -0.25, size = 3
      ) +
      geom_hline(aes(yintercept = semester %>%
        filter(plan == str_extract(
          input$select_plan,
          "[:alpha:]$"
        )) %>%
        pull(weekly_avereage)),
      linetype = "longdash",
      color = "blue",
      size = .75
      ) +
      geom_hline(aes(yintercept = mean(user_points_week)),
        linetype = "longdash",
        color = "red",
        size = .75
      ) +
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
          paste0(
            "$",
            semester %>%
              filter(plan == str_extract(
                input$select_plan,
                "[:alpha:]$"
              )) %>%
              pull(weekly_avereage)
          )
        )
      ), color = "blue", size = 3.75) +
      geom_label(aes(
        x = date[15],
        y = mean(user_points_week),
        label = paste0(
          "Uploaded Average: ", "$",
          round(mean(user_points_week), 2)
        )
      ), color = "red", size = 3.75) +
      labs(title = "Spending Per Week", x = "Weeks", y = "Points Spent") +
      scale_x_date(
        breaks = time_df()$date[c(TRUE, FALSE)], date_labels = "%b-%d",
        minor_breaks = NULL
      ) +
      scale_y_continuous(labels = scales::dollar_format()) +
      coord_cartesian(clip = "off") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
        )
  })

  ## Create Plan Progression Key
  key <- tibble(
    "plan_prog_x" = c(1, 2, 3, 4, 5, 6, 7, 8),
    "plan_prog_y" = c(1, 1, 1, 1, 1, 1, 1, 1),
    "user_x" = c(1, 2, 3, 4, 5, 6, 7, 8),
    "user_y" = c(.5, .5, .5, .5, .5, .5, .5, .5),
    "reg_x" = c(1, 2, 3, 4, 5, 6, 7, 8),
    "reg_y" = c(0, 0, 0, 0, 0, 0, 0, 0)
  )

  ### Create Plan Progression Key Plot
  plot_key <- reactive({
    ggplot(key) +
      geom_line(aes(x = plan_prog_x, y = plan_prog_y), color = "blue") +
      geom_line(aes(x = user_x, y = user_y), color = "red") +
      geom_point(aes(x = user_x, y = user_y), color = "red") +
      geom_line(aes(x = reg_x, y = reg_y), color = "red", linetype = "dashed") +
      geom_text(aes(x = 4.5, y = 1.15),
        label = input$select_plan,
        color = "blue"
      ) +
      geom_text(aes(x = 4.54, y = .65),
        label = "Uploaded Data",
        color = "red"
      ) +
      geom_text(aes(x = 4.55, y = .15),
        label = "Linear Regression of Uploaded",
        color = "red"
      ) +
      ylim(-0.5, 1.5) +
      labs(title = "Plan Progression Key") +
      coord_cartesian(clip = "off") +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
        )
  })


  output$overtime_key <- renderPlot(plot_key(), height = 200)

  ## Create Functionality To Select A Plan
  plan_select_table_code <- reactive({
    timedf2 <- time_df() %>%
      mutate(
        user_points_total = ifelse(date > max(food_points()$date),
          NA, user_points_total
        ),
        points_remaining = plan_points[1] - user_points_total
      )

    tibble(
      "Plan Total" = c(paste0("$", max(timedf2$plan_points))),
      "Points Spent" = c(paste0(
        "$",
        round(max(timedf2$user_points_total,
          na.rm = TRUE
        ), 2)
      )),
      "Points Remaining" = c(paste0(
        "$",
        round(min(timedf2$points_remaining,
          na.rm = TRUE
        ), 2)
      ))
    )
  })

  ### Output Selected Plan
  output$plan_select_table <- renderTable(
    plan_select_table_code(),
    align = "c",
    bordered = TRUE
  )
}

## RUN APP ##

shinyApp(ui, server)