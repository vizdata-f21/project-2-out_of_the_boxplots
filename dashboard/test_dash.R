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
#library(khroma)
library(patchwork)
library(tools)
library(ggtext)
library(png)
library(ggpubr)
library(leaflet)

# SET TO TRUE AT THE END TO GET RID OF ALL POSSIBLE ERRORS
#options(shiny.sanitize.errors = FALSE)

## DATA ##
semester <- read_csv(here::here("dashboard/data", "semester.csv"))
usage_chart <- read_csv(here::here("dashboard/data", "usage_chart.csv"))
template <- read_csv(here::here("dashboard/data", "input_food_points_data.csv"))
campus_map <- readPNG(here::here("dashboard/www", "duke_campus_map.png"))

## UI ##
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$a(href='https://mralph15.shinyapps.io/dashboard/',
                                 tags$img(src = 'food_point_logo.png', height='40', width='180'))
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
        fluidRow(
          column(12,
                 align = "center", offset = 3,
                 box(
                   align = "center", width = 6,
                   uiOutput("location_date_range"),
                   selectInput("map_selection",
                               "Choose a Map:",
                               choices = c("Swipes",
                                           "Spending"))
                 )
          )
        ),
        fluidRow(
          column(12,
                 align = "center", offset = 1,
                 box(
                   align = "center", width = 10, height = 500,
                   h3("Title"),
                   leafletOutput("leafmap")
                   # plotOutput("top_5_locations",
                   #            dblclick = "map_dblclick",
                   #            brush = brushOpts(id = "map_brush",
                   #                              resetOnNew = TRUE))
                 )
          )
        )
        ),
      tabItem(
        tabName = "writeup",
        h2("Project Write Up:")
      ),
      tabItem(
        tabName = "spendingtips",
        h2("Food Point Spending Tips"),
        fluidRow(
          # Sidebar with a slider input for the number of bins
          column(12,
          h3("If you are running low on food points:"),
          p("- Look at the Top 5 Restaurant bar plots and consider frequenting
          your top average spending locations less often. If you really enjoy
          these restaurants, consider ordering their $5 Daily Devil Deals,
          instead. If these top locations have a food in common, such as
            coffee, consider getting the monthly Panera coffee card"),
          p("- View the data table on the Top 5 tab and frequent the location
            with the smallest average spending more often."),
          p("- View the spending per week visualization and consider how your
            spending each week compares to your plan’s weekly average. Were
            there particular weeks where your spending was notably above the
            average amount? Consider what was going on during these weeks, and
            how you can use knowledge of this in the future."),
          p(tags$ul(tags$li("Look at how your plan progression compares to your own spending.
            Would a different plan be more suitable for you in future
            semesters?"))))),
       fluidRow(
            column(12,
          h3("If you have too many food points remaining:"),
          p("- Look at the Top 5 Restaurant bar plots and consider frequenting
            your top average spending locations more often."),
          p("- View the spending per week visualization and consider how your
          spending each week compares to your plan’s weekly average. Were there
          particular weeks where your spending was notably below the average
          amount? Consider what was going on during these weeks, and how you can
          use knowledge of this in the future."),
          p("- Look at how your plan progression compares to your own spending.
          Would a different plan be more suitable for you in future semesters?"),
          p("- Consider going to The Lobby Shop more to stock up on snacks, or
          getting Merchants on Points."))
      )),
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
                   wellPanel(plotOutput("plot_top_5")))

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
        # set overall location
        campus_location = case_when(
          restaurant == "Bella Union" ~ "McClendon Tower",
          restaurant == "Beyu Blue" ~ "Bryan Center",
          restaurant == "Cafe" ~ "West Union",
          restaurant == "Farmstead" ~ "West Union",
          restaurant == "Food Trucks at 300 Swift" ~ "300 Swift",
          restaurant == "Ginger and Soy" ~ "West Union",
          restaurant == "Gyotaku" ~ "West Union",
          restaurant == "Il Forno" ~ "West Union",
          restaurant == "JBs Roast and Chops" ~ 'West Union',
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
        # mutate cost variable to make it numeric
        cost = as.numeric(str_extract_all(amount, "[0-9]*\\.[0-9]*")),
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
          campus_location == "Around Duke's Campus"~ 300,
          TRUE ~ dim(campus_map)[2] / 2),
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
          campus_location == "Around Duke's Campus"~ 270,
          TRUE ~ dim(campus_map)[1] / 2)
      )
  })

  # create logo image variable for bar plot
  label_logos <- c(
    "Bella Union" = "<img src='www/bella-union.png' width='110' />",
    "Beyu Blue" = "<img src='www/beyu_blue.jpeg' width='100' />",
    "Cafe" = "<img src='www/cafe.png' width='75' />",
    "Farmstead" = "<img src='www/farmstead.jpeg' width='110' />",
    "Food Trucks at 300 Swift" = "<img src='www/food-trucks-300-swift.png' width='110' />",
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

  label_logos_medium <- c(
    "Bella Union" = "<img src='www/bella-union.png' width='90' />",
    "Beyu Blue" = "<img src='www/beyu_blue.jpeg' width='80' />",
    "Cafe" = "<img src='www/cafe.png' width='60' />",
    "Farmstead" = "<img src='www/farmstead.jpeg' width='90' />",
    "Food Trucks at 300 Swift" = "<img src='www/food-trucks-300-swift.png' width='90' />",
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

  label_logos_small <- c(
    "Bella Union" = "<img src='www/bella-union.png' width='60' />",
    "Beyu Blue" = "<img src='www/beyu_blue.jpeg' width='50' />",
    "Cafe" = "<img src='www/cafe.png' width='40' />",
    "Farmstead" = "<img src='www/farmstead.jpeg' width='50' />",
    "Food Trucks at 300 Swift" = "<img src='www/food-trucks-300-swift.png' width='60' />",
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

  # set colors for each dining location
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

  # MAP PLOTS
  #code for location date slider
  output$location_date_range <- renderUI({
    req(input$student_data)
    req(food_points())
    sliderInput("dates_slider",
                "Select Range of Dates",
                min = min(food_points()$date),
                max = max(food_points()$date),
                value = c(min(food_points()$date),
                          max(food_points()$date)),
                timeFormat="%m-%d-%Y",
                step = (max(food_points()$date) - min(food_points()$date))/6)
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
    DT::datatable(tmp %>%
      select(date, restaurant, cost, points_remaining) %>%
      arrange(points_remaining) %>%
      rename(
        "Date (Y-M-D)" = "date",
        "Restaurant" = "restaurant",
        "Cost" = "cost",
        "Points Remaining" = "points_remaining"
      )) %>%
      DT::formatCurrency(c(3:4))
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

  food_points_all_info <- reactive({food_points() %>%
    filter(date >= input$daterange[1] & date <= input$daterange[2]) %>%
    group_by(restaurant) %>%
    summarize(freq = n(), total_cost = sum(cost), avg_cost = total_cost/freq) %>%
    arrange(desc(total_cost)) %>%
    rename("Restaurant" = "restaurant",
           "Frequency" = "freq",
           "Total Cost" = "total_cost",
           "Average Cost" = "avg_cost")
  })

  # BAR PLOT DATA TABLE
  output$food_points_all_info_table <- DT::renderDataTable({
    req(input$daterange)
    DT::datatable(food_points_all_info()) %>%
      DT::formatCurrency(c(3:4))
  })

  #MAP Plot
  #calculate most frequent locations visited within slider
  # top_5_location_spend <- reactive({
  #   food_points() %>%
  #     filter(date >= input$dates_slider[1] & date <= input$dates_slider[2]) %>%
  #     group_by(restaurant) %>%
  #     summarise(total_spent = sum(cost)) %>%
  #     arrange(desc(total_spent)) %>%
  #     head(5)
  # })

  dining_location_freq <- reactive({
    food_points() %>%
      filter(date >= input$dates_slider[1] & date <= input$dates_slider[2]) %>%
      group_by(campus_location) %>%#, x_coord, y_coord) %>%
      count() %>%
      # arrange(desc(n)) %>%
      rename(freq = n)
  })

  dining_location_spend <- reactive({
    food_points() %>%
      filter(date >= input$dates_slider[1] & date <= input$dates_slider[2]) %>%
      group_by(campus_location) %>%#, x_coord, y_coord) %>%
      summarise("spending" = sum(cost))
  })

  # top_5_location_avg <- reactive({
  #   food_points() %>%
  #     filter(date >= input$dates_slider[1] & date <= input$dates_slider[2]) %>%
  #     group_by(restaurant) %>%
  #     summarise_at(vars(cost), list(name = mean)) %>%
  #     head(5) %>%
  #     rename(avg = name)
  #
  # })

  # # zoomable map plot
  # map_ranges <- reactiveValues(x = NULL, y = NULL)
  #
  # top_5_locations <- reactive({
  #   ggplot(data = dining_location_freq(), aes(x = x_coord, y = y_coord)) +
  #     background_image(campus_map) +
  #     geom_label(aes(label = paste0(campus_location,
  #                                   ": ",
  #                                   freq,
  #                                   " (",
  #                                   round(100*freq/sum(freq)),
  #                                   "%)")), hjust = 0.5, nudge_y = 0.2) +
  #     xlim(0, 1000) +
  #     ylim(0, 600) +
  #     coord_cartesian(xlim = map_ranges$x, ylim = map_ranges$y,
  #                     expand = FALSE) +
  #     labs(title = "Campus Dining Location Swipes \n ") +
  #     theme_void() +
  #     theme(plot.title = element_text(hjust = 0.5, size = 16))
  # })
  #
  # top_5_locations2 <- reactive({
  #   ggplot(data = dining_location_spend(), aes(x = x_coord, y = y_coord)) +
  #     background_image(campus_map) +
  #     geom_label(aes(label = paste0(campus_location,
  #                                   ": $",
  #                                   round(spending,2),
  #                                   " (",
  #                                   round(100*spending/sum(spending)),
  #                                   "%)")), hjust = 0.5, nudge_y = 0.2) +
  #     xlim(0, 1000) +
  #     ylim(0, 600) +
  #     coord_cartesian(xlim = map_ranges$x, ylim = map_ranges$y,
  #                     expand = FALSE) +
  #     labs(title = "Campus Dining Location Spending \n ") +
  #     theme_void() +
  #     theme(plot.title = element_text(hjust = 0.5, size = 16))
  # })
  #
  # # detect double-click on map plot, check if there's a brush on the plot.
  # # If so, zoom to the brush bounds; if not, reset the zoom.
  # observeEvent(input$map_dblclick, {
  #   brush <- input$map_brush
  #   if (!is.null(brush)) {
  #     map_ranges$x <- c(brush$xmin, brush$xmax)
  #     map_ranges$y <- c(brush$ymin, brush$ymax)
  #   } else {
  #     map_ranges$x <- NULL
  #     map_ranges$y <- NULL
  #   }
  # })

  output$leafmap <- renderLeaflet({
    tmp <- left_join(dining_location_freq(), dining_location_spend(), by = "campus_location")

    mapdf <- tmp %>%
      mutate(lng = case_when(campus_location == "West Union" ~ -78.939454,
                             campus_location == "Wilson Gym" ~ -78.941449,
                             campus_location == "Bryan Center" ~ -78.941014,
                             campus_location == "Perkins" ~ -78.938645,
                             campus_location == "McClendon Tower" ~ -78.937178,
                             campus_location == "300 Swift" ~ -78.922188,
                             campus_location == "The Nasher" ~ -78.929123,
                             campus_location == "E-Quad" ~ -78.940290,
                             campus_location == "East Campus" ~ -78.914628,
                             TRUE ~ -78.933194),
             lat = case_when(campus_location == "West Union" ~ 36.000774,
                             campus_location == "Wilson Gym" ~ 35.997383,
                             campus_location == "Bryan Center" ~ 36.001009,
                             campus_location == "Perkins" ~ 36.002389,
                             campus_location == "McClendon Tower" ~ 35.999341,
                             campus_location == "300 Swift" ~ 36.002853,
                             campus_location == "The Nasher" ~ 35.999024,
                             campus_location == "E-Quad" ~ 36.003635,
                             campus_location == "East Campus" ~ 36.007619,
                             TRUE ~ 36.002414),
             pop = case_when(campus_location == "West Union" ~ "test",
                             campus_location == "Wilson Gym" ~ "test",
                             campus_location == "Bryan Center" ~ "test",
                             campus_location == "Perkins" ~ "test",
                             campus_location == "McClendon Tower" ~ "test",
                             campus_location == "300 Swift" ~ "test",
                             campus_location == "The Nasher" ~ "test",
                             campus_location == "E-Quad" ~ "test",
                             campus_location == "East Campus" ~ "test",
                             TRUE ~ "36.002414"))

    leaflet(data = mapdf) %>%
      addTiles() %>%
      addMarkers(~lng, ~lat, label = ~campus_location, popup = ~pop,
                 labelOptions = labelOptions(noHide = TRUE))
  })

  # BAR PLOT GGPLOT CODE
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
        axis.text.x = element_markdown(),
        text = element_text(family = "Times New Roman")
      )
  })

  plot_top_freq <- reactive({
    ggplot(
      data = food_points_location_freq(),
      aes(
        x = fct_reorder(restaurant, desc(freq)),
        y = freq,
        fill = restaurant,
        label = freq)
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
        axis.text.x = element_markdown(),
        text = element_text(family = "Times New Roman")
      )
  })

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
        axis.text.x = element_markdown(),
        text = element_text(family = "Times New Roman")
      )
  })

  all_three <- reactive(
    (
      {
        ggplot(
          data = food_points_location_freq(),
          aes(
            x = fct_reorder(restaurant, desc(freq)),
            y = freq,
            fill = restaurant,
            label = freq)
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
            axis.text.x = element_markdown(),
            text = element_text(family = "Times New Roman")
          )
      }
      +
      {
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
            axis.text.x = element_markdown(),
            text = element_text(family = "Times New Roman")
          )
      }
    ) /
      {
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
            axis.text.x = element_markdown(),
            text = element_text(family = "Times New Roman")
          )
      }
  )

  plot_top_5 <- reactive({
    switch(input$top_5_input,
           "Total Number of Swipes per Restaurant" = plot_top_freq(),
           "Total Food Points Spent per Restaurant" = plot_top_costs(),
           "Average Food Points Spent per Restaurant" = plot_top_avg(),
           "All Three: Total Swipes, Total Spent, & Avg. Spent" = all_three()
    )
  })

  output$top_5_locations <- renderPlot({
    validate(
      need(input$dates_slider, message = FALSE)
    )
    switch(input$map_selection,
           "Swipes" = top_5_locations(),
           "Spending" = top_5_locations2()
    )
  })

  output$plot_top_5 <- renderPlot({
    req(food_points())
    req(input$daterange)
    plot_top_5()
  })

  #CAN DELETE BELOW AFTER BLOSSOM FINISHES MAP (keeping just in case)

  # # OVERALL LOCATION PLOTS DATA WRANGLING
  #
  # food_points_overall_location_freq <- reactive({
  #   food_points() %>%
  #     filter(date >= input$daterange[1] & date <= input$daterange[2]) %>%
  #     group_by(campus_location) %>%
  #     count() %>%
  #     arrange(desc(n)) %>%
  #     rename(freq = n) %>%
  #     mutate(prop = freq / nrow(food_points),
  #            bar = 1) %>%
  #     head(5) })
  #
  # # OVERALL LOCATION PLOTS GGPLOT
  #
  # plot_top_freq_overall_location <- reactive({
  # ggplot(
  #   data = food_points_overall_location_freq(),
  #   aes(
  #     x = bar,
  #     y = prop,
  #     fill = fct_reorder(campus_location, prop))
  # ) +
  #   geom_bar(position = "fill", stat = "identity") +
  #   coord_flip() +
  #   #scale_y_continuous(labels = percent_format()) +
  #   theme_minimal() +
  #   labs(
  #     x = NULL,
  #     y = "\n Percent of Total Number of Swipes",
  #     fill = "Dining Location",
  #     title = "Percent of Total Number of Card Swipes\nper Dining Location"
  #   ) +
  #   theme(
  #     plot.title = element_text(hjust = 0.5, face = "bold"),
  #     panel.grid.major.y = element_blank(),
  #     panel.grid.minor.y = element_blank(),
  #     axis.text.y = element_markdown(),
  #     text = element_text(family = "Times New Roman")
  #   )})


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
  # plot_top_avg <- reactive({
  #   ggplot(
  #     data = food_points_location_avg(),
  #     aes(
  #       x = fct_reorder(restaurant, desc(avg)),
  #       y = avg,
  #       fill = restaurant,
  #       label = paste0("$", format(round(avg, 2), nsmall = 2))
  #     )
  #   )
  #
  # #variable to save campus_map
  # campus_map <- readPNG("../images/duke_campus_map.png")
  # data <- read.csv("../data/blossom_food_points.csv")
  # map_plot <- reactive({
  #   ggplot(data) +
  #   background_image(campus_map)

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
        scale_x_date(breaks = time_df()$date[c(TRUE, FALSE)], date_labels = "%b-%d",
                     minor_breaks = NULL) +
        scale_y_continuous(labels = scales::dollar_format()) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
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
        geom_line(aes(x = date, y = plan_points), color = "blue") +
        geom_point(color = "red") +
        geom_line(color = "red") +
        labs(title = "Plan Progression", x = "Weeks", y = "Points Remaining") +
        geom_smooth(
          method = "lm", fullrange = TRUE, se = FALSE,
          color = "lightcoral", linetype = "dashed"
        ) +
        scale_x_date(breaks = time_df()$date[c(TRUE, FALSE)], date_labels = "%b-%d",
                     minor_breaks = NULL) +
        scale_y_continuous(limits = c(0, NA), labels = dollar_format()) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
        coord_cartesian(clip = "off")
    }
  })

  output$overtime2 <- renderPlot({
    time_df() %>%
      ggplot(aes(x = date, y = user_points_week)) +
      geom_col() +
      geom_text(aes(x = date, y = user_points_week,
                    label = paste0("$", round(user_points_week, 0))),
                vjust=-0.25, size = 3) +
      geom_hline(aes(yintercept = semester %>%
                       filter(plan == str_extract(
                         input$select_plan,
                         "[:alpha:]$"
                       )) %>%
                       pull(weekly_avereage)), linetype = "longdash", color = "blue", size = .75) +
      geom_hline(aes(yintercept = mean(user_points_week)), linetype = "longdash", color = "red", size = .75) +
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
      ), color = "blue", size = 3.75) +
      geom_label(aes(
        x = date[15],
        y = mean(user_points_week),
        label = paste0("Uploaded Average: ", "$", round(mean(user_points_week), 2))
      ), color = "red", size = 3.75) +
      labs(title = "Spending Per Week", x = "Weeks", y = "Points Spent") +
      scale_x_date(breaks = time_df()$date[c(TRUE, FALSE)], date_labels = "%b-%d",
                   minor_breaks = NULL) +
      scale_y_continuous(labels = scales::dollar_format()) +
      coord_cartesian(clip = "off") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16))
  })

  key <- tibble("plan_prog_x" = c(1,2,3,4,5,6,7,8),
                "plan_prog_y" = c(1,1,1,1,1,1,1,1),
                "user_x" = c(1,2,3,4,5,6,7,8),
                "user_y" = c(.5,.5,.5,.5,.5,.5,.5,.5),
                "reg_x" = c(1,2,3,4,5,6,7,8),
                "reg_y" = c(0,0,0,0,0,0,0,0))

  plot_key <- reactive({ggplot(key) +
    geom_line(aes(x = plan_prog_x, y = plan_prog_y), color = "blue") +
    geom_line(aes(x = user_x, y = user_y), color = "red") +
    geom_point(aes(x = user_x, y = user_y), color = "red") +
    geom_line(aes(x = reg_x, y = reg_y), color = "red", linetype = "dashed") +
    geom_text(aes(x = 4.5, y = 1.15),
              label = input$select_plan,
              color = "blue") +
    geom_text(aes(x = 4.54, y = .65),
              label = "Uploaded Data",
              color = "red") +
    geom_text(aes(x = 4.55, y = .15),
              label = "Linear Regression of Uploaded",
              color = "red") +
    ylim(-0.5, 1.5) +
    labs(title = "Plan Progression Key") +
    coord_cartesian(clip = "off") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 16))
  })


  output$overtime_key <- renderPlot(plot_key(), height = 200)

  plan_select_table_code <- reactive({
    timedf2 <- time_df() %>%
      mutate(
        user_points_total = ifelse(date > max(food_points()$date),
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