## PACKAGES ##
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

options(shiny.sanitize.errors = FALSE)

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
              fileInput(
                "student_data",
                "Upload Your Food Point Usage:"
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