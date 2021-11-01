###Test

#Packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

## DATA ##
semester <- read_csv(here::here("data", "semester.csv"))

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