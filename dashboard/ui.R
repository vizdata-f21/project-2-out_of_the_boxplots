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

semester <- read_csv(here::here("data", "semester.csv"))
usage_chart <- read_csv(here::here("data", "usage_chart.csv"))
template <- read_csv(here::here("data", "input_food_points_data.csv"))

# UI ---------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$a(
      href = "https://mralph15.shinyapps.io/dashboard/",
      tags$img(src = "food_point_logo.png", height = "40", width = "180")
    )
  ),
  ## Create Sidebar Tabs -------------------------------------------------------
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
      ## Upload Instructions Tab -----------------------------------------------
      tabItem(
        tabName = "upload",
        h2(a("How to Access and Upload Your Food Points",
             href = "https://dukecard.duke.edu/manage-your-dukecard")),
        fluidRow(
          img(
            src = "food-points-instructions.png", height = 800, width = 700,
            style = "display: block; margin-left: auto; margin-right: auto;",
            alt =
              "This image shows instructions on how to access and upload your
              food points. 1) Click on Download Food Point Template on the
              Dashboard, and navigate to bit.ly/food-points. 2) Once navigating
              to the link, click on EACOUNTS LOG IN at the bottom of My
              DukeCard. 3) Log in with your Duke NetID. 4) Click on Account
              Transactions, which appears in the top gray bar. 5) Select “Food”
              under the Account tab, and then choose which dates you would like
              Food Points data from under the Transaction Period tab. NOTE:
              Decide which 2021 - 2022 semester you would like to see
              visualizations for. The dates selected must be within only that
              Fall or Spring semester. Press search on the bottom right. 6)
              Highlight over the rows you would like data from. Copy and paste
              into the Food Point Template Excel file you downloaded in the
              first step. Ensure that Duke’s initial deposit of your food points
              is included in what you copy and paste. Example rows with this
              information can be found below. Once you have navigated through
              the pages you wish to select data from on the bottom left, upload
              your Food Point Usage to the Dashboard.",
            title =
              "This image shows instructions on how to access and upload your
              food points. 1) Click on Download Food Point Template on the
              Dashboard, and navigate to bit.ly/food-points. 2) Once navigating
              to the link, click on EACOUNTS LOG IN at the bottom of My
              DukeCard. 3) Log in with your Duke NetID. 4) Click on Account
              Transactions, which appears in the top gray bar. 5) Select “Food”
              under the Account tab, and then choose which dates you would like
              Food Points data from under the Transaction Period tab. NOTE:
              Decide which 2021 - 2022 semester you would like to see
              visualizations for. The dates selected must be within only that
              Fall or Spring semester. Press search on the bottom right. 6)
              Highlight over the rows you would like data from. Copy and paste
              into the Food Point Template Excel file you downloaded in the
              first step. Ensure that Duke’s initial deposit of your food points
              is included in what you copy and paste. Example rows with this
              information can be found below. Once you have navigated through
              the pages you wish to select data from on the bottom left, upload
              your Food Point Usage to the Dashboard."
          ),
        )
      ),
      ## Food Points Overview Tab ----------------------------------------------
      tabItem(
        tabName = "overview",
        h2("Food Points Overview"),
        p(
          "First learn how to upload your data by going to the upload
          instructions tab! Then, naviagte through the tabs in whichever order
          you prefer."
        ),
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
      ## Spending Over Time Tab ------------------------------------------------
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
      ## Dining Locations Tab --------------------------------------------------
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
      ## Project Write-Up Tab --------------------------------------------------
      tabItem(
        tabName = "writeup",
        h2("Project Write Up"),
        fluidPage(
          column(12,
                 h3("Introducton:"),
                 p(
                   "How can Duke students better understand their food points
                   usage? Every semester, Duke offers the same various food
                   plans to meet the diverse needs of their undergraduate
                   students. These customized plans are tailored specifically
                   for upper-class, first-year and off-campus students, yet most
                   students, regardless of their food plan, face the same issue
                   — they always either run out of food points, or have too many
                   remaining by the end of the semester. For instance, the
                   dreaded feeling of bargaining for your friend’s spare food
                   points is something many Duke students have felt at one point
                   in time, and this is the motivation for the pertinent issue
                   we explore. With all the options Duke offers, why is it so
                   hard to budget our food points, and what can help us
                   understand our spending better? Although the purpose of this
                   project is not to create a food-management tool, our goal is
                   that by creating a report which illustrates food point usage,
                   Duke undergraudate students can better understand their own
                   spending, and use the information to inform any personal
                   spending goals they may develop."
                 ),
                 p(
                   "Ergo, to alleviate the mental hurdles that come with
                   deciding how much money we can spend on food, we have created
                   a R Shiny dashboard app that uses a Duke student’s food plan
                   and data from their DukeCard Account Transaction Report to
                   generate a visual report on their spending. To do this we
                   utilize the information available in a student’s personal
                   usage data — accessed through their",
                   a(
                     'DukeCard eAccounts login',
                     href = "https://dukecard.duke.edu/manage-your-dukecard"
                   ),
                   "— to provide a comprehensive report on their food spending.
                   We consulted Duke Dining on the best way to extract food
                   point data and they conccured that our method of copying and
                   pasting was most efficient, rather than using an inaccessible
                   API.  Following instructions on the dashboard, students copy
                   and paste their personal eAccounts information into a
                   provided Excel template, to be uploaded to the R Shiny app.
                   The template includes variables for ", em("Date/Time"),
                   "(date and time purchase occurred)", em(", Account Name, "),
                   em("Card Number, "), em("Location "),
                   "(restaurant purchase occurred at), ",
                   em("Transaction Type, "), "and ", em("Amount"),
                   "(cost of purchase)."),
                 p(
                   "Other datasets used to accomplish this goal include ",
                   em("semester.csv,")," which offers a brief summary of each
                   plan, allowing us to quickly outline every plan’s offerings.
                   Each observation is one of Duke's 8 offered undergraduate
                   food plans. The accompanying variables are information about
                   the total number of points offered on the plan, the expected
                   number of points to be spent per week, and the expected
                   number of points to be spent per day. There are",
                   nrow(semester), "observations and", ncol(semester),
                   "variables in the data set. The data was collected from",
                   a(
                     "Duke's Food Plan and Points Website",
                     href = "https://studentaffairs.duke.edu/dining/plans-points"
                   ),
                   ". The second dataset we use,", em("usage_chart.csv," ),
                   "offers in depth recommended spending information for each
                   plan, which provides the ability to track a user’s spending
                   against the advised spending. Each observation is a week
                   during a Duke undergraduate semester in the 2021-2022 school
                   year. The accompanying variables are information about the
                   start date for the week in the Fall or Spring semester
                   (see ", em("fall"), " and ", em("spring "), "variables), and
                   the expected number of food points to start that week with,
                   depending on which plan a student has. There are",
                   nrow(usage_chart), "observations and", ncol(usage_chart),
                   "variables in the data set. The data was collected from
                   Duke's",
                   a(
                     "Fall 2021",
                     href = "https://studentaffairs.duke.edu/sites/default/files/2021-07/Food%20Point%20Usage%20Chart%20Fall-%2021.pdf"
                   ),
                   "and ",
                   a(
                     "Spring 2022",
                     href = "https://studentaffairs.duke.edu/sites/default/files/2021-07/Food%20Point%20Usage%20Chart%20Spring-22.pdf"),
                   "Food Point Calculators."
                 ),
                 p(
                   "A comprehensive code book for each data set is located in
                   the",
                   a(
                     "GitHub repository",
                     href = "https://github.com/vizdata-f21/project-2-out_of_the_boxplots/tree/main/dashboard/data"
                   ),
                   ". Using the DukeCard statement data in the user input
                    template, the R Shiny app generates a usage report with
                    visualizations. The report's purpose is to help its intended
                    audience of undergraduate Duke students be better informed
                    about their food point spending decision making, thus
                    helping to accomplish any goals they may have set for
                    themselves. The dashboard does this by providing students
                    with the necessary insight on how much they can spend on
                    food, where they should or shouldn’t consider spending their
                    food, and what plan may be best for their spending habits,
                    while staying within the limits of their food plan."
                 ),
                 p(
                   "The repository's",
                   a(
                     "sample data sets folder",
                     href = "https://github.com/vizdata-f21/project-2-out_of_the_boxplots/tree/main/sample%20food%20point%20data%20sets"),
                   "contains two sample food point templates filled out with
                    team members' data. Users who wish to view the functionality
                    of the app, but who are not students or do not have access
                    to eAccounts data, are welcome to upload these samples."
                 ),
                 h3("Justification & Explanation of Approach:"),
                 h4("Upload Instructions Tab"),
                 p(
                   "Students begin by uploading their filled-in template csv
                   file containing a full account of their food point purchases
                   over a selected semester. Users are instructed to visit the
                   upload instructions tab for further guidance on how to do
                   this. This step must be completed first in order to populate
                   all subsequent visualizations and information on the
                   generated report. However, once completed, users may navigate
                   the tabs in whichever order they please, such that they are
                   most effectively able to inform their food point spending
                   goals."
                 ),
                 img(
                   src = "food-points-instructions.png", height = 500, width = 450,
                   style = "display: block; margin-left: auto; margin-right: auto;",
                   alt =
                     "This image shows instructions on how to access and upload
                    your food points. 1) Click on Download Food Point Template
                    on the Dashboard, and navigate to bit.ly/food-points. 2)
                    Once navigating to the link, click on EACOUNTS LOG IN at the
                    bottom of My DukeCard. 3) Log in with your Duke NetID. 4)
                    Click on Account Transactions, which appears in the top gray
                    bar. 5) Select “Food” under the Account tab, and then choose
                    which dates you would like Food Points data from under the
                    Transaction Period tab. NOTE: Decide which 2021 - 2022
                    semester you would like to see visualizations for. The dates
                    selected must be within only that Fall or Spring semester.
                    Press search on the bottom right. 6) Highlight over the rows
                    you would like data from. Copy and paste into the Food Point
                    Template Excel file you downloaded in the first step. Ensure
                    that Duke’s initial deposit of your food points is included
                    in what you copy and paste. Example rows with this
                    information can be found below. Once you have navigated
                    through the pages you wish to select data from on the bottom
                    left, upload your Food Point Usage to the Dashboard.",
                   title =
                     "This image shows instructions on how to access and upload
                    your food points. 1) Click on Download Food Point Template
                    on the Dashboard, and navigate to bit.ly/food-points. 2)
                    Once navigating to the link, click on EACOUNTS LOG IN at the
                    bottom of My DukeCard. 3) Log in with your Duke NetID. 4)
                    Click on Account Transactions, which appears in the top gray
                    bar. 5) Select “Food” under the Account tab, and then choose
                    which dates you would like Food Points data from under the
                    Transaction Period tab. NOTE: Decide which 2021 - 2022
                    semester you would like to see visualizations for. The dates
                    selected must be within only that Fall or Spring semester.
                    Press search on the bottom right. 6) Highlight over the rows
                    you would like data from. Copy and paste into the Food Point
                    Template Excel file you downloaded in the first step. Ensure
                    that Duke’s initial deposit of your food points is included
                    in what you copy and paste. Example rows with this
                    information can be found below. Once you have navigated
                    through the pages you wish to select data from on the bottom
                    left, upload your Food Point Usage to the Dashboard."
                 ),
                 h4("Overview Tab"),
                 p(
                   "Once uploaded, the Shiny app displays which food plan the
                   user has, the total points offered on their plan, their
                   points spent thus far, and their points remaining for the
                   semester. Beneath this information, users have access to a
                   populated searchable data table with their relevant uploaded
                   information. The Dining Location variable was created based
                   on the authors' knowledge of restaurant locations on campus,
                   with the goal of providing more nuance to location
                   information."
                 ),
                 img(
                   src = "overview.png", height = 500, width = 800,
                   style = "display: block; margin-left: auto; margin-right: auto;",
                   alt =
                     "This image displays the landing page for the dashboard
                     with a student's data loaded in. In the top left corner is
                     a button to download a template for uploading a user's food
                     points and an upload button to upload this template once
                     filled in. In the top right corner is an indicator of what
                     food point plan the user has uploaded, Plan J, a summary of
                     the plan's total food points, $1818, the number of points
                     the user has spent, $921, and the number of points they
                     have remaining, $897. The bottom half of the image is a
                     table displaying each of the user's food point transactions
                     and includes the transaction date, restaurant, dining
                     location, cost, and points remaining after the
                     transaction.",
                   title =
                     "This image displays the landing page for the dashboard
                     with a student's data loaded in. In the top left corner is
                     a button to download a template for uploading a user's food
                     points and an upload button to upload this template once
                     filled in. In the top right corner is an indicator of what
                     food point plan the user has uploaded, Plan J, a summary of
                     the plan's total food points, $1818, the number of points
                     the user has spent, $921, and the number of points they
                     have remaining, $897. The bottom half of the image is a
                     table displaying each of the user's food point transactions
                     and includes the transaction date, restaurant, dining
                     location, cost, and points remaining after the
                     transaction."
                 ),
                 h4("Spending Over Time Tab"),
                 p(
                   "Subsequently, students may move to the Spending Over Time
                   tab, which allows the opportunity to select the semester and
                   corresponding food plan users wish to have analyzed. Once
                   selected, the application generates two plots. The bar plot
                   shows the student’s average spending each week. We include
                   labels which compare the user’s average spending across weeks
                   with Duke's suggested average weekly spend for a chosen plan.
                   The purpose of this plot is to indicate whether or not a
                   student is overspending, on average, and help the user
                   identify the weeks where the overspending or underspending
                   occurred, if they wish to do so. The line plot projects if,
                   and when, a student will run out of food points, which is
                   denoted by the line “Expected Progression.” To obtain our
                   Expected Progression we utilized ggplot's",
                   em("geom_smooth "), "linear regression functionality with the
                   student's uploaded data. The expected progression predicts
                   the student’s remaining food point balance week by week over
                   the selected semester. The non-dashed red line indicates the
                   user's inputted data, while the blue line indicates the
                   reccomended spending for each week, based on the selected
                   plan. The purpose of this plot is to give a generalized idea
                   of a student’s spending habits and how their current spending
                   will impact their remaining balance later in the semester.
                   This is valuable insight because the plot shows precisely how
                   much a student can stretch their food points without running
                   out. This page’s functionality also allows users to see how
                   their data compares to the recommended spending for
                   alternative plans, which is useful when deciding on whether
                   to switch food plans in future semesters."
                 ),
                 p(
                   "In the sample visualization below, created using Matt’s
                   data, we assume Matt’s goal is to know whether he should
                   upgrade to plan A, from his current plan J, for next
                   semester. The bar plot makes clear that if the user has plan
                   A, he would be spending much less per week than allotted on
                   his selected plan. If there are certain bars in the spending
                   per week visualization which are notably higher, or lower,
                   than the others, a user can also take the opportunity to
                   reflect on what occured during these weeks. In Matt’s plot,
                   he spent very little the week of August 23rd, likely because
                   it was the start of the semester, which may inform his future
                   spending choices. Following this trend, based on his current
                   spending, the user is projected to end the semester with a
                   little less than $1,500 on plan A. Consequently, the
                   visualizations inform Matt that he should not switch to a
                   larger plan."
                 ),
                 img(
                   src = "spending-over-time-a.png", height = 450, width = 750,
                   style = "display: block; margin-left: auto; margin-right: auto;",
                   alt =
                     "In this image users are given the option to compare their
                     own usage across the plans offered by Duke dining by
                     selecting a food plan and semester via the drop down menu
                     in the top left corner. Once a food plan is selected (Plan
                     A in this case), the Plan Total, Points Spent, and Points
                     Remaining are displayed in the top middle of the dashboard
                     while two plots are generated in the lower half. Plan A
                     offers a total of $2570.33, the user has spent $839.71, and
                     $1730.62 remain.

                     The first plot is a bar plot titled 'Spending per Week'. It
                     shows a user's spending per week over the course of a
                     semester with bars denoting the amount spent week by week
                     and horizontal lines denoting the user's weekly average
                     compared to the food plan's suggested weekly average. Our
                     user spends $51.18 on average each week which is lower than
                     plan A's suggested weekly average of $165.07.

                     The second plot is titled 'Plan Progression' and uses a
                     linear regression model to show a user's projected number
                     of remaining food points based on their current spending
                     habits. This projected line plot is plotted alongside the
                     selected plan's suggested usage rate and the user's current
                     usage rate. Based on their spending habits, the user is not
                     projected to run out of food points if they were to switch
                     to Plan A.",
                   title =
                     "In this image users are given the option to compare their
                     own usage across the plans offered by Duke dining by
                     selecting a food plan and semester via the drop down menu
                     in the top left corner. Once a food plan is selected (Plan
                     A in this case), the Plan Total, Points Spent, and Points
                     Remaining are displayed in the top middle of the dashboard
                     while two plots are generated in the lower half. Plan A
                     offers a total of $2570.33, the user has spent $839.71, and
                     $1730.62 remain.

                     The first plot is a bar plot titled 'Spending per Week'. It
                     shows a user's spending per week over the course of a
                     semester with bars denoting the amount spent week by week
                     and horizontal lines denoting the user's weekly average
                     compared to the food plan's suggested weekly average. Our
                     user spends $51.18 on average each week which is lower than
                     plan A's suggested weekly average of $165.07.

                     The second plot is titled 'Plan Progression' and uses a
                     linear regression model to show a user's projected number
                     of remaining food points based on their current spending
                     habits. This projected line plot is plotted alongside the
                     selected plan's suggested usage rate and the user's current
                     usage rate. Based on their spending habits, the user is not
                     projected to run out of food points if they were to switch
                     to Plan A."
                 ),
                 p(
                   "Alternatively, if Matt’s goal is to know whether he should
                   switch to a smaller plan for next semester, he can toggle
                   between plans and compare his current food plan to a smaller
                   one, such as plan I. The bar plot below shows that Matt’s
                   current spending is much closer aligned to plan I than any of
                   the larger plans. Therefore, in combination, these
                   visualizations inform the user on which meal plan is right
                   for their spending patterns."
                 ),
                 img(
                   src = "spending-over-time-b.png", height = 450, width = 750,
                   style = "display: block; margin-left: auto; margin-right: auto;",
                   alt =
                     "This image shows the same plot as above, but with Plan I
                     selected. Plan I is smaller than Plan A. It has a total of
                     $885.8; the user has spent $839.71, and $46.09 remains.

                     The first plot shows our user spends $51.18 on average each
                     week, which is lower than plan I's suggested weekly average
                     of $56.89.

                     The second plot shows the user is projected to run out of
                     their food points before November 22nd if they were to
                     switch to Plan I.",
                   title =
                     "This image shows the same plot as above, but with Plan I
                     selected. Plan I is smaller than Plan A. It has a total of
                     $885.8; the user has spent $839.71, and $46.09 remains.

                     The first plot shows our user spends $51.18 on average each
                     week, which is lower than plan I's suggested weekly average
                     of $56.89.

                     The second plot shows the user is projected to run out of
                     their food points before November 22nd if they were to
                     switch to Plan I."
                 ),
                 h4("Dining Locations Tab"),
                 p(
                   "The next tab, Dining Locations, allows viewers to see a
                   Leaflet map of Duke’s campus. We chose to include a map as
                   one of our visualizations because it allows students to
                   visualize which areas of campus they frequent more or less,
                   which may help them remember specific dining buildings that
                   are key in the development of their spending goals. The
                   slider on the top of the page provides the ability to choose
                   a desired date range in the data to be visualized, helping
                   students see data for a specific period of time they may be
                   interested in. If one week's bar stood out on the spending
                   over time page, users may wish to view this date range, for
                   example. Additionally, if clicked, each pin on the map
                   displays the number of transactions and amount of money spent
                   at each location, accompanied with percentages detailing how
                   that compares relative to other dining locations. The map
                   also has zoom functionality. If we were to interact with the
                   visualization below, we would see that Matt spends most of
                   his time eating at West Union which contributes to 84% of his
                   total spend."
                 ),
                 img(
                   src = "dining-locations.png", height = 450, width = 800,
                   style = "display: block; margin-left: auto; margin-right: auto;",
                   alt =
                     "This figure displays a map titled 'Campus Dining
                     Locations' with all the dining locations at Duke. Users can
                     click on each location's pin to return the number of
                     transactions made and amount of money spent at each dining
                     location. This semester the user has frequented West Union
                     71 times spending a total of $776.42, or 84% of their spent
                     food points there. The map includes zoom capabilities, and
                     users are able to set which dates they would like to see
                     data for by using the slider at the top. The names of each
                     location are also hyperlinked to the duke dining hour
                     website. This map displays seven location pins.",
                   title =
                     "This figure displays a map titled 'Campus Dining
                     Locations' with all the dining locations at Duke. Users can
                     click on each location's pin to return the number of
                     transactions made and amount of money spent at each dining
                     location. This semester the user has frequented West Union
                     71 times spending a total of $776.42, or 84% of their spent
                     food points there. The map includes zoom capabilities, and
                     users are able to set which dates they would like to see
                     data for by using the slider at the top. The names of each
                     location are also hyperlinked to the duke dining hour
                     website. This map displays seven location pins."
                 ),
                 h4("Top 5 Restaurants Tab"),
                 p(
                   "Our next set of visualizations provide a comprehensive,
                   detailed look into where a student is spending their food
                   points. This differs from the information the map provides,
                   as this tab focuses on Duke restaurants, whereas the map
                   focused on overall dining locations, such as West Union. We
                   created vertical bar charts to display a student’s top 5
                   restaurants based on their total number of card swipes, food
                   points, and average food points spent per transaction at each
                   dining location over a desired date range. Each plot can be
                   viewed individually, or all together, allowing a user to
                   compare across measures. We chose to include these
                   visualizations to accompany the line plots because they
                   provide a detailed look into where food points are being
                   spent, rather than a broader trend of spending. Students can
                   use this information to decide how to better tailor their
                   spending at their favorite restaurants, such as cutting back
                   on JBs due to its high average transaction cost. To increase
                   aesthetic appeal, each axis label is a respective
                   restaurant's logo, and each bar is filled in with a prominent
                   color from the logo. We photoshopped custom labels for
                   restaurants who did not have logos, in order to standardize
                   axis apperances. All bars have exact values above them to
                   help students be more informed about their precise spending.
                   For those students who wish to see information for
                   restaurants other than their top five, a table below the
                   plots contains statistics for all restaurants."
                 ),
                 img(
                   src = "top-5.png", height = 500, width = 800,
                   style = "display: block; margin-left: auto; margin-right: auto;",
                   alt =
                     "The figure shows three different bar charts. Users can
                     select individual bar charts to view using a drop down
                     menu. The dates can be changed using a calendar at the top
                     of the screen. All bar plots show data for a 4 month
                     period, from August to November.

                     The first bar chart in the upper left hand corner is titled
                     'Total Number of Card Swipes per Restaurant' and it
                     displays the number of total card swipes for the top five
                     restaurants (Ginger and Soy, Sazon, Panera, JBs, and Cafe,
                     in this specific image). The user had 24 swipes at Ginger
                     and Soy, 15 at Sazon, 10 at Panera, 9 at JBs, and 6 at
                     Cafe.

                     The second bar chart in the upper right hand corner is
                     titled 'Total Food Points Spent per Restaurant' and it
                     displays the total number of food points spent at each of
                     the top five restaurants (Ginger and Soy, Sazon, JBs,
                     Panera, and Pitchforks, in this specific image). The user
                     spent approximately $255 at Ginger and Soy, $176 at Sazon,
                     $111 at JBs, $108 at Panera, and $57 at Pitchforks.

                     The third bar chart on the bottom is titled 'Average Food
                     Points Spent per Transaction at Restaurant' and it displays
                     the average number of food points spent per transaction at
                     each of the top five restaurants (JBs, Panera, Ginger and
                     Soy, Il Forno, and Cafe, in this specific image). Per
                     transaction, the user spent approximately $12 at JBs, $11
                     at Panera, $11 at Ginger and Soy, $10 at Il Forno, and $9
                     at Cafe.",
                   title =
                     "The figure shows three different bar charts. Users can
                     select individual bar charts to view using a drop down
                     menu. The dates can be changed using a calendar at the top
                     of the screen. All bar plots show data for a 4 month
                     period, from August to November.

                     The first bar chart in the upper left hand corner is titled
                     'Total Number of Card Swipes per Restaurant' and it
                     displays the number of total card swipes for the top five
                     restaurants (Ginger and Soy, Sazon, Panera, JBs, and Cafe,
                     in this specific image). The user had 24 swipes at Ginger
                     and Soy, 15 at Sazon, 10 at Panera, 9 at JBs, and 6 at
                     Cafe.

                     The second bar chart in the upper right hand corner is
                     titled 'Total Food Points Spent per Restaurant' and it
                     displays the total number of food points spent at each of
                     the top five restaurants (Ginger and Soy, Sazon, JBs,
                     Panera, and Pitchforks, in this specific image). The user
                     spent approximately $255 at Ginger and Soy, $176 at Sazon,
                     $111 at JBs, $108 at Panera, and $57 at Pitchforks.

                     The third bar chart on the bottom is titled 'Average Food
                     Points Spent per Transaction at Restaurant' and it displays
                     the average number of food points spent per transaction at
                     each of the top five restaurants (JBs, Panera, Ginger and
                     Soy, Il Forno, and Cafe, in this specific image). Per
                     transaction, the user spent approximately $12 at JBs, $11
                     at Panera, $11 at Ginger and Soy, $10 at Il Forno, and $9
                     at Cafe."
                 ),
                 h3("Discussion:"),
                 p(
                   "In conclusion, this R Shiny app helps visualize Duke
                   students’ food point spending, helping our peers set and
                   accomplish their spending-related goals for the semester,
                   whatever these may be. Furthermore, we have included a set of
                   food point tips in order to provide students with guidance on
                   how they could amend their spending habits. We have provided
                   a list of tips for both the over and under spender.
                   Screenshots of these tips can be found below."
                 ),
                 img(
                   src = "underspend.png", height = 450, width = 650,
                   style = "display: block; margin-left: auto; margin-right: auto;"),
                 br(),
                 img(
                   src = "overspend.png", height = 450, width = 600,
                   style = "display: block; margin-left: auto; margin-right: auto;"),
                 h4("Limitations"),
                 p(
                   "It is important to note that there are limitations to the
                   project. For example, the R Shiny app takes a noticeable time
                   to load the visualizations after the user’s data is imputed.
                   This is because there is a substantial amount of data
                   cleaning and wrangling that is conducted in the Shiny app.
                   Ideally, to improve dashboard performance, we would have a
                   separate R script for data cleaning, but because the user is
                   uploading their own data into the Shiny app, the data
                   cleaning process must be contained within the app's reactive
                   functionality. Further, the colors used in the bar plots and
                   x-axis labels on the Top 5 Restaurants tab are not color
                   blind friendly. This is for aesthetic purposes, as, when
                   possible, each bar’s color corresponds to the primary color
                   on the restaurant’s logo. Fortunately, the differentiating
                   colors are not necessary to interpret the bar plots.
                   Moreover, if users are unable to read the restaurant logos,
                   such as Ginger and Soy’s, which is a bright yellow color, the
                   table is placed at the bottom of the tab to allow for easy
                   reading of restaurant names and values. The app is also
                   currently only functional for the 2021-2022 school year,
                   because past and future food point usage charts are not
                   available on Duke's website. This limits users’ ability to
                   look at their food point data from past years and track how
                   their spending patterns have changed, or stayed the same,
                   throughout their time at Duke. The app is also limited by the
                   eAccounts data which is available to users. As a result,
                   detailed information about what foods were bought at specific
                   dining locations are not accessible, making us unable to
                   comment on unnecessary food point usage, such as excessive
                   spending on coffee or snacks."
                 ),
                 h4("Future Directions"),
                 p(
                   "In addition, some potentially useful plots were not included
                   in the final app. We may consider adding these in the future.
                   For instance, eAccounts provides data about the time of
                   purchase, and while but we did not use this information in
                   the dashboard, it may be beneficial to users to break down
                   food point spending by meal time/type. However, college
                   students eat meals at varied times, which makes it difficult
                   to set cutoffs for what qualifies as breakfast, lunch, and
                   dinner. Lastly, and most critically, the greatest limiting
                   factor of the app is the data upload process. As is, users
                   must spend a potentially excessive period of time copying and
                   pasting data into their downloaded Excel template. This R
                   Shiny app could be extended by connecting it to a Duke netID
                   login portal, such that users would simply need to provide
                   their login information, and the app would auto-fill their
                   food points information. Depending on Duke students’ interest
                   in the interactive dashboard, next steps may include
                   contacting Duke administration to see if they would be
                   interested in deploying a beta version of the application
                   with the login portal functionality for our peers. This would
                   help everyone be able to more easily access a report, helping
                   them to accomplish their personal food point spending
                   goals."
                 ),
                 h3("Code"),
                 p(
                   "Code for the Shiny application can be accessed here: ",
                   a(
                     "UI",
                     href = "https://github.com/vizdata-f21/project-2-out_of_the_boxplots/blob/main/dashboard/ui.R"
                   ),
                   "and",
                   a(
                     "Server",
                     href = "https://github.com/vizdata-f21/project-2-out_of_the_boxplots/blob/main/dashboard/server.R"),
                   "."
                 )
          )
        )
      ),
      ## Food Point Tips Tab ---------------------------------------------------
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
      ## Top 5 Restaurants Tab -------------------------------------------------
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