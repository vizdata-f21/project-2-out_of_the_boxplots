Project Proposal
================
OutOfTheBoxplots

## High-Level Goal

The goal of this project is to build an R Shiny dashboard app that takes
Duke students’ food plan data and generates a report on their usage.

## Project Goals

Every semester Duke offers the same various food plans to meet the
diverse needs of their undergraduate students. These customized plans
are tailored specifically for upper-class, first-year and off-campus
students, yet most students, regardless of their food plan, face the
same issue — they always run out of food points by the end of the
semester. The dreaded feeling of bargaining for your friend’s spare food
points is something all Duke students have felt at one point in time,
and this is the motivation for a pertinent issue we would like to
explore. With all the options Duke offers, why is it so hard to budget
our food points, and what can help us budget better? In order to
alleviate the mental hurdles that come with deciding how much money we
can spend on food, we have decided to create a R Shiny dashboard app
that uses a Duke student’s food plan and data from their DukeCard
Account Transaction Report to generate a visualreport on their spending.
To do this we will utilize Duke’s Fall 2021 and Spring 2022 Food Points
Usage Chart. This chart breaks down all 8 of Duke’s food plans and the
suggested usage rate based on daily and weekly averages. We plan to use
this in combination with a student’s personal usage data, which includes
the location food points were spent at — accessed through their
[DukeCard eAccounts
login](https://dukecard.duke.edu/manage-your-dukecard) — to provide a
comprehensive report on their food spending. Students will generate a
usage report from their online DukeCard statements. Students will then
copy and paste this personal information into an Excel template we
provide, to be uploaded to the R Shiny app. This Excel template can be
found in our data folder which looks like the following:

‘input\_food\_points\_data.csv’:

    ## Rows: 0
    ## Columns: 6
    ## $ `Date/Time`        <chr> 
    ## $ `Account Name`     <chr> 
    ## $ `Card Number`      <chr> 
    ## $ Location           <chr> 
    ## $ `Transaction Type` <chr> 
    ## $ Amount             <chr>

We consulted Duke Dining on the best way to extract food point data and
they believe our method is most efficient.

Students will then be provided information about how their spending
compares to Duke’s recommended spending by utilizing a trendline that
shows their spending rate for each week as the semester goes on, as well
as other visualizations which communicate metrics about the times and
eatery locations in which they spend the most food points. We hope that
this tool can provide students with the necessary insight on how much
they can spend on food and where they should or shouldn’t spend their
food, while staying within the limits of their food plan.

The first dataset we are using, `semester.csv`, offers a brief summary
of each plan, which will allow us to quickly outline each plan’s
offerings. Each observation is an offered Duke undergraduate food plan.
The accompanying variables are information about the total number of
points offered on the plan, the expected number of points to be spent
per week, and the expected number of points to be spent per day. There
are 8 observations and 4 variables in the data set. The data was
collected from [Duke’s Food Plan and Points
Website](https://studentaffairs.duke.edu/dining/plans-points). The
second dataset we are using, `usage_chart.csv`, offers in depth
recommended spending information for each plan, which will allow us to
offer recommendations to Duke students. Each observation is a week
during a Duke undergraduate semester. The accompanying variables are
information about the start date for the week in the Fall or Spring
semester, and the expected number of food points to start that week
with, depending on which plan a student has. There are 18 observations
and 10 variables in the data set. The data was collected from the Fall
2021 and Spring 2022 Food Points Calculators found on [Duke’s Food Plan
and Points
Website](https://studentaffairs.duke.edu/dining/plans-points). A
comprehensive code book for each data set can be found in the README
file inside the data folder in this project’s GitHub repository. A
glimpse of each data set is also provided below:

`semester.csv`:

    ## Rows: 8
    ## Columns: 4
    ## $ plan            <chr> "A", "B", "C", "D", "E", "F", "I", "J"
    ## $ total_value     <dbl> 2570.33, 3080.95, 3412.05, 3661.45, 3992.55, 841.73, 8…
    ## $ weekly_avereage <dbl> 165.07, 197.86, 219.12, 235.14, 256.40, 54.06, 56.89, …
    ## $ daily_average   <dbl> 23.58, 28.27, 31.30, 33.59, 36.63, 7.72, 8.13, 16.68

`usage_chart.csv`:

    ## Rows: 18
    ## Columns: 10
    ## $ fall   <chr> "8/16/21", "8/23/21", "8/30/21", "9/6/21", "9/13/21", "9/20/21"…
    ## $ spring <chr> "1/3/21", "1/10/21", "1/17/21", "1/24/21", "1/31/21", "2/7/21",…
    ## $ plan_a <dbl> 2570.33, 2452.42, 2287.36, 2122.29, 1957.22, 1792.16, 1627.09, …
    ## $ plan_b <dbl> 3080.95, 2939.62, 2741.76, 2543.90, 2346.04, 2148.19, 1950.33, …
    ## $ plan_c <dbl> 3412.05, 3255.53, 3036.41, 2817.29, 2598.17, 2379.04, 2159.92, …
    ## $ plan_d <dbl> 3661.45, 3493.49, 3258.35, 3023.22, 2788.08, 2552.94, 2317.80, …
    ## $ plan_e <dbl> 3992.55, 3809.41, 3553.00, 3296.60, 3040.20, 2783.80, 2527.39, …
    ## $ plan_f <dbl> 841.73, 803.12, 749.06, 695.01, 640.95, 586.89, 532.84, 494.23,…
    ## $ plan_i <dbl> 885.80, 845.17, 788.28, 731.39, 674.51, 617.62, 560.74, 520.10,…
    ## $ plan_j <dbl> 1817.83, 1734.44, 1617.70, 1500.96, 1384.22, 1267.48, 1150.74, …

## Weekly Plan of Attack

*Week 3 (starting Nov 1): Complete Project Proposal*

-   Finalize Project Repo, include code book and all necessary data to
    configure R Shiny app (Anna)
-   Create plan of attack for the project and figure out roles (Blossom)
-   Write up 1-2 paragraph description of our goals for project proposal
    (Hebron)
-   Start testing R Shiny app dashboard and assist Anna with finalizing
    repo (Matthew)

*Week 4 (starting Nov 8): Update + Finalize Project Proposal*

-   Create list of visualizations we want to see within app (Everyone)
    -   Consider food points data over time, location, factor in student
        breaks/holidays, potential finals season
-   Add Sample Dataset (Matthew)
-   Create Food Points Excel Template (Anna)
-   Update project proposal based on peer feedback (Hebron)
-   Start implementing visualization ideas (Everyone)

*Week 5-6 (starting Nov 15, starting Nov 22): Work on Data
Visualizations*

-   Create visualizations for data with focus over time (consider
    semester, student breaks, holidays, finals season) (Anna, Matthew)
-   Create visualizations for data with focus by location (i.e. explore
    custom geospatial possibilities, bar charts) (Blossom, Hebron)

*Week 7 (starting Nov 29): Project Finishing Touches*

-   Write Introduction (Everyone)
-   Write Conclusion (Everyone)
-   Contribute to Project write-up (Everyone)
-   Practice Presentation (Everyone)
-   Finalize Website and Presentation (Everyone)

## Organization of Project Repository

For our project repository organization we will have folders for: the
proposal, the data, the dashboard, the presentation, and images. The
dashboard folder will contain the script to create our final
deliverable: the dashboard. The proposal folder will contain the rmd and
md files for our proposal write up. Accompanying images that will be
included in the dashboard will be placed in the images folder. The
presentation folder will contain the code for a xaringan slideshow (if
we choose to present using this format). The data folder will contain
the csv files with information about Duke’s food plans, as well as
possibly the Excel template that users of the dashboard can download (we
will decide on the best location for this template later on, so it’s
exact location in the project repository is yet to be determined). Each
folder contains a README file explaining in further detail the folder’s
contents, as well as a README file in the main folder, describing the
overall project. This overall README file will likely contain the
finalized write-up, too, and the accompanying rmd file will be placed in
the main folder.
