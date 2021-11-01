Project Proposal
================
OutOfTheBoxplots

    ## Warning in system("timedatectl", intern = TRUE): running command 'timedatectl'
    ## had status 1

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.0.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    ## [1] "/home/guest/STA313/project-2-out_of_the_boxplots/proposal"

    ## Rows: 8 Columns: 4

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): plan
    ## dbl (3): total_value, weekly_avereage, daily_average

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Rows: 18 Columns: 10

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): fall, spring
    ## dbl (8): plan_a, plan_b, plan_c, plan_d, plan_e, plan_f, plan_i, plan_j

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Summary

Build a Shiny dashboard app that takes Duke students’ food plan data and
generates a report on their usage.

## Project Goals

Every semester Duke offers the same various food plans to meet the
diverse needs of their undergraduate students. These customized plans
are tailored specifically for upper-class, first-year and off-campus
students, yet most students regardless of their food plan face the same
issue — they always run out of food points by the end of the semester.
The dreaded feeling of bargaining for your friend’s spare food points is
something all Duke students have felt at one point in time, and this is
the motivation for a pertinent issue we would like to explore. With all
the options Duke offers, why is it so hard to budget our food points,
and what can help us budget better? In order to alleviate the mental
hurdles that come with deciding how much money we can spend on food, we
have decided to create a R Shiny dashboard app that uses a Duke
student’s food plan and data from their DukeCard Account Transaction
Report to generate a report on their spending. To do this we will
utilize Duke’s Fall 2021 and Spring 2022 Food Points Usage Chart. This
chart breaks down all 8 of Duke’s food plans and the suggested usage
rate based on daily and weekly averages. We plan to use this in
combination with a student’s personal usage data, which includes the
location food points were spent at — accessed through their [DukeCard
eAccounts login](https://dukecard.duke.edu/manage-your-dukecard) — to
provide a comprehensive report on their food spending. Students will
copy and paste this personal information into an Excel template we
provide, to be uploaded to the R Shiny app. We hope that this tool can
provide students with the necessary insight on how much they can spend
on food and where they should or shouldn’t spend their food, while
staying within the limits of their food plan.

The first dataset we are using offers a brief summary of each plan,
which will allow us to quickly outline each plan’s offerings. Each
observation is an offered Duke undergraduate food plan. The accompanying
variables are information about the total number of points offered on
the plan, the expected number of points to be spent per week, and the
expected number of points to be spent per day. There are 8 observations
and 4 variables in the data set. The data was collected from [Duke’s
Food Plan and Points
Website](https://studentaffairs.duke.edu/dining/plans-points). The
second dataset we are using offers in depth recommended spending
information for each plan, which will allow us to offer recommendations
to Duke students. Each observation is a week during a Duke undergraduate
semester. The accompanying variables are information about the start
date for the week in the Fall or Spring semester, and the expected
number of food points to start that week with, depending on which plan a
student has. There are 18 observations and 10 variables in the data set.
The data was collected from the Fall 2021 and Spring 2022 Food Points
Calculators found on [Duke’s Food Plan and Points
Website](https://studentaffairs.duke.edu/dining/plans-points). A
comprehensive codebook for each data set can be found in the README file
inside the data folder in this project’s GitHub repository.

## Plan of Attack

Week 3 (starting Nov 1): Complete Project Proposal - Finalize Project
Repo, include code book and all necessary data to configure R Shiny app
(Anna) - Create plan of attack for the project and figure out roles
(Blossom) - Write up 1-2 paragraph description of our goals for project
proposal (Hebron) - Start testing R Shiny app dashboard and assist Anna
with finalizing repo (Matthew)

Week 4 (starting Nov 8): Update + Finalize Project Proposal - Create
List of visualizations we want to see within app (EVERYONE) - Consider
food points data over time, location, factor in student breaks/holidays,
potential finals season - Add Sample Dataset (Matthew) - Create Food
Points Excel Template (Anna) - Update Project proposal based on peer
feedback (Hebron) - Start implementing visualization ideas (EVERYONE)

Week 5-6 (starting Nov 15, starting Nov 22): Work on Data Visualizations
- Create visualizations for data with focus over time (consider
semester, student breaks, holidays, finals season) - (Anna, Matthew) -
Create visualizations for data with focus by location (i.e. explore
custom geospatial possibilities, bar charts) - (Blossom, Hebron)

Week 7 (starting Nov 29): Project Finishing Touches - Write Introduction
(EVERYONE) - Write Conclusion (EVERYONE) - Contribute to Project
write-up (EVERYONE) - Practice Presentation (EVERYONE) - Finalize
Website and Presentation (EVERYONE)

## Repository Organization
