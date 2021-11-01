```{r load-data, echo = FALSE}
library(tidyverse)
semester <- read_csv("data/semester.csv")
usage_chart <- read_csv("data/usage_chart.csv")
```
# Food Points Data: Semester Data Set (semester.csv)

Each observation is an offered Duke undergraduate food plan. The accompanying 
variables are information about the total number of points offered in the plan, 
the expected number of points spent more week, and the expected daily average to 
be spent. The data was collected from 
[Duke's Food Plan and Points Website](https://studentaffairs.duke.edu/dining/plans-points).

|variable         |description                           |type |
|:----------------|:-------------------------------------|-----|
|plan             |Type of food plan (A - J)             |chr  |
|total_value      |Total number of food points included in plan|dbl  |
|weekly_average   |Expected number of food points to be spent per week |dbl  |
|daily_average    |Expected number of food points to be spent per day  |dbl  |

# Food Points Data: Usage Chart Data Set (usage_chart.csv)

Each observation is a week during a Duke undergraduate semester. The 
accompanying variables are information about the start date for the week in 
the Fall or Spring semester, and the expected number of food points to start 
that week with, depending on which plan a student has. The data was collected from 
the Fall 2021 and Spring 2022 Food Points Calculators found on the 
[Duke's Food Plan and Points Website](https://studentaffairs.duke.edu/dining/plans-points).

|variable         |description                           |type |
|:----------------|:-------------------------------------|-----|
|fall             |Start date of each week in the Fall 2021 semester |chr  |
|spring           |Start date of each week in the Spring 2022 semester |chr  |
|plan_a           |Expected number of food points to start a week with, under Plan A |dbl  |
|plan_b           |Expected number of food points to start a week with, under Plan B |dbl  |
|plan_c           |Expected number of food points to start a week with, under Plan C |dbl  |
|plan_d           |Expected number of food points to start a week with, under Plan D |dbl  |
|plan_e           |Expected number of food points to start a week with, under Plan E |dbl  |
|plan_f           |Expected number of food points to start a week with, under Plan F |dbl  |
|plan_i           |Expected number of food points to start a week with, under Plan I |dbl  |
|plan_j           |Expected number of food points to start a week with, under Plan J |dbl  |
