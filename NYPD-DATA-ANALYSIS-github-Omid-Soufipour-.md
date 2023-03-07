NYC Shooting Incident Data (2006-2022)
================
Omid Soufipour
2023-03-03

## Introduction

I conducted a data analysis of shooting reports in New York City for a
course on the Master of Science in Data Science program at the
**University of Colorado Boulder**. To complete the project, I utilized
publicly available data from the [City of NewYork
website](https://data.cityofnewyork.us/Public-Safety/%20NYPD-Shooting-Incident-Data-Historic-/833y-fsy8).
The purpose of this analysis was to gain insights into the frequency and
location of shootings in the city.

## Libraries

I used only the **“tidyverse”** library for analyzing data and drawing
plots. The library **“lubridate”** is also in the same package, but it
needed to call seperately.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0     ✔ purrr   1.0.1
    ## ✔ tibble  3.1.8     ✔ dplyr   1.1.0
    ## ✔ tidyr   1.3.0     ✔ stringr 1.5.0
    ## ✔ readr   2.1.3     ✔ forcats 1.0.0
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

## Data Analysis

For the purpose of data analysis, I performed the following steps:

#### 1. Reading the data:

I retrieved the data from the website mentioned earlier and saved it in
a file named “nyc_shootings”.

``` r
url <- "https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD"
nyc_shootings <- read_csv(url)
```

    ## Rows: 25596 Columns: 19
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (10): OCCUR_DATE, BORO, LOCATION_DESC, PERP_AGE_GROUP, PERP_SEX, PERP_R...
    ## dbl   (7): INCIDENT_KEY, PRECINCT, JURISDICTION_CODE, X_COORD_CD, Y_COORD_CD...
    ## lgl   (1): STATISTICAL_MURDER_FLAG
    ## time  (1): OCCUR_TIME
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

#### 2- Checking the data:

I need to examine the data to determine the type of analysis and any
necessary modifications.

``` r
glimpse(nyc_shootings)
```

    ## Rows: 25,596
    ## Columns: 19
    ## $ INCIDENT_KEY            <dbl> 236168668, 231008085, 230717903, 237712309, 22…
    ## $ OCCUR_DATE              <chr> "11/11/2021", "07/16/2021", "07/11/2021", "12/…
    ## $ OCCUR_TIME              <time> 15:04:00, 22:05:00, 01:09:00, 13:42:00, 20:00…
    ## $ BORO                    <chr> "BROOKLYN", "BROOKLYN", "BROOKLYN", "BROOKLYN"…
    ## $ PRECINCT                <dbl> 79, 72, 79, 81, 113, 113, 42, 52, 34, 75, 32, …
    ## $ JURISDICTION_CODE       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0…
    ## $ LOCATION_DESC           <chr> NA, NA, NA, NA, NA, NA, "COMMERCIAL BLDG", NA,…
    ## $ STATISTICAL_MURDER_FLAG <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,…
    ## $ PERP_AGE_GROUP          <chr> NA, "45-64", "<18", NA, NA, NA, NA, NA, NA, "2…
    ## $ PERP_SEX                <chr> NA, "M", "M", NA, NA, NA, NA, NA, NA, "M", "M"…
    ## $ PERP_RACE               <chr> NA, "ASIAN / PACIFIC ISLANDER", "BLACK", NA, N…
    ## $ VIC_AGE_GROUP           <chr> "18-24", "25-44", "25-44", "25-44", "25-44", "…
    ## $ VIC_SEX                 <chr> "M", "M", "M", "M", "M", "M", "M", "M", "M", "…
    ## $ VIC_RACE                <chr> "BLACK", "ASIAN / PACIFIC ISLANDER", "BLACK", …
    ## $ X_COORD_CD              <dbl> 996313, 981845, 996546, 1001139, 1050710, 1051…
    ## $ Y_COORD_CD              <dbl> 187499, 171118, 187436, 192775, 184826, 196646…
    ## $ Latitude                <dbl> 40.68132, 40.63636, 40.68114, 40.69579, 40.673…
    ## $ Longitude               <dbl> -73.95651, -74.00867, -73.95567, -73.93910, -7…
    ## $ Lon_Lat                 <chr> "POINT (-73.95650899099996 40.68131820000008)"…

#### 3- Removing unwanted columns:

After reviewing the columns, I decided to delete the following columns:

``` r
nyc_shootings <- nyc_shootings %>% 
  select(-LOCATION_DESC, -X_COORD_CD, -Y_COORD_CD, -Latitude,
         -Longitude, -Lon_Lat, -JURISDICTION_CODE, -PRECINCT)
```

#### 4- Renaming some columns:

It’s a good idea to rename the column names to names that are more
suitable for R programming. If you think it’s the easy part, just read
this quote from Phil Karlton:  
”*There are only two hard things in Computer Science: cache invalidation
and naming things.*”

``` r
nyc_shootings <- nyc_shootings %>% 
  rename(date = OCCUR_DATE, time = OCCUR_TIME, borough = BORO,
         murder = STATISTICAL_MURDER_FLAG, 
         incident_key = INCIDENT_KEY, perpetrator_age = PERP_AGE_GROUP,
         perpetrator_sex = PERP_SEX, 
         perpetrator_race = PERP_RACE, victim_age = VIC_AGE_GROUP,
         victim_sex = VIC_SEX, victim_race = VIC_RACE)
```

#### 5- Other changes to the data frame:

Now is a good opportunity to make additional modifications to the
dataframe. First, I will update the “incident_key” column since the
current data appears to be meaningless. My plan is to sort the data
based on the date and assign a unique ID, starting from 1, to each
incident.  
Before proceeding with this task, I will convert the date format to a
standard format.

``` r
nyc_shootings <- nyc_shootings %>% 
  mutate(date=mdy(date)) %>%
  arrange(time) %>% 
  arrange(date)
```

Next, I added a id column that uses the date to create a meaningful
identifier, and removed the incident_key column.

``` r
nyc_shootings <- nyc_shootings %>%
  mutate(id = dense_rank(incident_key))


unique_incident_key <- distinct(nyc_shootings,
  incident_key, .keep_all = TRUE)$incident_key

nyc_shootings$id <- match(nyc_shootings$incident_key, unique_incident_key)

nyc_shootings <- nyc_shootings %>% 
  select(-incident_key)
```

I added two columns to the dataset - the first column indicating the
weekday, and the second column indicating the time of day.

``` r
nyc_shootings <- nyc_shootings %>% 
  mutate(weekday = weekdays(date),
         time_of_day = case_when(hour(time) < 6 ~ "midnight",
                                 hour(time) < 12 ~ "morning",
                                 hour(time) < 18 ~ "afternoon",
                                 hour(time) < 24 ~ "evening"))
```

I added the current population data (as of 2022) using the data from the
[citypopulation
website](http://www.citypopulation.de/en/usa/newyorkcity/)

``` r
nyc_shootings$population <- NA

nyc_shootings$population[nyc_shootings$borough == "BRONX"] <- 1424948
nyc_shootings$population[nyc_shootings$borough == "BROOKLYN"] <- 2641052
nyc_shootings$population[nyc_shootings$borough == "MANHATTAN"] <- 1576876
nyc_shootings$population[nyc_shootings$borough == "QUEENS"] <- 2331143
nyc_shootings$population[nyc_shootings$borough == "STATEN ISLAND"] <- 493494
```

We have many empty cells, as well as a significant number of cells
labeled ‘U’ or ‘UNKNOWN.’ Since there are more of these than we can
ignore, I will replace all of them with the label ‘unknown’.

``` r
nyc_shootings[, !(names(nyc_shootings) %in% c("date", "time"))] <- lapply(nyc_shootings[, !(names(nyc_shootings) %in% c("date", "time"))], function(x) ifelse(is.na(x), "unknown", x))
nyc_shootings[, !(names(nyc_shootings) %in% c("date", "time"))] <- replace(nyc_shootings[, !(names(nyc_shootings) %in% c("date", "time"))], nyc_shootings[, !(names(nyc_shootings) %in% c("date", "time"))] == "U", "unknown")
nyc_shootings[, !(names(nyc_shootings) %in% c("date", "time"))] <- replace(nyc_shootings[, !(names(nyc_shootings) %in% c("date", "time"))], nyc_shootings[, !(names(nyc_shootings) %in% c("date", "time"))] == "UNKNOWN", "unknown")
```

I rearranged the order of the columns and removed the “time” column.

``` r
nyc_shootings <- nyc_shootings %>%
  select(id, everything()) %>% 
  select(-time)
```

## Plots

I am beginning to create some plots. However, it would be beneficial to
have a table displaying the number of shootings per day beforehand.

``` r
shootings_per_day <- nyc_shootings %>%
  count(date) %>%
  arrange(desc(n))

shootings_per_day
```

    ## # A tibble: 5,409 × 2
    ##    date           n
    ##    <date>     <int>
    ##  1 2020-07-05    47
    ##  2 2011-09-04    31
    ##  3 2020-07-26    29
    ##  4 2007-08-11    26
    ##  5 2006-09-04    25
    ##  6 2020-08-15    24
    ##  7 2010-07-11    23
    ##  8 2010-08-07    23
    ##  9 2007-10-06    22
    ## 10 2008-05-24    22
    ## # … with 5,399 more rows

What a day July 5th, 2020 was! Unbelievable! There were 47 shootings in
just one day!  
Let’s perform some more analysis. I am interested in determining which
day of the week and time of day had the highest number of shootings. I
would like to create a plot to visualize this information.

``` r
nyc_shootings %>%
  group_by(weekday, time_of_day) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = factor(weekday, levels = c("Monday", "Tuesday",
          "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
  y = count, fill = factor(time_of_day, levels = c("midnight","morning","afternoon","evening")))) +
  geom_col(position = "dodge") +
  labs(x = "Day of the Week", y = "Number of Shootings", fill = "Time of Day") +
  scale_fill_manual(values = c("morning" = "lightblue", "afternoon" = "orange", 
                               "evening" = "darkred", "midnight" = "black")) +
  theme_bw()+labs(title = "1- Number of shootings per weekday and time of day")
```

    ## `summarise()` has grouped output by 'weekday'. You can override using the
    ## `.groups` argument.

![](NYPD-DATA-ANALYSIS-github-Omid-Soufipour-_files/figure-gfm/plot1-1.png)<!-- -->

chart of number of shootings per season:

``` r
nyc_shootings %>%
  mutate(season = ifelse(month(date) %in% c(12, 1, 2), "Winter",
                         ifelse(month(date) %in% c(3, 4, 5), "Spring",
                                ifelse(month(date) %in% c(6, 7, 8), "Summer", "Fall")))) %>%
  group_by(season) %>%
  summarise(shootings = n()) %>%
  ggplot(aes(x = factor(season, levels=c("Spring", "Summer", "Fall", "Winter")), y = shootings, fill = season)) +
  geom_col() +
  labs(x = "Season", y = "Number of shootings", title = "2- Number of shootings per season") +
  theme_bw()
```

![](NYPD-DATA-ANALYSIS-github-Omid-Soufipour-_files/figure-gfm/plot2-1.png)<!-- -->

I want to compare the number of shootings across different boroughs in
New York City.

``` r
NYPD_shooting_with_pop <- nyc_shootings %>%
  group_by(borough) %>%
  summarize(total_population = max(population),
            incidents_count = n(),
            percentage_of_shootings = incidents_count*1000/total_population)

NYPD_shooting_with_pop%>%
  ggplot(aes(x = borough, y = percentage_of_shootings)) +
  geom_bar(stat = "identity") +
  labs(x = "Location", y = "Number of shootings per 1000 people ",
       title = "3- Comparing NYC boroughs based on shootings")
```

![](NYPD-DATA-ANALYSIS-github-Omid-Soufipour-_files/figure-gfm/plot3-1.png)<!-- -->

Now, I would like to create a table that displays the number of murders
per day in descending order. As you may recall, the highest number of
shootings in a single day occurred on July 5, 2020. However, according
to the table below, this date ranks second in terms of the number of
murders.

``` r
nyc_shootings %>%
  filter(murder == TRUE) %>%
  group_by(date) %>%
  summarise(murders = n()) %>%
  arrange(desc(murders))
```

    ## # A tibble: 2,704 × 2
    ##    date       murders
    ##    <date>       <int>
    ##  1 2020-07-26      12
    ##  2 2020-07-05      11
    ##  3 2011-12-12      10
    ##  4 2007-10-06       9
    ##  5 2007-11-18       9
    ##  6 2010-07-11       9
    ##  7 2018-01-06       9
    ##  8 2006-09-04       8
    ##  9 2008-10-27       8
    ## 10 2010-02-22       8
    ## # … with 2,694 more rows

I want to create a plot that shows the number of shootings by gender.

``` r
ggplot(nyc_shootings, aes(x = perpetrator_sex, fill = victim_sex)) +
  geom_bar(position = "dodge") +
  labs(title = "4- Shooting by Perpetrator Sex and Victim Sex",
       x = "Perpetrator Sex", y = "number of shootings") +
  theme_minimal()
```

![](NYPD-DATA-ANALYSIS-github-Omid-Soufipour-_files/figure-gfm/shooting_per_sex-1.png)<!-- -->

I want to create a plot that shows the number of shootings by age group:

``` r
ggplot(nyc_shootings, aes(x = perpetrator_age, fill = victim_age)) +
  geom_bar(position = "dodge") +
  labs(title = "5- Shooting by Perpetrator Age and Victim Age",
       x = "Perpetrator Age", y = "number of shootings") +
  theme_minimal()
```

![](NYPD-DATA-ANALYSIS-github-Omid-Soufipour-_files/figure-gfm/shooting_per_age-1.png)<!-- -->

If anyone is interested, they can use my previous code to conduct the
same analysis on race. I excluded this analysis to prevent my project
from becoming overly lengthy.

## Define a Model

As the final step of my project, I aim to define a model that
demonstrates the correlation between the number of shootings and murders
per year.

``` r
shootings_by_year <- nyc_shootings %>%
  group_by(year = lubridate::year(date)) %>%
  summarize(shootings = n()) %>%
  ungroup()

murders_by_year <- nyc_shootings %>%
  filter(murder) %>%
  group_by(year = lubridate::year(date)) %>%
  summarize(murders = n()) %>%
  ungroup()

shootings_and_murders_by_year <- shootings_by_year %>%
  left_join(murders_by_year, by = "year") %>%
  mutate(murders = ifelse(is.na(murders), 0, murders))

lm_model <- lm(murders ~ shootings, data = shootings_and_murders_by_year)
summary(lm_model)
```

    ## 
    ## Call:
    ## lm(formula = murders ~ shootings, data = shootings_and_murders_by_year)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -45.047 -17.980  -0.395  15.950  39.750 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -33.73553   27.45209  -1.229    0.239    
    ## shootings     0.21362    0.01667  12.817    4e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 26.16 on 14 degrees of freedom
    ## Multiple R-squared:  0.9215, Adjusted R-squared:  0.9159 
    ## F-statistic: 164.3 on 1 and 14 DF,  p-value: 3.996e-09

and here is the plot of this simple model:

``` r
shootings_and_murders_by_year %>%
  ggplot(aes(x = shootings, y = murders)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Shootings per Year", y = "Murders per Year",
       title = "Linear Model between Shootings and Murders in NYC") +
  theme_bw()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](NYPD-DATA-ANALYSIS-github-Omid-Soufipour-_files/figure-gfm/model_plot-1.png)<!-- -->

## Conclusion and potential biases

Based on the project and the plots I have created, the following
conclusions can be drawn:

1.  Most shootings occurred from Friday evening until Sunday morning,
    indicating that they were not with prior intention and happened
    spontaneously.  
2.  The plot showing the number of shootings per season confirms my
    earlier finding that most shootings were not premeditated. Summer
    had the highest number of shootings, which could be attributed to
    people going out to nightclubs and other recreational activities on
    weekends. This finding contradicts my initial assumption that crime
    groups were responsible for most shootings, as it appears that
    ordinary people are also involved in such incidents. However, it’s
    important to acknowledge the possibility of biases in my analysis,
    and further research is needed to confirm these findings.  
3.  The available age group and sex data do not allow for precise
    conclusions as there is a significant amount of unknown data.
    However, based on the available data, it can be said that the
    majority of both perpetrators and victims were male and in the 25-44
    age group.
