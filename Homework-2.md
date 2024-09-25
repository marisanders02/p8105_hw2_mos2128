Homework 2
================
Mari Sanders
2024-09-24

``` r
library(tidyverse)
library(readxl)
```

# Problem 1

``` r
transit_df <- read_csv("data_hw2/nyc_subway_data.csv") %>% 
  janitor::clean_names() %>% 
  select(
    -c("division", "entrance_type", "exit_only", "staffing", "staff_hours",
       "ada_notes", "free_crossover", "north_south_street", "east_west_street", 
       "corner", "entrance_latitude", "entrance_longitude", "station_location", 
       "entrance_location")
  ) %>% 
  mutate(
    entry = case_match(
      entry, 
      "YES" ~ TRUE, 
      "NO" ~ FALSE
    )
  )
```

    ## Rows: 1868 Columns: 32
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (22): Division, Line, Station Name, Route1, Route2, Route3, Route4, Rout...
    ## dbl  (8): Station Latitude, Station Longitude, Route8, Route9, Route10, Rout...
    ## lgl  (2): ADA, Free Crossover
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Problem 2

``` r
mr_trash_wheel_df <- read_excel("data_hw2/202309_Trash_Wheel_Collection_Data.xlsx", 
                                range = "A2:N586", sheet = "Mr. Trash Wheel") %>% 
  janitor::clean_names() %>% 
  select(
    -c("dumpster","month", "year", "date", "homes_powered")
  ) %>% 
  mutate(
    sports_balls, sports_balls = as.integer(sports_balls)) %>% 
  mutate(trash_wheel = "Mr.")


prof_trash_wheel_df <- 
  read_excel("data_hw2/202309_Trash_Wheel_Collection_Data.xlsx", 
             range = "A2:M108", sheet = "Professor Trash Wheel") %>% 
  janitor::clean_names() %>% 
  select(
    -c("dumpster","month", "year", "date", "homes_powered")
  ) %>% 
  mutate(trash_wheel = "Professor")

gwynnda_df <- 
  read_excel("data_hw2/202309_Trash_Wheel_Collection_Data.xlsx", 
             range = "A2:L157", sheet = "Gwynnda Trash Wheel") %>% 
  janitor::clean_names() %>% 
  select(
    -c("dumpster","month", "year", "date", "homes_powered")) %>% 
  mutate(
    trash_wheel = "Gwynnda"
  )

trash_wheel_df <- 
  bind_rows(mr_trash_wheel_df, prof_trash_wheel_df,gwynnda_df) %>% 
  relocate(trash_wheel, .before = weight_tons)
```

## Problem 3

``` r
baker_df <- 
  read_csv("data_hw2/bakers.csv") %>% 
  janitor::clean_names()
```

    ## Rows: 120 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Baker Name, Baker Occupation, Hometown
    ## dbl (2): Series, Baker Age
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
bakes_df <- 
  read_csv("data_hw2/bakes.csv") %>%
  janitor::clean_names()
```

    ## Rows: 548 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Baker, Signature Bake, Show Stopper
    ## dbl (2): Series, Episode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
results_df <- 
  read_csv("data_hw2/viewers.csv") %>% 
  janitor::clean_names() 
```

    ## Rows: 10 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (11): Episode, Series 1, Series 2, Series 3, Series 4, Series 5, Series ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
