Homework 2
================
Mari Sanders

``` r
library(tidyverse)
library(readxl)
options(scipen=999)
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

The data contains the variables line, station_name, station_latitude,
station_longitude, route1, route2, route3, route4, route5, route6,
route7, route8, route9, route10, route11, entry, vending, ada. To clean
the data, I started by using the `clean_names()` function from the
`janitor` package. Then I selected only the rows that were needed for
the analysis by using `select()`. Next, I changed the entry variable to
a logical variable instead of a character variable. This is fairly tidy,
except the routes that each station serve it a bit hard to understand.
This cleaned data contains 1868 rows and 18 columns.

``` r
distinct_station <- 
  transit_df %>%
  distinct(line, station_name) %>%
  nrow()
```

There are 465 distinct stations in this dataset.

``` r
ada_compliant <- transit_df %>% 
  filter(ada == "TRUE") %>% 
  distinct(line, station_name) %>% 
  nrow()
```

There are 84 ada compliant stations.

``` r
clean_transit <- transit_df %>% 
  mutate(
    across(starts_with("route"), 
           as.character)
  ) %>% 
  pivot_longer(
    cols = route1:route11,
    names_to = "route_number",
    values_to = "route_name"
  ) 
```

``` r
a_stations <- clean_transit %>% 
  filter(route_name == "A") %>%  
  distinct(line, station_name) %>%  
  nrow()
```

There are 60 distinct stations that serve the A train

``` r
a_ada_stations <- 
  clean_transit %>% 
  filter(route_name == "A" & ada == "TRUE") %>%  
  distinct(line, station_name) %>%  
  nrow()
```

There are 17 stations that serve the a train and are ada accessible.

# Problem 2

``` r
mr_trash_wheel_df <- read_excel("data_hw2/202309_Trash_Wheel_Collection_Data.xlsx", 
                                range = "A2:N586", sheet = "Mr. Trash Wheel") %>% 
  janitor::clean_names() %>% 
  select(
    -c("homes_powered")
  ) %>% 
  mutate(
    sports_balls, sports_balls = as.integer(sports_balls)) %>% 
  mutate(trash_wheel = "mr.") %>% 
  mutate(year, year = as.numeric(year))


prof_trash_wheel_df <- 
  read_excel("data_hw2/202309_Trash_Wheel_Collection_Data.xlsx", 
             range = "A2:M108", sheet = "Professor Trash Wheel") %>% 
  janitor::clean_names() %>% 
  select(
    -c( "homes_powered")
  ) %>% 
  mutate(trash_wheel = "professor") %>% 
  mutate(year, year = as.numeric(year))

gwynnda_df <- 
  read_excel("data_hw2/202309_Trash_Wheel_Collection_Data.xlsx", 
             range = "A2:L157", sheet = "Gwynnda Trash Wheel") %>% 
  janitor::clean_names() %>% 
  select(
    -c("homes_powered")) %>% 
  mutate(
    trash_wheel = "gwynnda"
  ) %>% 
  mutate(year,year = as.numeric(year))

trash_wheel_df <- 
  bind_rows(mr_trash_wheel_df, prof_trash_wheel_df,gwynnda_df) %>% 
  relocate(trash_wheel, .before = month)
```

This data contains variables dumpster, trash_wheel, month, year, date,
weight_tons, volume_cubic_yards, plastic_bottles, polystyrene,
cigarette_butts, glass_bottles, plastic_bags, wrappers, sports_balls. To
clean each of the data sets, I used `clean_names()` from the package
`janitor`, then I selected only the variables that I needed, which were
all the columns that contained information about the dumpster-specific
information. Finally, to keep track of which trash wheel the data is
from, I created a variable called `trash_wheel` that contained values
`mr.`, `gwynnda`, or `professor`. There are 845 observations and 14
variables.

``` r
total_weight_prof <- 
  trash_wheel_df %>% 
  filter(trash_wheel == "professor") %>% 
  pull(weight_tons) %>% 
  sum()
```

The total weight collected by Professor Trash Wheel is 216.26 tons.

``` r
cig_butts_gwynnda <- 
  trash_wheel_df %>% 
  filter(trash_wheel == "gwynnda") %>% 
  filter(year == 2022) %>% 
  filter(month == "June") %>% 
  pull(cigarette_butts) %>% 
  sum()
```

Gwynnda collected 18120 cigarette butts in June 2022.

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
