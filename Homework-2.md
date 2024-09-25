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
transit_df <- read_csv("data/nyc_subway_data.csv") %>% janitor::clean_names() %>% 
  select(
    c("line", "station_name", "station_latitude", "station_longitude", "route1", "route2", "route3", "route4", "route5", "route6", "route7", "route8", "route9", "route10", "route11", "entry", "vending", "entrance_type", "ada")
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
