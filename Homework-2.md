Homework 2
================
Mari Sanders

``` r
library(tidyverse)
library(readxl)
options(scipen = 999)
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
mr_trash_wheel_df <-read_excel("data_hw2/202409_Trash_Wheel_Collection_Data.xlsx",range = "A2:N653", sheet = "Mr. Trash Wheel") %>% 
  janitor::clean_names() %>% 
  select(
    -c("homes_powered")
  ) %>% 
  mutate(
    sports_balls, sports_balls = as.integer(sports_balls)) %>% 
  mutate(trash_wheel = "mr.") %>% 
  mutate(year, year = as.numeric(year))


prof_trash_wheel_df <- 
  read_excel("data_hw2/202409_Trash_Wheel_Collection_Data.xlsx", 
             range = "A2:M121", sheet = "Professor Trash Wheel") %>% 
  janitor::clean_names() %>% 
  select(
    -c( "homes_powered")
  ) %>% 
  mutate(trash_wheel = "professor") %>% 
  mutate(year, year = as.numeric(year))

gwynnda_df <- 
  read_excel("data_hw2/202409_Trash_Wheel_Collection_Data.xlsx", 
             range = "A2:L265", sheet = "Gwynnda Trash Wheel") %>% 
  janitor::clean_names() %>% 
  select(
    -c("homes_powered")) %>% 
  mutate(
    trash_wheel = "gwynnda"
  ) %>% 
  mutate(year,year = as.numeric(year))

trash_wheel_df <- 
  bind_rows(mr_trash_wheel_df, prof_trash_wheel_df,gwynnda_df) %>% 
  relocate(trash_wheel, .after = month)
```

This data contains variables dumpster, month, trash_wheel, year, date,
weight_tons, volume_cubic_yards, plastic_bottles, polystyrene,
cigarette_butts, glass_bottles, plastic_bags, wrappers, sports_balls. To
clean each of the data sets, I used `clean_names()` from the package
`janitor`, then I selected only the variables that I needed, which were
all the columns that contained information about the dumpster-specific
information. Finally, to keep track of which trash wheel the data is
from, I created a variable called `trash_wheel` that contained values
`mr.`, `gwynnda`, or `professor`. There are 1033 observations and 14
variables.

``` r
total_weight_prof <- 
  trash_wheel_df %>% 
  filter(trash_wheel == "professor") %>% 
  pull(weight_tons) %>% 
  sum()
```

The total weight collected by Professor Trash Wheel is NA tons.

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
  janitor::clean_names() %>% 
  separate(baker_name, into = c("first_name", "last_name"), sep = " ")
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
  read_csv("data_hw2/results.csv", skip = 2) %>% 
  janitor::clean_names()
```

    ## Rows: 1136 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): baker, result
    ## dbl (3): series, episode, technical
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
final_data <- baker_df %>% 
  left_join(bakes_df, by = c(first_name = "baker", series = "series")) %>% 
  left_join(results_df, by = c(series = "series", episode = "episode", first_name = "baker")) %>% 
  relocate(series, episode, .before = "first_name")

write_csv(final_data, "ggb_data.csv")
```

The first thing that I did with all of the datasets is I used the
`clean_names()` function from the `janitor` package to make all the
names of the columns easier to work with. For the `baker_df()`, I
separated the `baker_name` into `first_name` and `last_name`, so that it
would be easier to join the data later. To combine each of the data, I
first used `left_join` by `first_name` and `series` because I wanted to
have all of the data from `baker_df` to be included. Then I used
`left_join` to join the `results_df` with the rest of the combined data
by `series`, `episode` and `first_name`. Finally, to make the data more
readable, I relocated `series` and `episode` to be before `first_name`.
This dataset includes the variables series, episode, first_name,
last_name, baker_age, baker_occupation, hometown, signature_bake,
show_stopper, technical, result. It has 11 columns and 566 rows. This
dataset is still a bit messy because in order to show each of the bakers
`show_stopper` and `signature_bake`, the data about their `series`,
`episode`, `first_name`, `last_name`, `baker_age`, `baker_occupation`,
and `hometown` are duplicated for the amount of times they baked a
signature_bake and a show_stopper. I think it would be best to keep the
data seperate and then use the data that you need from both of them,
without combining them.

``` r
final_data %>% 
  filter(series > 5) %>% 
    filter(result == "STAR BAKER" | result == "WINNER") %>% 
  knitr::kable(., col.names = c("Season", "Episode", "First Name", "Last Name", "Age", "Occupation", "Hometown", "Signature Bake", "Show Stopper", "Technical Score", "Result")) 
```

| Season | Episode | First Name | Last Name     | Age | Occupation                                 | Hometown                        | Signature Bake                                                                    | Show Stopper                                                      | Technical Score | Result     |
|-------:|--------:|:-----------|:--------------|----:|:-------------------------------------------|:--------------------------------|:----------------------------------------------------------------------------------|:------------------------------------------------------------------|----------------:|:-----------|
|      7 |       7 | Andrew     | Smyth         |  25 | Aerospace engineer                         | Derby / Holywood, County Down   | Tropical Holiday Roulade                                                          | Childhood Ice Cream Mousse Cakes                                  |               1 | STAR BAKER |
|      7 |       9 | Andrew     | Smyth         |  25 | Aerospace engineer                         | Derby / Holywood, County Down   | Cheesy Elephant Ears and Herby Treble Clefs                                       | Philharmonic Fondants                                             |               2 | STAR BAKER |
|      7 |       4 | Benjamina  | Ebuehi        |  23 | Teaching assistant                         | South London                    | Red Onion Chutney, Brie and Bacon Yorkshire Puddings                              | Tropical Churros                                                  |               1 | STAR BAKER |
|      7 |       2 | Candice    | Brown         |  31 | PE teacher                                 | Barton-Le-Clay, Bedfordshire    | Salted Caramel, Chocolate Iced Shiny Hearts                                       | Gingerbread Pub with Sticky Ginger Carpet                         |               8 | STAR BAKER |
|      7 |       5 | Candice    | Brown         |  31 | PE teacher                                 | Barton-Le-Clay, Bedfordshire    | Danish Pastry Croque Monsieur Kites and Cinnamon Apple, Vanilla Crème Rose Danish | Sausage, Black Pudding and Apple Rounds and Banoffee Whiskey Cups |               2 | STAR BAKER |
|      7 |       8 | Candice    | Brown         |  31 | PE teacher                                 | Barton-Le-Clay, Bedfordshire    | Cheesy Cheeky Fish Pies                                                           | Peacock                                                           |               1 | STAR BAKER |
|      7 |      10 | Candice    | Brown         |  31 | PE teacher                                 | Barton-Le-Clay, Bedfordshire    | Queen Victoria’s Mango and Strawberry Crown                                       | Picnic for Pearly Kings and Queens                                |               2 | WINNER     |
|      6 |       2 | Ian        | Cumming       |  41 | Travel photographer                        | Great Wilbraham, Cambridgeshire | Orange, Rosemary and Almond Biscotti                                              | Sandwich de la Confiture                                          |               3 | STAR BAKER |
|      6 |       3 | Ian        | Cumming       |  41 | Travel photographer                        | Great Wilbraham, Cambridgeshire | Wild Garlic Pesto Soda Breads                                                     | Flour Power                                                       |               1 | STAR BAKER |
|      6 |       4 | Ian        | Cumming       |  41 | Travel photographer                        | Great Wilbraham, Cambridgeshire | Pomegranate Two Ways Crème Brûlées                                                | Trio of Spicy and Herby Baked Cheesecakes                         |               4 | STAR BAKER |
|      7 |       1 | Jane       | Beedle        |  61 | Garden designer                            | Beckenham                       | Lemon and Poppy Seed Drizzle Cake                                                 | Chocolate Orange Mirror Cake                                      |               7 | STAR BAKER |
|      8 |       3 | Julia      | Chernogorova  |  21 | Aviation Broker                            | Crawley, West Sussex            | Earl Grey Dried Fruit Teacakes                                                    | ‘The Snail Under a Mushroom’ Bread Sculpture                      |               2 | STAR BAKER |
|      8 |       4 | Kate       | Lyon          |  29 | Health and safety inspector                | Merseyside                      | Salted Bay Caramel Millionaire Shortbreads                                        | Sticky Toffee Apple Caramel Cake                                  |               6 | STAR BAKER |
|      8 |       6 | Liam       | Charles       |  19 | Student                                    | North London                    | ‘Standard FC’ Decorative Pies                                                     | ‘Nan’s Sunday Dinner’ Pie                                         |               4 | STAR BAKER |
|      6 |       1 | Marie      | Campbell      |  66 | Retired                                    | Auchterarder, Perthshire        | Zingy Citrus Madeira Cake                                                         | A Walk in the Black Forest                                        |               3 | STAR BAKER |
|      6 |       6 | Mat        | Riley         |  37 | Fire fighter                               | London                          | Piña Colada Frangipane Tart                                                       | His ‘n’ Hers Vol-au-vents                                         |               1 | STAR BAKER |
|      6 |       5 | Nadiya     | Hussain       |  30 | Full-time mother                           | Leeds / Luton                   | Naked Blueberry and Caraway Crunch Cake                                           | Chocolate and Strawberry Lime Ice Cream Roll                      |               1 | STAR BAKER |
|      6 |       8 | Nadiya     | Hussain       |  30 | Full-time mother                           | Leeds / Luton                   | Rose Pistachio and Mocha Hazelnut Horns                                           | Bubble Gum and Peppermint Cream Religieuse à l’ancienne           |               1 | STAR BAKER |
|      6 |       9 | Nadiya     | Hussain       |  30 | Full-time mother                           | Leeds / Luton                   | Peanut Salted Caramel and Chocolate Tart                                          | Peacock in Nan’s Door                                             |               4 | STAR BAKER |
|      6 |      10 | Nadiya     | Hussain       |  30 | Full-time mother                           | Leeds / Luton                   | Cardamom and Almond Buns & Nutmeg and Sour Cherry Fingers                         | My Big Fat British Wedding Cake                                   |               1 | WINNER     |
|      8 |       5 | Sophie     | Faldo         |  33 | Former army officer and trainee stuntwoman | West Molesey, Surrey            | Ginger, Fig and Honey Steamed School Pudding                                      | Raspberry, Yuzu & White Chocolate Bûche Trifle Terrine            |               1 | STAR BAKER |
|      8 |       9 | Sophie     | Faldo         |  33 | Former army officer and trainee stuntwoman | West Molesey, Surrey            | Strawberry & Rhubarb and Chestnut & Vanilla Choux Buns                            | ‘Tutu with Opera Filling’ Meringue Centrepiece                    |               1 | STAR BAKER |
|      8 |      10 | Sophie     | Faldo         |  33 | Former army officer and trainee stuntwoman | West Molesey, Surrey            | Spelt Boules, Mushroom Ciabatta and Orange Plaited Brioche                        | ‘Ode to the Honey Bee’ Entremet                                   |               2 | WINNER     |
|      8 |       8 | Stacey     | Hart          |  42 | Former school teacher                      | Radlett, Hertfordshire          | Camembert & Onion and Apple & Blueberry Bedfordshire Clangers                     | ‘Bright’ Lemon & Orange Savoy Cake                                |               3 | STAR BAKER |
|      8 |       1 | Steven     | Carter-Bailey |  34 | Marketer                                   | Watford, Hertfordshire          | Bonfire Night Cake                                                                | ‘A Baker’s Lunch’ Cake                                            |               6 | STAR BAKER |
|      8 |       2 | Steven     | Carter-Bailey |  34 | Marketer                                   | Watford, Hertfordshire          | Amarpressi Biscuits                                                               | ‘Check Bake’ Game                                                 |               6 | STAR BAKER |
|      8 |       7 | Steven     | Carter-Bailey |  34 | Marketer                                   | Watford, Hertfordshire          | Italian Style Cannoli                                                             | ‘Sicilian-style’ Sfogliatelle                                     |               1 | STAR BAKER |
|      6 |       7 | Tamal      | Ray           |  29 | Trainee anaesthetist                       | Manchester                      | Middle Eastern Game Pie                                                           | Spiced Blackberry, Raspberry and Cardamom Charlotte Russe         |               3 | STAR BAKER |
|      7 |       3 | Tom        | Gilliford     |  26 | Project engagement manager                 | Rochdale                        | Chocolate Orange and Chilli Swirl Bread                                           | Jörmungandr and Mjölnir                                           |               4 | STAR BAKER |
|      7 |       6 | Tom        | Gilliford     |  26 | Project engagement manager                 | Rochdale                        | Blood Orange Halloween Pumpkin Pie                                                | Floral Tea Cake                                                   |               1 | STAR BAKER |

Tamal Ray and Benjama Ebuehi seem to be surprise winners because they
only won once, but ended up being the star-baker. Nadiya Hussain either
got star-baker or won 4 times, so they seem like a predictable winner.
Sophie Faldo and Steve Carter-Bailey both got star-baker or won 3 times.

``` r
viewership_df <- 
  read_csv("data_hw2/viewers.csv") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    cols = series_1:series_10,
    names_to = "season", 
    names_prefix = "series_"
  ) %>% 
  mutate(season, season = as.numeric(season)) %>% 
  arrange(season) %>% 
  relocate(season, .before = episode) 
```

    ## Rows: 10 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (11): Episode, Series 1, Series 2, Series 3, Series 4, Series 5, Series ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(viewership_df, 10)
```

    ## # A tibble: 10 × 3
    ##    season episode value
    ##     <dbl>   <dbl> <dbl>
    ##  1      1       1  2.24
    ##  2      1       2  3   
    ##  3      1       3  3   
    ##  4      1       4  2.6 
    ##  5      1       5  3.03
    ##  6      1       6  2.75
    ##  7      1       7 NA   
    ##  8      1       8 NA   
    ##  9      1       9 NA   
    ## 10      1      10 NA

``` r
avg_view_1 <- 
  viewership_df %>% 
  filter(season == 1) %>% 
  summarise(avg = mean(value, na.rm = TRUE))  %>% 
  pull()

avg_view_5 <- 
  viewership_df %>% 
  filter(season == 5) %>% 
  summarise(avg = mean(value, na.rm = TRUE))  %>% 
  pull()
```

The average viewership in Season 1 was 2.77. The average viewership in
Season 5 was 10.0393.
