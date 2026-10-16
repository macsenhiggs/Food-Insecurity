library(readxl)
library(tidyverse)
library(janitor)

data2020 <- read_excel("Food_Insecurity.xlsx", 
                       sheet = "Data2020") |>
  rename(State = StateName) |>
  filter(
    if_any(everything(), ~ !is.na(.)),
    State != "United States"
    )

states <- as.list(data2020$State)


poverty <- read_excel("Food_Insecurity.xlsx", 
                      sheet = "Poverty") |>
  filter(
    if_any(everything(), ~ !is.na(.)),
    State %in% states
  )

race <- read_excel("Food_Insecurity.xlsx", 
                   sheet = "Race") |>
  filter(
    if_any(everything(), ~ !is.na(.)),
    State %in% states
  )

population <- read_excel("Food_Insecurity.xlsx", 
                         sheet = "USPop") |>
  rename(State = NAME) |>
  filter(
    if_any(everything(), ~ !is.na(.)),
    State %in% states
  )


datajoined <- data2020 |>
  left_join(population, by = "State") |>
  left_join(poverty, by = "State") |>
  left_join(race, by = "State") |>
  select(-matches("202[1-4]|23"))

datarenamed <- datajoined |>
  #clean numerals
  clean_names(numerals = "right", use_make_names = FALSE) |>
  #move numerics to end of column names and remove 2021-2024 data
  rename_with(
    ~ str_replace(., "^(202[0-4])(.+)", "\\2_\\1"),
    matches("^202[0-4]|")
  ) |>
  #remove age group poverty columns
  select(-matches("below_pov|blw_pov"), -est_population_2020)


# TODO: convert all raw numeric columns to rate metrics by dividing by population
# raw numerical column
cols_to_scale_1 <- c("snap_participation_2020", "births_2020", "deaths_2020", "internationalmig_2020", "domesticmig_2020")
# rate columns stored as XX%
cols_to_scale_2 <- c("unemployment_rate_2020", "obese_2020", "poverty_rate_2020", "white_2020", "non_white_2020", "under_18_2020")

datarate <- datarenamed |>
  mutate(
    # convert raw numerical values to rate
    across(all_of(cols_to_scale_1), ~ .x / popestimate_2020),
    across(contains("nslp"), ~ .x / popestimate_2020),
    
    # convert existing rates from XX% to 0.XX
    across(all_of(cols_to_scale_2), ~ .x / 100),
  )


datascaled <- datarate |>
  mutate(
    across(where(is.numeric), ~ as.numeric(scale(.x)))
  )









