library(readxl)
library(tidyverse)
library(janitor)

data2023 <- read_excel("Food_Insecurity.xlsx", 
                              sheet = "Data2023") |>
  rename(State = StateName) |>
  filter(if_any(everything(), ~ !is.na(.)))

states <- as.list(data2023$State)

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


datajoined <- data2023 |>
  left_join(population, by = "State") |>
  left_join(poverty, by = "State") |>
  left_join(race, by = "State")



datamod <- datajoined |>
  mutate(
    `2023SNAPPROP` = `2023SNAPParticipation` / `2023EstPopulation`
  )

ggplot(datamod, aes(x = `2023SNAPPROP`)) +
  geom_histogram() +
  theme_minimal()



