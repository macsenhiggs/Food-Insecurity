library(tidyverse)
library(shadowtext)



datajoined <- datajoined |>
  mutate(
    CensusRegion = factor(CensusRegion, levels = c("Midwest", "Northeast", "South", "West"))
  )


NSLP_per_capita <- datajoined |>
  select(State, StateAbbrev, `2023NSLP`, `2023NSLP_Lunches`, `2023EstPopulation`, CensusRegion, `2020Under18`) |>
  mutate(
    NSLP_prop = `2023NSLP` / `2023EstPopulation`,
    NSLP_lunches_prop = `2023NSLP_Lunches` / `2023EstPopulation`,
    `2020Under18` = `2020Under18` / 100
  ) |>
  rename(youth_prop_2020 = `2020Under18`)


box1 <- ggplot(NSLP_per_capita, aes(x = CensusRegion, y = NSLP_prop, fill = CensusRegion)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of NSLP\nEnrollment Proportion\nby Census Region",
    y = "School Lunch Enrollment Proportion (2023)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

print(box1)
ggsave("EDA/plots/box1.png", box1, width = 3.67, height = 3.5)

box2 <- ggplot(NSLP_per_capita, aes(x = CensusRegion, y = youth_prop_2020, fill = CensusRegion)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Youth Proportion of\nPopulation by Census Region",
    y = "Youth Population Proportion (2020)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

print(box2)
ggsave("EDA/plots/box2.png", box2, width = 3.67, height = 3.5)

scatter1 <- ggplot(NSLP_per_capita, aes(x = youth_prop_2020, y = NSLP_prop, label = StateAbbrev)) +
  geom_point(aes(size = `2023EstPopulation`, color = CensusRegion)) +
  geom_shadowtext(vjust = 0, nudge_y = 0.001) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  #scale_color_brewer(palette = "Dark2") +
  labs(
    y = "School Lunch Enrollment Proportion (2023)",
    x = "Youth Proportion (2020)",
    size = "2023 Population",
    title = "School Lunch Enrollment Rate by Proportion of Population Under 18"
  ) +
  scale_fill_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")) +
  theme_minimal() +
  theme(
    #legend.position = "none"
  )

print(scatter1)
ggsave("EDA/plots/scatter1.png", scatter1, width = 9, height = 6)


poverty_vs_lunches <- datajoined |>
  select(State, StateAbbrev, `2023NSLP`, `2023EstPopulation`, CensusRegion, `<18BelowPov`, `23Poverty`) |>
  mutate(
    NSLP_prop = `2023NSLP` / `2023EstPopulation`,
    `<18BelowPov` = `<18BelowPov` / 100,
    `23Poverty` = `23Poverty` / 100
  )


box3 <- ggplot(poverty_vs_lunches, aes(x = CensusRegion, y = `<18BelowPov`, fill = CensusRegion)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Youth Poverty Rate\nby Census Region",
    y = "Youth Poverty Rate (2020)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

print(box3)
ggsave("EDA/plots/box3.png", box3, width = 3.67, height = 3.5)

scatter2 <- ggplot(poverty_vs_lunches, aes(x = `<18BelowPov`, y = NSLP_prop, label = StateAbbrev)) +
  geom_point(aes(size = `2023EstPopulation`, color = CensusRegion)) +
  geom_shadowtext(vjust = 0, nudge_y = 0.001) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  #scale_color_brewer(palette = "Dark2") +
  labs(
    y = "School Lunch Enrollment Proportion (2023)",
    x = "Youth Poverty Rate (2020)",
    size = "2023 Population",
    title = "School Lunch Enrollment Rate by Youth Poverty Rate"
  ) +
  scale_fill_manual(values=c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")) +
  theme_minimal() +
  theme(
    #legend.position = "none"
  )

print(scatter2)
ggsave("EDA/plots/scatter2.png", scatter2, width = 9, height = 6)




