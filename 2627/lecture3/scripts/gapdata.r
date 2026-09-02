rm(list = ls())
# source: https://www.gapminder.org/data/
# Data can be reused freely but please attribute the original data source (where applicable) and Gapminder.
# Example: FREE DATA FROM WORLD BANK VIA GAPMINDER.ORG, CC-BY LICENSE
library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)

data_dir <- "lecture3/scripts/data"
files <- list.files(data_dir, pattern = ".csv", full.names = TRUE)

variable_nms <- c(
  "child_mortality",
  "fertility",
  "co2_pcap",
  "gdp_pcap",
  "life_expectancy",
  "daily_income"
)

ds <- lapply(files, read.csv)
ds2 <- lapply(ds, \(d) tidyr::pivot_longer(d, cols = X1800:last_col(), names_to = "year"))
for (i in seq_along(ds2)) ds2[[i]]$variable <- variable_nms[[i]]

df <- bind_rows(ds2)
df$year <- as.numeric(gsub("X", "", df$year))
summary(df)
df |> group_by(variable) |> summarise(
  mean = mean(value, na.rm = TRUE), 
  sd = sd(value, na.rm = TRUE), 
  q25 = quantile(value, .25, na.rm = TRUE), 
  q75 = quantile(value, .75, na.rm = TRUE),
  min_year = min(year),
  max_year = max(year)
)
summary(df)

library(dplyr)
library(tidyr)

df_2022 <- df |>
  filter(year == 2022) |>
  select(geo, name, variable, value) |>
  pivot_wider(
    names_from = variable,
    values_from = value
  )

df_2022 |>
  summarise(
    across(
      -c(geo, name),
      ~ sum(!is.na(.))
    )
  )
summary(df_2022)
df_2022 |>
  select(
    child_mortality,
    co2_pcap,
    daily_income,
    fertility,
    gdp_pcap,
    life_expectancy
  ) |>
  cor(use = "pairwise.complete.obs")

df |>
  filter(year >= 2015, year <= 2022) |>
  group_by(year, variable) |>
  summarise(
    n = sum(!is.na(value)),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = variable,
    values_from = n
  )

plot(df_2022$life_expectancy, df_2022$fertility)
plot(df_2022$gdp_pcap, df_2022$life_expectancy)
plot(df_2022$gdp_pcap, df_2022$life_expectancy)
plot(log(df_2022$gdp_pcap), df_2022$life_expectancy)
hist(log(df_2022$gdp_pcap))
cor(log(df_2022$gdp_pcap), df_2022$life_expectancy, use = "pairwise")
cor(df_2022$gdp_pcap, df_2022$life_expectancy, use = "pairwise")
