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



df_2022 <- df |>
  filter(year == 2022) |>
  select(geo, name, variable, value) |>
  pivot_wider(
    names_from = variable,
    values_from = value
  )

saveRDS(object = df_2022, file = file.path(data_dir, "gap2022.rds"))

d <- df_2022
d <- readRDS("lecture3/scripts/data/gap2022.rds")
set.seed(42)
nms <- c("Afghanistan", "Angola", "Albania",
         "Belgium", "Denmark", "France", "Germany",
         "USA", "UK" , "Spain", "Italy", "Brazil", "China",
         "Japan", "Netherlands", "Poland", "Austria", "Czech Republic", "Switzerland",
         "Canada")
rand_names_opts <- setdiff(df$name, nms)
rand_names <- c(sample(rand_names_opts, 10), nms)
d |> filter(name %in% rand_names)

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


toDT <- function(dat, scroll = TRUE, rownames = FALSE) {
  
  numeric_cols <- if (is.data.frame(dat))
    names(dat)[sapply(dat, is.numeric)]
  else if (is.matrix(dat)) {
    which(apply(dat, 2, is.numeric))
  }
  
  DT::datatable(
    dat, options = list(searching = FALSE, info = FALSE, paging = FALSE, scrollX = scroll, scrollY = "350px", scrollCollapse = TRUE,autoWidth = TRUE
    ), rownames = rownames) |>
    DT::formatRound(numeric_cols, digits = 2)
}
d |> select(-geo, -name) |> cov(use = "pairwise.complete.obs") |> as.data.frame() |> toDT(rownames = TRUE)
d |> select(-geo, -name) |> cor(use = "pairwise.complete.obs") |> as.data.frame() |> toDT(rownames = TRUE)

d |> select(name, life_expectancy, fertility) |> 
  mutate(
    life_expectancy_m0 = life_expectancy - mean(life_expectancy, na.rm = TRUE),
    fertility_m0 = fertility - mean(fertility, na.rm = TRUE),
    SP = fertility_m0 * life_expectancy_m0
  ) |> 
  head(10) |> 
  kableExtra::kbl(escape = FALSE, align = rep('l', length(df[,1])), format = "html") %>%
  kableExtra::kable_styling("striped")

with(d, (life_expectancy - mean(life_expectancy, na.rm = TRUE)) * (fertility - mean(fertility, na.rm = TRUE)))
sum(d$life_expectancy - mean(life_expectancy, na.rm = TRUE)) * (fertility - mean(fertility, na.rm = TRUE))
sum(d$life_expectancy, df_2022$fertility)

plot(df_2022$life_expectancy, df_2022$fertility)

plot(df_2022$gdp_pcap, df_2022$life_expectancy)
plot(df_2022$gdp_pcap, df_2022$life_expectancy)
plot(log(df_2022$gdp_pcap), df_2022$life_expectancy)
hist(log(df_2022$gdp_pcap))
cor(log(df_2022$gdp_pcap), df_2022$life_expectancy, use = "pairwise")
cor(df_2022$gdp_pcap, df_2022$life_expectancy, use = "pairwise")

d |>
  select(gdp_pcap)|>
  head(5) |>
  mutate(rank_gd = rank(gdp_pcap)) |>
  toDT()
d$gdp_pcap
rank(d$gdp_pcap)

ms <- c("pearson", "kendall", "spearman")
mm <- matrix(NA, 3, 2, dimnames = list(ms, c("GDP", "Log(GDP)")))
for (i in seq_along(ms)) {
  mm[i, 1] <- cor(d$gdp_pcap, d$life_expectancy, use = "pairwise.complete.obs", method = ms[i])
  mm[i, 2] <- cor(log(d$gdp_pcap), d$life_expectancy, use = "pairwise.complete.obs", method = ms[i])
}
mm

format(cor(d$gdp_pcap, d$life_expectancy, use = "pairwise.complete.obs", method = "pearson"), digits = 3)
format(cor(d$gdp_pcap, d$life_expectancy, use = "pairwise.complete.obs"), digits = 3)

g1 <- ggplot(d, aes(x = child_mortality, y = fertility)) + 
  geom_point(size = 3, shape = 21, fill = "grey", alpha = .7) + 
  labs(x = "Child mortality", y = "Fertility") +
  theme_bw(base_size = 24) 
g2 <- ggplot(d, aes(x = child_mortality, y = life_expectancy)) + 
  geom_point(size = 3, shape = 21, fill = "grey", alpha = .7) + 
  labs(x = "Child mortality", y = "Life expectancy") +
  theme_bw(base_size = 24) 
g3 <- ggplot(d, aes(x = fertility, y = life_expectancy)) + 
  geom_point(size = 3, shape = 21, fill = "grey", alpha = .7) + 
  labs(x = "Fertility", y = "Life expectancy") +
  theme_bw(base_size = 24) 
library(patchwork)
g123 <- g1 + g2 + g3
ggsave(filename = "lecture3/images/wooclap_q2.png", g123, width = 21, height = 7)


x <- seq(-2, 2, .01)
fns <- list(
  exp = exp,
  power = \(x) 10^x,
  log = log,
  log10 = log10,
  reciprocal = \(x) 1 / x,
  root = sqrt
)
nms <- names(fns)
y <- setNames(nm = nms, lapply(nms, \(nm) fns[[nm]](x)))
lens <- lengths(y)
tib <- tibble(
  x = rep(x, length(y)), 
  y = unlist(y, use.names = FALSE),
  transformation = rep(nms, lens)
)
ggplot(tib, aes(x = x, y = y, color = transformation)) + 
  geom_line() + 
  facet_wrap(~transformation, scales = "free") +
  theme_bw(base_size = 20)

