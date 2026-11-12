rm(list = ls())
library(dplyr)
library(ggplot2)
library(fs)

fragment_kable <- function(obj, steps, digits = 2, ...) {
  stopifnot(length(steps) == ncol(obj))

  html <- knitr::kable(
    obj, format = "html", digits = digits,
    row.names = FALSE, ...
  )
  doc <- xml2::read_html(as.character(html))
  table <- xml2::xml_find_first(doc, "//table")

  for (j in which(!is.na(steps))) {
    cells <- xml2::xml_find_all(
      table,
      sprintf("./thead/tr/th[%d] | ./tbody/tr/td[%d]", j, j)
    )
    xml2::xml_set_attr(cells, "class", "fragment")
    xml2::xml_set_attr(
      cells, "data-fragment-index", as.character(steps[j])
    )
  }

  knitr::asis_output(as.character(table))
}
out_dir_fig <- path("lecture12", "figures")
out_dir_tb  <- path("lecture12", "tables")
data("anorexia", package = "MASS")
data("birthwt", package = "MASS")



summary(anorexia)
anorexia <- anorexia |>
  filter(Treat != "CBT") |>
  mutate(
    delta = Postwt - Prewt
  )

ggplot(anorexia, aes(x = Treat, y = delta)) +
  geom_point() +
  labs(x = "Treatment", y = "Postweight - Preweight")

anorexia |>
  group_by(Treat) |>
  summarise(mean = mean(delta), se = ) |>
  ggplot(aes(x = Treat, y = mean)) +
  geom_col() +
  geom_errorbar()
  labs(x = "Treatment", y = "mean(Postweight - Preweight)")

ggplot(anorexia, aes(x = Treat, y = delta)) +
  geom_boxplot(fill = NA)

ggplot(anorexia, aes(x = Treat, y = delta)) +
  ggbeeswarm::geom_quasirandom(width = 0.1) +
  labs(x = "Treatment", y = "Postweight - Preweight")

welch <- t.test(delta ~ Treat, data = anorexia)
studt <- t.test(delta ~ Treat, data = anorexia, var.equal = TRUE)

set_theme(
  theme_bw(base_size = 24) +
  theme(geom = element_geom(pointshape = 21, fill = "grey"))
)

GeomPoint$default_aes
GeomBoxplot$default_aes

ggplot(anorexia, aes(x = Treat, y = delta)) +
  geom_boxplot(fill = NA) +
  geom_point() +
  labs(x = "Treatment", y = "Postweight - Preweight")

ggplot(anorexia, aes(x = Treat, y = delta)) +
  geom_boxplot(fill = NA) +
  ggbeeswarm::geom_quasirandom(dodge.width = 0.5) +
  labs(x = "Treatment", y = "Postweight - Preweight")

tapply(anorexia$delta, anorexia$Treat, mean)

anorexia |>
  group_by(Treat) |>
  summarise(mean = mean(delta)) |>
  ggplot(aes(x = Treat, y = mean)) +
  geom_col() +
  labs(x = "Treatment", y = "mean(Postweight - Preweight)")


nrow(murrar)

# Boxplot explanation ----
# s <- boxplot.stats(df$x)$stats
# q1  <- s[2]
# med <- s[3]
# q3  <- s[4]
q1  <- unname(quantile(df$x, 0.25))
med <- median(df$x)
q3  <- unname(quantile(df$x, 0.75))

boxplot_explanation <- ggplot(df, aes(y = x)) +
  geom_boxplot(fill = NA, orientation = "x", width = 0.35) +
  # Median
  annotate("segment", x = 0.55, xend = 0.25, y = med, yend = med, arrow = arrow(length = grid::unit(0.2, "cm"))) +
  annotate("text", x = 0.58, y = med, label = "Median", hjust = 0) +
  annotate("segment", x = 0.55, xend = 0.25, y = q1, yend = q1, arrow = arrow(length = grid::unit(0.2, "cm"))) +
  annotate("text", x = 0.58, y = q1, label = "Q1 / 25% quantile", hjust = 0) +
  annotate("segment", x = 0.55, xend = 0.25, y = q3, yend = q3, arrow = arrow(length = grid::unit(0.2, "cm"))) +
  annotate("text", x = 0.58, y = q3, label = "Q3 / 75% quantile", hjust = 0) +
  # Interquartile range
  annotate("segment", x = -0.25, xend = -0.25, y = q1, yend = q3, arrow = arrow(ends = "both", length = grid::unit(0.15, "cm"))) +
  annotate("text", x = -0.5, y = (q1 + q3) / 2, label = "IQR (middle 50%)", hjust = 1) +
  # Outliers
  annotate("segment", x = 0.55, xend = 0.05, y = 3, yend = 3, arrow = arrow(length = grid::unit(0.2, "cm"))) +
  annotate("text", x = 0.58, y = 3, label = "Outlier", hjust = 0) +
  scale_x_continuous(breaks = NULL, limits = c(-1, 1)) +
  labs(y = NULL, x = NULL) +
  theme_bw(base_size = 18)

save_figs(boxplot_explanation, width = 40, .base = path(out_dir_fig, "boxplot_expl", ext = "svg"))

# murrar ----

murrar <- experimentr::murrar |>
  mutate(Prejudice = arab_t1 - arab_t0, Cast = factor(condition, levels = c(0, 1), labels = c("all-white", "diverse"))) |>
  filter(!is.na(Prejudice))
g0 <- ggplot(murrar, aes(x = Cast, y = Prejudice)) + geom_point() + ggtitle("Actual data")
g0a <-ggplot(murrar |> mutate(Cast = factor(Cast, labels = c("0\nall-white", "1\ndiverse"))),
             aes(x = Cast, y = Prejudice)) + geom_point() + ggtitle("Actual data") + theme(axis.text.x = element_text(color = "darkred", face = "bold"))

g1 <- ggplot(murrar, aes(x = Cast, y = Prejudice)) + ggbeeswarm::geom_beeswarm(width = 0.2) + ggtitle("Add fake spread to x-coordinates")
g2 <- ggplot(murrar, aes(x = Cast, y = Prejudice)) + geom_boxplot(fill = NA) + ggtitle("Box plot")

g3 <- murrar |>
  group_by(Cast) |>
  summarise(mean = mean(Prejudice), se = sqrt(sd(Prejudice) / length(Prejudice))) |>
  ggplot(aes(x = Cast, y = mean)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = mean - qnorm(0.025) * se, ymax = mean + qnorm(0.025) * se), width = .3) +
  labs(y = "Prejudice", title = "Bar graph with 95% CI")

murrar_means <- murrar |>
  group_by(Cast) |>
  summarise(mean = mean(Prejudice), se = sqrt(sd(Prejudice) / length(Prejudice)))
g3b <- murrar_means |>
  ggplot(aes(x = Cast, y = mean)) +
  geom_errorbar(aes(ymin = mean - qnorm(0.025) * se, ymax = mean + qnorm(0.025) * se), width = .3) +
  geom_point(color = "black") +
  labs(y = "Prejudice", title = "Bar graph with 95% CI")

g0123 <- wrap_plots(g0, g1, g2, g3, ncol = 2, nrow = 2, axes = "collect")

save_figs(g0, g0a, g1, g2, g3, g3b, g0123, width = 40, .base = path(out_dir_fig, "murray_raw_%d", ext = "svg"))

murrar |> select(Cast, Prejudice) |> mutate(Dummy_Cast = ifelse(Cast == "diverse", 1, 0)) |> head(n = 5) |>
  fragment_kable(steps = c(NA, NA, 2)) |>
  writeLines(path(out_dir_tb, "murrar-table.qmd"))

fit <- lm(Prejudice ~ Cast, murrar)
fit_a <- coef(fit)[1]
fit_b <- coef(fit)[2]
coef(fit)
murrar_means
fitted.values(fit)

murrar2 <- murrar |> mutate(Cast = factor(Cast, labels = c("all-white (0)", "diverse (1)")))
murrar_means2 <- murrar_means  |> mutate(Cast = factor(Cast, labels = c("all-white (0)", "diverse (1)")))
mean_points <- geom_point(data = murrar_means2, aes(x = Cast, y = mean), shape = 23, fill = "dodgerblue", size = 8, inherit.aes = FALSE)
g_base0 <-
  ggplot(murrar2, aes(x = Cast, y = Prejudice)) +
  ggbeeswarm::geom_beeswarm(dodge.width = 0.45, alpha = .7) +
  ggtitle("Add fake spread to x-coordinates")
g_base1 <- g_base0 +
  mean_points +
  ggtitle("Blue diamonds are sample means")
g_base2 <- g_base0 +
  geom_abline(intercept = fit_a-fit_b, slope = fit_b, color = regressioncol) +
  mean_points +
  ggtitle("Red line is still the regression line")

g_base0
g_base1
g_base2
save_figs(g_base0, g_base1, g_base2, width = 40, .base = path(out_dir_fig, "scatter_%d", ext = "svg"))

t.test(delta ~ condition, murrar)
car::leveneTest(delta ~ condition, data = murrar, center = mean)

data("births", package = "openintro")
dim(births)


ggplot(births, aes(x = smoke, y = weight)) +
  geom_point() +
  labs(x = "Smoke", y = "Birth weight")

births |>
  group_by(smoke) |>
  summarise(mean = mean(weight), se = sqrt(sd(weight) / length(weight))) |>
  ggplot(aes(x = smoke, y = mean)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = mean - qnorm(0.025) * se, ymax = mean + qnorm(0.025) * se), width = .3) +
  labs(x = NULL, y = "Birthweight")

ggplot(anorexia, aes(x = Treat, y = delta)) +
  geom_boxplot(fill = NA)

ggplot(anorexia, aes(x = Treat, y = delta)) +
  ggbeeswarm::geom_quasirandom(width = 0.1) +
  labs(x = "Treatment", y = "Postweight - Preweight")

births$m_age
births <- births |>
  mutate(
    old_mom = factor(m_age >= 35, labels = c("old", "young")),
    old_dad = factor(f_age >= 35, labels = c("old", "young"))
  )

hist(births$m_age, breaks = 20)
hist(births$f_age)
summary((births$f_age))
with(births, plot(m_age, f_age))
abline(0, 1)

# age dad
t.test(weight ~ old_dad, data = births, var.equal = TRUE)
t.test(weight ~ old_dad, data = births, var.equal = FALSE)

# age mom
t.test(weight ~ old_mom, data = births, var.equal = TRUE)
t.test(weight ~ old_mom, data = births, var.equal = FALSE)

# sex_baby
t.test(weight ~ sex_baby, data = births, var.equal = TRUE)
t.test(weight ~ sex_baby, data = births, var.equal = FALSE)

# Maternal smoking
t.test(weight ~ smoke, data = births, var.equal = TRUE)
t.test(weight ~ smoke, data = births, var.equal = FALSE)

# Prematurity
t.test(weight ~ premature, data = births, var.equal = FALSE)
t.test(weight ~ premature, data = births, var.equal = TRUE)

# Homogeneity tests (mean-centered Levene)
car::leveneTest(weight ~ sex_baby,  data = births, center = mean)
car::leveneTest(weight ~ smoke,     data = births, center = mean)
car::leveneTest(weight ~ premature, data = births, center = mean)
car::leveneTest(weight ~ old_mom,   data = births, center = mean)
