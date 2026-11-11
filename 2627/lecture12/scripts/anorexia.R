rm(list = ls())
library(dplyr)
library(ggplot2)
data("anorexia", package = "MASS")


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

