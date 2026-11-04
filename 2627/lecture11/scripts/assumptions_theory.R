rm(list = ls())
library(ggplot2)
library(fs)
library(datasauRus)
library(dplyr)
out_dir <- "lecture11/figures"
save_figs <- function(..., width = 25, height = 25, units = "cm", .base) {
  if (...length() == 1L) {
    ggsave(...elt(1), file = .base,
           width = width, height = height, units = units)
  } else
    for (i in seq_len(...length()))
      ggsave(...elt(i), file = sprintf(.base, i),
             width = width, height = height, units = units)
}

plt_dino <- datasaurus_dozen |> 
  filter(dataset == "dino") |> 
  ggplot(mapping = aes(x, y)) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick2") +
  geom_point(shape = 21, fill = "grey70") +
  theme_bw(base_size = 20)
save_figs(plt_dino, .base = fs::path(out_dir, "dino", ext = "svg"))


set.seed(42)
n <- 100
x <- runif(n, 0, 10)
e <- rnorm(n, sd = 0.7)

dat <- data.frame(
  x = rep(x, 4),
  y = c(
    2 + 0.6*x + e,
    2 + 0.23*(x-5)^2 + e,
    2 + 5*(1-exp(-x/2)) + e,
    2 + ifelse(x < 5, 0.2, 1)*x + .75*e
  ),
  relation = factor(
    rep(c("Linear", "U-shaped", "Plateau", "Threshold"), each = n),
    levels = c("Linear", "U-shaped", "Plateau", "Threshold")
  )
)

ggplot(dat, aes(x, y)) +
  geom_point(shape = 21, fill = "grey70") +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick2") +
  facet_wrap(~relation) +
  theme_bw(base_size = 20)


# library(ggplot2)

set.seed(42)
n <- 100
x <- runif(n, 0, 10)
e <- rnorm(n, sd = 0.7)

fun_list <- list(
  "Linear"              = function(x) 2 + 0.6 * x,
  "U-shaped"            = function(x) 8 -  0.23 * (x - 5)^2,
  "Plateau"             = function(x) 2 + 5 * (1 - exp(-x / 2)),
  "Threshold"    = function(x) 2 + 0.2 * x + 1.2 * pmax(0, x - 5)
)

dat <- do.call(
  rbind,
  lapply(names(fun_list), function(name) {
    mu <- fun_list[[name]](x)
    data.frame(
      x = x,
      mu = mu,
      y = mu + e,
      relation = name
    )
  })
)

dat$relation <- factor(dat$relation, levels = names(fun_list))
dat_line <- dat[order(dat$relation, dat$x), ]

ggplot(dat, aes(x, y)) +
  geom_point(shape = 21, fill = "grey70", color = "grey40") +
  geom_line(data = dat_line, aes(y = mu), color = "dodgerblue3", linewidth = 1.2) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick2", linewidth = 1.2) +
  facet_wrap(~relation) +
  theme_bw(base_size = 20)

nms <- names(fun_list)
xlabs <- c("Hours worked", "Dopamine", "Hours studied", "Swimming lessons")
ylabs <- c("Income", "Working memory performance", "Exam performance", "Distance swum")
figs <- sapply(seq_along(nms), \(i) {
  nm <- nms[i]
  extraline <- if (nm == "Linear") {
    NULL
  } else {
    geom_line(data = dat_line |> filter(relation == nm), aes(y = mu), color = "dodgerblue3", linewidth = 1.2)
  }
  ggplot(dat |> filter(relation == nm), aes(x, y)) +
    geom_point(shape = 21, fill = "grey70", color = "grey40") +
    extraline +
    geom_smooth(method = "lm", se = FALSE, color = "firebrick2", linewidth = 1.2) +
    facet_wrap(~relation) +
    labs(x = xlabs[i], y = ylabs[i]) +
    theme_bw(base_size = 28)
})

for (i in seq_along(names(fun_list)))
  save_figs(figs[[i]], .base = fs::path(out_dir, names(fun_list)[i], ext = "svg"),
            width = 40)

