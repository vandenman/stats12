rm(list = ls())
library(ggplot2)
library(dplyr)
library(tibble)
out_dir <- "lecture10/figures"
if (!dir.exists(out_dir)) dir.create(out_dir)

save_figs <- function(..., width = 25, height = 25, units = "cm", .base) {
  if (...length() == 1L) {
    ggsave(...elt(1), file = .base,
           width = width, height = height, units = units)
  } else
    for (i in seq_len(...length()))
      ggsave(...elt(i), file = sprintf(.base, i),
             width = width, height = height, units = units)
}

regressioncol <- "firebrick2"

n <- 50
intercept <- 0
sd_x   <- 1
sd_err <- 1
slope <- .25


coef_reps <- matrix(NA, 1000, 2)
for (i in seq_len(nrow(coef_reps))) {
x <- rnorm(n, 0, sd_x) # is fixed?
  e <- rnorm(n, 0, sd_err)
  y <- intercept + x * slope + e
  coef_reps[i, ] <- unname(coef(lm(y ~ x)))
}

hist(coef_reps[, 1])
qqnorm(coef_reps[, 1]); qqline(coef_reps[, 1])

set.seed(42)
nsim <- 8
tib <- tibble()
  x <- rnorm(n, 0, sd_x) # is fixed?
for (i in seq_len(nsim)) {
  e <- rnorm(n, 0, sd_err)
  y <- intercept + x * slope + e
  tib <- rbind(tib, tibble(
    x = x, y = y, panel = i
  ))
}
  
coefs <- tib |> group_by(panel) |> summarise(
  intercept = coef(lm(y ~ x))[1],
  slope = coef(lm(y ~ x))[2]
) |> mutate(label = sprintf("a = %.2f", intercept))

base0 <- ggplot(tib, aes(x = x, y = y)) +
  # geom_smooth(method = "lm", se = FALSE) +
  geom_point(shape = 21, fill = "grey") +
  # geom_text(data = coefs, aes(x = I(.025), y = I(.95), label = label), hjust = 0) +
  facet_wrap(~panel, nrow = 2) +
  theme_bw(base_size = 24)
base1 <- base0 + geom_smooth(method = "lm", se = FALSE, color = regressioncol)
base2 <- base1 + 
  geom_text(data = coefs, aes(x = I(.025), y = I(.94), label = label), hjust = 0) +
  geom_point(data = coefs, aes(x = 0, y = intercept), shape = 17, color = "gold", size = 8)
base2

w <- 12
save_figs(base0, base1, base2, 
          width = 4 * w, height = 2 * w,
          .base = file.path(out_dir, "sampling_dist_%d.svg"))



xx <- seq(0.01, 4, .01)
# yy <- df(xx, 1, 50)
yy <- df(xx, 1, 1002)
f_crit <- .5
f_max <- max(xx)
tib_line  <- tibble(x = xx, y = yy)
tib_shade <- tib_line |> filter(x >= f_crit)
# tib_shade <- bind_rows(tibble(x=f_crit, y = 0), tib_shade, tibble(x=f_max, y = 0))
f_dist0 <- ggplot(tib_line, aes(x, y)) +
  geom_line() +
  labs(x = "F-statistic", y = "Density") +
  theme_bw(base_size = 30)

stat_col <- "darkgreen"
f_dist1 <- f_dist0 +
  geom_vline(xintercept = f_crit, linetype = "dashed", alpha = .7, color = stat_col) +
  annotate("text", x = f_crit + .1, y = 3.05, label = "Test-statistic", hjust = 0, vjust = 0, color = stat_col)

area_col <- "darkorchid4"
xb <- 2.5
yb <- 2
xe <- 1.5
ye <- df(xe, 1, 50) * 2
f_dist2 <- f_dist1 +
  geom_ribbon(data = tib_shade, aes(x = x, ymax = y, ymin = 0), fill = area_col, color = area_col, alpha = .8) +
  geom_segment(x = xb, y = yb, xend = xe, yend = ye, arrow = arrow(length = unit(0.4,"cm")), color = area_col) +
  annotate("text", x = xb*1.05, y = yb*1.05, label = "Area = p-value", hjust = 0, vjust = 0, color = area_col)

save_figs(f_dist0, f_dist1, f_dist2, .base = file.path(out_dir, "f_dist_%d.svg"), width = 40)

f_crit <- .5
f_max <- max(xx)
tib_line  <- tibble(x = xx, y = yy)
tib_shade <- tib_line |> filter(x >= f_crit)
# tib_shade <- bind_rows(tibble(x=f_crit, y = 0), tib_shade, tibble(x=f_max, y = 0))
f_dist0 <- ggplot(tib_line, aes(x, y)) +
  geom_line() +
  labs(x = "F-statistic", y = "Density") +
  theme_bw(base_size = 30)

stat_col <- "darkgreen"
f_dist1 <- f_dist0 +
  geom_vline(xintercept = f_crit, linetype = "dashed", alpha = .7, color = stat_col) +
  annotate("text", x = f_crit + .1, y = 3.05, label = "Test-statistic", hjust = 0, vjust = 0, color = stat_col)

area_col <- "darkorchid4"
xb <- 2.5
yb <- 2
xe <- 1.5
ye <- df(xe, 1, 50) * 2
f_dist2 <- f_dist1 +
  geom_ribbon(data = tib_shade, aes(x = x, ymax = y, ymin = 0), fill = area_col, color = area_col, alpha = .8) +
  geom_segment(x = xb, y = yb, xend = xe, yend = ye, arrow = arrow(length = unit(0.4,"cm")), color = area_col) +
  annotate("text", x = xb*1.05, y = yb*1.05, label = "Area = p-value", hjust = 0, vjust = 0, color = area_col)

save_figs(f_dist0, f_dist1, f_dist2, .base = file.path(out_dir, "f_dist_%d.svg"), width = 40)


