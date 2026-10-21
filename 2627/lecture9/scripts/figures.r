rm(list = ls())
library(ggplot2)
library(tibble)
library(dplyr)
library(patchwork)
out_dir <- "lecture9/figures"
if (!dir.exists(out_dir)) dir.create(out_dir)

theme_set(
  theme_bw(base_size = 32) +
    theme(geom = element_geom(pointshape = 21, pointsize = 3, fill = scales::alpha("grey", .85)))
)

set.seed(42/101)
Hours = abs(rnorm(92, 4, 2))
Grade = abs(3 + (7/9)*Hours)+rnorm(92)
Grade[Grade > 10] <- 10 - (Grade[Grade > 10] - 10)
studentdata <- data.frame(Grade=Grade, Hours=Hours)

studentdata <- studentdata |> 
  mutate(Special = case_when(
    Grade > 8.7 & Hours < 4.3 ~ "high",
    Grade < 7.0 & Grade > 6.5 & Hours < 4.1 & Hours > 3.9   ~ "low",
    .default = "other"
  ))
table(studentdata$Special)

round(head(studentdata[, 1:2], 6), 2)
hh <- head(studentdata[, 1:2])
knitr::kable(hh, format = "pipe", digits = 2)
a <- 2.9
b <- 0.8
hh$yhat <- a + b * hh$Hours
knitr::kable(hh, format = "pipe", digits = 2)
hh$resid <- hh$Grade - hh$yhat
knitr::kable(hh, format = "pipe", digits = 2)

studentdata$yhat  <- a + b * studentdata$Hours
studentdata$resid <- studentdata$Grade - studentdata$yhat
sum(studentdata$resid^2)

fit <- lm(Hours ~ Grade, studentdata)
sum(resid(fit))

# dput(studentdata)
# points: [
#   [0.7, 3.1], [1.3, 4.5], [2.1, 3.9], [2.8, 5.7],
#   [3.5, 5.0], [4.1, 6.8], [4.8, 5.8], [5.4, 7.7],
#   [6.2, 7.0], [7.0, 8.5], [7.8, 8.0], [8.7, 9.4]
# ],
to_js <- function(dat) {
  cat("points: [\n  ")
  n <- nrow(dat)
  for (i in seq_len(n)) {
    cat(sprintf("[%.2f, %.2f]", dat$Hours[i], dat$Grade[i]))
    if (i != n)
      cat(", ")
  }
  cat("\n]\n")
}
to_js(studentdata)
head(studentdata)

scales <- list(
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)), 
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1))
)

base <- ggplot(studentdata, aes(x = Hours, y = Grade)) + scales

points <- geom_point()

regressioncol <- "firebrick2"
y_mean <- geom_hline(yintercept = mean(Grade), linewidth = 2, colour = regressioncol)

errorcol <- scales::alpha("dodgerblue2", .85)
y_mean_errors <- geom_segment(x=Hours, xend = Hours, y = mean(Grade), yend = Grade, col=errorcol)

cijfer.uren <- lm(Grade ~ Hours, studentdata)
y_linreg <- geom_abline(intercept = cijfer.uren$coefficients[1], slope = cijfer.uren$coefficients[2], colour = regressioncol, size = 2)
y_linreg_errors <- geom_segment(x = Hours, xend = Hours, y = Grade,  yend = predict.lm(cijfer.uren, newdata = studentdata), colour = errorcol)

preds <- predict.lm(cijfer.uren, newdata = studentdata)
residuals <- Grade - preds
i1 <- preds > Grade
i2 <- !i1
i3 <- ifelse(i1, 1, 2)
errorcol_above <- "springgreen"
errorcol_below <- "gold"
y_linreg_errors_abovebelow <- geom_segment(x = Hours, xend = Hours, y = Grade, yend = preds, colour = c(errorcol_below, errorcol_above)[i3])

sum(residuals[i1])
sum(residuals[!i1])


cols  <- c("black", "springgreen",  "gold")
fills <- c(scales::alpha("grey", .85), "springgreen3", "gold3")
errorcols <- c("springgreen4", "gold4")
alpha <- .2
points1 <- geom_point(
  colour = cols[ 1 + (studentdata$Special == "high")],
  fill   = fills[1 + (studentdata$Special == "high")],
  alpha  = alpha + (1 - alpha) * (studentdata$Special == "high"),
  size = 3 + 1 * (studentdata$Special == "high")
)
i1 <- which(studentdata$Special == "high")
y_linreg_errors1 <- geom_segment(x = Hours[i1], xend = Hours[i1], y = Grade[i1],  yend = predict.lm(cijfer.uren, newdata = studentdata[i1, ]), colour = errorcols[1])
lines1 <- geom_line(data = tibble(
  Hours = c(0, studentdata$Hours[i1], studentdata$Hours[i1]),
  Grade = c(studentdata$Grade[i1], studentdata$Grade[i1], 0)
), mapping = aes(x = Hours, y = Grade), linetype = 2, color = errorcols[1], inherit.aes = FALSE)

points2 <- geom_point(
  colour = cols[ 1 + 2*(studentdata$Special == "low")],
  fill   = fills[1 + 2*(studentdata$Special == "low")],
  alpha  = alpha + (1 - alpha) * (studentdata$Special == "low"),
  size = 3 + 1 * (studentdata$Special == "low")
)
i2 <- which(studentdata$Special == "low")
y_linreg_errors2 <- geom_segment(x = Hours[i2], xend = Hours[i2], y = Grade[i2],  yend = predict.lm(cijfer.uren, newdata = studentdata[i2, ]), colour = errorcols[2])
lines2 <- geom_line(data = tibble(
  Hours = c(0, studentdata$Hours[i2], studentdata$Hours[i2]),
  Grade = c(studentdata$Grade[i2], studentdata$Grade[i2], 0)
), mapping = aes(x = Hours, y = Grade), linetype = 2, color = errorcols[2], inherit.aes = FALSE)

# base + points
# base + y_mean + points
# base + y_mean_errors + y_mean + points 
# base + y_linreg + points
# base + y_linreg_errors + y_linreg + points

base + 
  y_linreg_errors_abovebelow +
  y_linreg + points

(base + y_linreg_errors1 + y_linreg + points1) +
(base + y_linreg_errors2 + y_linreg + points2) +
  patchwork::plot_layout(axes = "collect")

(base + y_linreg + lines1 + points1) +
(base + y_linreg + lines2 + points2) +
  patchwork::plot_layout(axes = "collect")

save_figs <- function(..., width = 25, height = 25, units = "cm", .base) {
  for (i in seq_len(...length()))
    ggsave(...elt(i), file = sprintf(.base, i),
           width = height, height = height, units = units)
}
save_figs(
  base + points,
  base + y_mean + points,
  base + y_mean_errors + y_mean + points,
  base + y_linreg + points,
  base + y_linreg_errors + y_linreg + points,
  .base = file.path(out_dir, "plot_%d.svg")
)
ggsave(file.path(out_dir, "pred_error.svg"), plot = (base + y_linreg + lines1 + points1) +
         (base + y_linreg + lines2 + points2) +
         patchwork::plot_layout(axes = "collect"),
       width = 50, height = 25, units = "cm"
)

ggsave(
  filename = file.path(out_dir, "pred_errorabovebelow.svg"), 
  plot = 
    base + 
    y_linreg_errors_abovebelow +
    y_linreg + points
)

preds_ols <- tibble(
  r = seq(0, 5, .01),
  r_ols = r^2,
  r_abs = abs(r) 
) |> tidyr::pivot_longer(cols = c(r_ols, r_abs))

plt <- ggplot(preds_ols, aes(x = r, y = value, color = name)) +
  geom_line() +
  labs(x = "Residual", y = NULL, color = NULL) +
  scale_color_manual(values = c("firebrick2", "springgreen"), 
                     labels = c("r_ols" = "Squared Error",
                                "r_abs" = "Absolute Error")) +
  theme(legend.position = "inside",
        legend.position.inside = c(.25, .85),
        legend.background = element_blank())

save_figs(
  plt,
  .base = file.path(out_dir, "plot_error_%d.svg")
)


n <- 20
df <- n - 1

tib <- tibble(
  x = seq(-5, 5, .01),
  y = dt(x, df = df)
)

crit <- qt(0.975, df = df)
tib_crit <- tib |> filter(x > crit)

ggplot() + 
  geom_ribbon(data = tib_crit, aes(x = x, ymax = y, ymin = 0)) +
  geom_line(data = tib, aes(x = x, y = y))+ 
  scale_y_continuous("Density", breaks = c(0, .2, .4), labels = NULL) +
  scale_x_continuous("t", breaks = c(-5, 0, 5))


# ---
#   title: "SPSS Style Tables"
# format: html
# ---
#   
#   ```{r}
# #| label: setup
# #| message: false
# #| warning: false
library(tidyverse)
library(gtsummary)
library(gt)

# 1. Generate Fake Data
set.seed(42)
fake_data <- tibble(
  Age = rnorm(100, mean = 45, sd = 10),
  Gender = sample(c("Male", "Female"), 100, replace = TRUE),
  Group = sample(c("Control", "Treatment A", "Treatment B"), 100, replace = TRUE),
  Satisfied = sample(c("Yes", "No"), 100, replace = TRUE)
)

# 2. Create SPSS-style Demographics Table
fake_data %>%
  select(Gender, Group, Satisfied) %>%
  tbl_summary(by = Group) %>%
  modify_header(label = "**Variable**") %>%
  as_gt() %>%
  # 3. Apply SPSS-like minimalist styling
  tab_options(
    table.border.top.color = "black",
    table.border.top.width = px(2),
    table.border.bottom.color = "black",
    table.border.bottom.width = px(2),
    table_body.border.bottom.color = "black",
    table_body.border.bottom.width = px(1),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(1)
  )
# ```
