rm(list = ls())
library(ggplot2)
library(patchwork)
out_dir <- "lecture9/figures"

set.seed(42)
regressioncol <- "firebrick2"
a <- 2
b <- -2
residual_var <- 2.0
x <- runif(50, -1.75, 1.75)
e <- rnorm(50, 0, 1)
e <- scale(e) * sqrt(residual_var)

# Make errors orthogonal to x and mean zero
e <- residuals(lm(e ~ x))

# Scale to desired residual variance
e <- e * sqrt(residual_var / var(e))

y <- a + b * x + e
lm(y ~ x)

fig <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) + 
  geom_abline(slope = b, intercept = a, color = regressioncol) + 
  scale_y_continuous(breaks = seq(-6, 6, 2), limits = c(-6, 6)) +
  scale_x_continuous(breaks = seq(-2, 2, 1), limits = c(-2, 2), expand = 0) +
  geom_point()
fig

ggsave(file = "lecture9/figures/pplot.png", plot = fig)


# Q2
set.seed(42/101)
Hours = abs(rnorm(92, 4, 2))
Grade = abs(3 + (7/9)*Hours)+rnorm(92)
Grade[Grade > 10] <- 10 - (Grade[Grade > 10] - 10)
studentdata <- data.frame(Grade=Grade, Hours=Hours)

# add one point to every grade, then fit the regression again.
# Start one point lower so both panels stay within the 0–10 grading scale.
grades_original <- studentdata
grades_original$Grade <- grades_original$Grade - 1
grades_shifted <- grades_original
grades_shifted$Grade <- grades_shifted$Grade + 1

grade_original <- ggplot(grades_original, aes(Hours, Grade)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = regressioncol) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  coord_cartesian(ylim = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  labs(title = "Original grades", x = "Hours studied", y = "Grade")

grade_shifted <- grade_original + grades_shifted +
  labs(title = "Grades + 1")

grade_shift_arrow <- ggplot() +
  annotate("segment", x = 0.1, xend = 0.9, y = 0.5, yend = 0.5,
           linewidth = 1.2, arrow = grid::arrow(length = grid::unit(0.3, "cm"))) +
  annotate("text", x = 0.5, y = 0.65, label = "Add 1", size = 7) +
  xlim(0, 1) + ylim(0, 1) + theme_void()

grade_shift_example <- (grade_original + grade_shift_arrow + grade_shifted) +
  plot_layout(widths = c(1, 0.18, 1), axes = "collect")
grade_shift_example

ggsave("lecture9/figures/grades_plus_one.png", grade_shift_example,
       width = 45, height = 20, units = "cm")
