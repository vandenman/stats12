# Run from 2627 so .Rprofile activates the project renv library:
# Rscript lecture11/scripts/diagnostic_figures.R [path/to/attribute_means.csv]
# Reuses the examples and diagnostic approach in data_analyses.R and
# variance_comparison_scratch.R. The exploratory scripts are left unchanged.
library(ggplot2)
library(patchwork)

stopifnot(basename(getwd()) == "2627")
args <- commandArgs(trailingOnly = TRUE)
data_path <- if (length(args)) args[[1]] else Sys.getenv(
  "OMI_ATTRIBUTE_MEANS", "~/github/education/omi/attribute_means.csv"
)
if (!file.exists(data_path)) {
  stop("Supply the OMI attribute_means.csv path as an argument or OMI_ATTRIBUTE_MEANS.")
}
dat <- read.csv(data_path, check.names = FALSE)
variables <- c("happy", "cute", "godly", "electable", "typical", "smart", "gender", "age")
stopifnot(all(c("trustworthy", variables) %in% names(dat)))
stopifnot(all(vapply(dat[c("trustworthy", variables)], function(x) all(is.finite(x)), logical(1))))
out_dir <- "lecture11/figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fits <- setNames(lapply(variables, function(variable) {
  lm(reformulate(paste0("`", variable, "`"), "trustworthy"), data = dat)
}), variables)
diagnostics <- function(variable) {
  fit <- fits[[variable]]
  data.frame(x = dat[[variable]], y = dat$trustworthy,
             fitted = fitted(fit), residual = residuals(fit))
}
theme_set(theme_bw(base_size = 19))
theme_update(plot.title = element_text(size = 20, face = "bold"),
             panel.grid.minor = element_blank())
red <- "#c43138"
blue <- "#087fa4"
points <- geom_point(shape = 21, fill = "grey65", colour = "grey35",
                     alpha = 0.5, size = 1.3, stroke = 0.2)
save_figure <- function(plot, name, width = 12, height = 4.6) {
  ggsave(file.path(out_dir, paste0(name, ".svg")), plot,
         device = svglite::svglite, width = width, height = height,
         bg = "white")
}
residual_plot <- function(variable, smooth = FALSE) {
  p <- ggplot(diagnostics(variable), aes(x, residual)) + points +
    geom_hline(yintercept = 0, colour = red, linewidth = 0.8) +
    coord_cartesian(ylim = c(-45, 45)) +
    labs(x = tools::toTitleCase(variable), y = "Residual",
         title = paste("Predictor:", variable))
  if (smooth) p <- p + geom_smooth(method = "loess", formula = y ~ x,
                                  span = 0.75, se = FALSE,
                                  colour = blue, linewidth = 1)
  p
}
histogram <- function(variable) {
  d <- diagnostics(variable)
  ggplot(d, aes(residual)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                   fill = "grey80", colour = "grey45") +
    stat_function(fun = dnorm,
                  args = list(mean = mean(d$residual), sd = sd(d$residual)),
                  colour = red, linewidth = 1) +
    coord_cartesian(xlim = c(-45, 45), ylim = c(0, 0.065)) +
    labs(x = "Residual", y = "Density", title = paste("Predictor:", variable))
}
qq_plot <- function(variable) {
  ggplot(diagnostics(variable), aes(sample = residual)) +
    stat_qq(alpha = 0.6, size = 1) +
    stat_qq_line(colour = red, linewidth = 0.9) +
    labs(x = "Normal quantile", y = "Residual quantile",
         title = paste("Predictor:", variable))
}

# The same highlighted face appears in both panels; no points are simulated.
d <- diagnostics("happy")
highlight <- d[which.max(d$residual), , drop = FALSE]
line <- coef(fits$happy)
scatter <- ggplot(d, aes(x, y)) + points +
  geom_abline(intercept = line[[1]], slope = line[[2]], colour = red, linewidth = 1) +
  geom_segment(data = highlight, aes(xend = x, y = fitted, yend = y),
               colour = blue, linewidth = 1.2) +
  geom_point(data = highlight, colour = blue, size = 3) +
  labs(x = "Happy", y = "Trustworthy", title = "Distance from the fitted line")
residuals <- residual_plot("happy") +
  geom_segment(data = highlight, aes(xend = x, y = 0, yend = residual),
               colour = blue, linewidth = 1.2) +
  geom_point(data = highlight, colour = blue, size = 3) +
  labs(title = "The same face in a residual plot")
save_figure(scatter + residuals, "faces_to_residuals")

save_figure(residual_plot("typical") + residual_plot("electable") +
              residual_plot("smart"), "faces_residual_patterns", width = 15)
save_figure(residual_plot("cute", TRUE) + residual_plot("electable", TRUE),
            "faces_residual_centres")

# Illustration of the normal-error model fitted to the faces. These bell
# shapes are model assumptions, not estimated conditional distributions.
model_x <- c(15, 45, 75)
model_bells <- do.call(rbind, lapply(model_x, function(x0) {
  mu <- predict(fits$happy, newdata = data.frame(happy = x0))
  y <- seq(mu - 2.8 * sigma(fits$happy), mu + 2.8 * sigma(fits$happy), length.out = 160)
  data.frame(x0, y, x = x0 + 7 * dnorm(y, mu, sigma(fits$happy)) /
               dnorm(mu, mu, sigma(fits$happy)))
}))
model_plot <- ggplot(d, aes(x, y)) +
  geom_point(colour = "grey75", alpha = 0.35, size = 1) +
  geom_abline(intercept = line[[1]], slope = line[[2]], colour = red, linewidth = 1) +
  geom_segment(data = data.frame(x0 = model_x, mu = line[[1]] + line[[2]] * model_x),
               aes(x = x0, xend = x0, y = mu - 2.8 * sigma(fits$happy),
                   yend = mu + 2.8 * sigma(fits$happy)), inherit.aes = FALSE,
               colour = blue, linetype = "dotted") +
  geom_path(data = model_bells, aes(x, y, group = x0), inherit.aes = FALSE,
            colour = blue, linewidth = 1.3) +
  labs(x = "Happy", y = "Trustworthy")
save_figure(model_plot, "faces_normal_model", width = 10, height = 4.8)
save_figure(histogram("godly") + histogram("happy"), "faces_normal_histograms")
save_figure(qq_plot("godly") + qq_plot("happy"), "faces_normal_qq")
save_figure(residual_plot("godly", TRUE) + qq_plot("godly"), "faces_normal_is_not_enough")

gender_hist <- ggplot(dat, aes(gender)) +
  geom_histogram(bins = 30, fill = "grey80", colour = "grey45") +
  labs(x = "Perceived gender rating (0-100)", y = "Number of faces",
       title = "The predictor is not normal")
save_figure(gender_hist + qq_plot("gender") +
              plot_annotation(caption = "Gender here is a continuous rating of perceived gender, not a binary category."),
            "faces_predictor_need_not_be_normal")

save_figure(residual_plot("typical", TRUE) + residual_plot("smart", TRUE),
            "faces_residual_spread")
# Quartiles describe changing spread along a continuous predictor; they are
# not experimental groups or a proposed recoding for subsequent tests.
spread_data <- do.call(rbind, lapply(c("typical", "smart"), function(variable) {
  d <- diagnostics(variable)
  d$quartile <- cut(d$x, quantile(d$x, seq(0, 1, 0.25)),
                    include.lowest = TRUE, labels = paste0("Q", 1:4))
  d$variable <- variable
  d
}))
spread <- aggregate(residual ~ variable + quartile, spread_data, sd)
names(spread)[[3]] <- "sd"
spread$label <- sprintf("SD %.1f", spread$sd)
set.seed(11)
spread_boxes <- ggplot(spread_data, aes(quartile, residual)) +
  geom_hline(yintercept = 0, colour = red) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.12, size = 0.7) +
  geom_boxplot(outlier.shape = NA, fill = "#8fb9cf", alpha = 0.7, width = 0.5) +
  geom_text(data = spread, aes(quartile, 39, label = label),
             inherit.aes = FALSE, size = 5) +
  facet_wrap(~variable, nrow = 1) +
  coord_cartesian(ylim = c(-45, 45)) +
  labs(x = "Predictor quartile (low to high)", y = "Residual")
save_figure(spread_boxes, "faces_spread_by_quartile", height = 4.8)
save_figure(residual_plot("age", TRUE) + qq_plot("age"), "faces_diagnosis_exercise")

# Illustrative population distributions: no invented grouping of real faces.
population_y <- seq(0, 100, length.out = 600)
group_means <- c(A = 25, B = 50, C = 75)
population <- do.call(rbind, lapply(names(group_means), function(group) {
  data.frame(y = population_y, density = dnorm(population_y, group_means[[group]], 5), group)
}))
mixture <- aggregate(density ~ y, population, mean)
group_plot <- ggplot(population, aes(y, density, colour = group)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = c(A = blue, B = red, C = "#619623")) +
  labs(x = "Outcome", y = "Density", colour = "Group", title = "Normal within each group")
pooled_plot <- ggplot(mixture, aes(y, density)) +
  geom_line(colour = "#003366", linewidth = 1.1) +
  labs(x = "Outcome", y = "Density", title = "All groups combined: not normal")
save_figure(group_plot + pooled_plot, "normal_within_groups")

normality <- do.call(rbind, lapply(c("godly", "happy", "typical", "smart"), function(variable) {
  test <- shapiro.test(residuals(fits[[variable]]))
  data.frame(predictor = variable, n = nobs(fits[[variable]]),
             W = unname(test$statistic), p = test$p.value)
}))
write.csv(normality, file.path(out_dir, "faces_normality_results.csv"), row.names = FALSE)
write.csv(spread, file.path(out_dir, "faces_spread_results.csv"), row.names = FALSE)
p_text <- ifelse(normality$p < 0.001, "< .001", sprintf("= %.3f", normality$p))
table <- c("Predictor | $N$ | Shapiro-Wilk $W$ | $p$",
           "--- | ---: | ---: | ---:",
           sprintf("%s | %d | %.3f | %s", normality$predictor, normality$n, normality$W, p_text))
writeLines(c("<!-- Generated by scripts/diagnostic_figures.R; do not edit values manually. -->", "", table),
           file.path(out_dir, "faces_normality_table.qmd"))
cat("Active renv project:", renv::project(), "\n")
print(normality)
print(spread)
cat("Saved lecture figures in", out_dir, "\n")
