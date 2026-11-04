# Focused teaching comparison: constant versus changing residual spread.
library(ggplot2)
library(patchwork)

dat <- read.csv("~/github/education/omi/attribute_means.csv", check.names = FALSE)
variables <- c("typical", "smart")

residual_data <- do.call(rbind, lapply(variables, function(variable) {
  x <- dat[[variable]]
  quartile <- cut(x, quantile(x, seq(0, 1, 0.25)),
                  include.lowest = TRUE, labels = paste0("Q", 1:4))
  data.frame(variable, quartile,
             residual = residuals(lm(dat$trustworthy ~ x)))
}))
residual_data$variable <- factor(residual_data$variable, levels = variables)

spread <- aggregate(residual ~ variable + quartile, residual_data, sd)
names(spread)[3] <- "sd"
spread$label <- sprintf("SD %.1f", spread$sd)

boxes <- ggplot(residual_data, aes(quartile, residual)) +
  geom_hline(yintercept = 0, colour = "grey55") +
  geom_jitter(width = 0.18, height = 0, alpha = 0.18, size = 0.45) +
  geom_boxplot(width = 0.48, outlier.shape = NA, fill = "#8fb9cf",
               alpha = 0.7) +
  geom_text(data = spread, aes(quartile, 39, label = label),
            inherit.aes = FALSE, size = 3.2) +
  facet_wrap(~variable, nrow = 1) +
  coord_cartesian(ylim = c(-45, 43)) +
  labs(x = "Predictor quartile (low to high)", y = "Residual") +
  theme_bw(base_size = 11)

sd_plot <- ggplot(spread, aes(quartile, sd, colour = variable, group = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.8) +
  scale_colour_manual(values = c(typical = "#14769d", smart = "#ca5335")) +
  scale_y_continuous(limits = c(8, 19), breaks = seq(8, 18, 2)) +
  labs(x = "Predictor quartile (low to high)", y = "Residual SD",
       colour = "Predictor") +
  theme_bw(base_size = 11)

figure <- (boxes / sd_plot) +
  plot_layout(heights = c(2, 1.1)) +
  plot_annotation(
    title = "Residual spread: typical versus smart",
    subtitle = "Separate OLS fits of trustworthy on each predictor; each quartile contains 251 faces.",
    caption = "The upper panels share a residual scale. Boxes show the middle 50% of residuals; labels and lower panel show SD."
  )

ggsave("lecture11/figures/variance_comparison.pdf", figure,
       width = 11, height = 7.5, units = "in")
