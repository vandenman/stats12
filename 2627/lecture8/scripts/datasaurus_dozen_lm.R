rm(list = ls())
library(patchwork)
library(ggplot2)
library(datasauRus)
data("datasaurus_dozen")
# datasaurus_dozen$dataset <- factor(datasaurus_dozen$dataset)

all <- unique(datasaurus_dozen$dataset)
cbind(seq_along(all), all)
keep <- all[c(1, 2, 5, 6, 7, 8, 9, 10, 11)]

plot2 <- ggplot(datasaurus_dozen |> filter(dataset %in% keep), aes(x = x, y = y, fill = dataset)) +
  geom_point(size = 3, colour = "black", alpha = .6, shape = 21) +
  theme(legend.position = "none") +
  facet_wrap(~dataset, ncol = 3) +
  scale_x_continuous(breaks = c(0, 50, 100), limits = c(0, 100)) +
  scale_y_continuous(breaks = c(0, 50, 100), limits = c(0, 100)) +
  theme_bw(base_size = 20) +
  theme(strip.text = element_text(size = 12))

plot2
ggsave("lecture8/figures/datasaurus_dozen.svg", plot = plot2,
       width = 9, height = 9)

for (i in seq_along(keep)) {
  
  keep2 <- keep[i]
  d_sub <- datasaurus_dozen |> filter(dataset %in% keep2)
  fit_sub <- lm(y ~ x, data = d_sub)
  d_fit <- data.frame(
    fitted = fitted(fit_sub),
    residuals  = residuals(fit_sub),
    dataset = d_sub$dataset
  )
  
  plot31 <- ggplot(d_sub, aes(x = x, y = y, fill = dataset)) +
    geom_point(size = 3, colour = "black", alpha = .6, shape = 21) +
    geom_abline(intercept = coef(fit_sub)[1], slope = coef(fit_sub)[2]) +  
    scale_x_continuous(breaks = c(0, 50, 100), limits = c(0, 100)) +
    scale_y_continuous(breaks = c(0, 50, 100), limits = c(0, 100)) +
    theme_bw(base_size = 20) +
    ggtitle("Raw x vs. y") +
    theme(strip.text = element_text(size = 12),
          legend.position = "none")
  
  # plot32 <- ggplot(d_fit, aes(x = fitted, y = residuals, fill = dataset)) +
  #   geom_point(size = 3, colour = "black", alpha = .6, shape = 21) +
  #   ggtitle("Fitted (x) vs. Residuals (y)") +
  #   theme_bw(base_size = 20) +
  #   theme(strip.text = element_text(size = 12),
  #         legend.position = "none")
  
  d_fit <- data.frame(
    x = d_sub$x,
    fitted = fitted(fit_sub),
    residuals = residuals(fit_sub),
    dataset = d_sub$dataset
  )
  
  plot32 <- ggplot(d_fit, aes(x = x, y = residuals, fill = dataset)) +
    geom_point(size = 3, colour = "black", alpha = .6, shape = 21) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ggtitle("Predictor (x) vs. Residuals (y)") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none")
  
  plt <- plot31 + plot32
  ggsave(sprintf("lecture8/figures/dd_lm_%d.svg", i), plot = plt)

}

data("anscombe")
anscombe_tib <- tibble(
  x = unlist(anscombe[, 1:4]),
  y = unlist(anscombe[, 5:8]),
  dataset = rep(c("linear", "nonlinear", "robust", "outlier"), each = nrow(anscombe))
)

plot1 <- ggplot(anscombe_tib, aes(x = x, y = y, fill = dataset)) +
  geom_point(size = 3, colour = "black", alpha = .6, shape = 21) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE, alpha = .7) +
  theme(legend.position = "none") +
  facet_wrap(~dataset, ncol = 2) +
  theme_bw(base_size = 20) +
  theme(strip.text = element_text(size = 12))
plot1

ggsave(filename = "lecture8/figures/anscombe_linear.svg", plot = plot1)
