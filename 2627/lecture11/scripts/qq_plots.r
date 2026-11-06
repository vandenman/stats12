library(patchwork)
library(ggplot2)

out_dir <- "lecture11/figures"
img_dir <- "~/github/education/omi/images"
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

make_qq_plots <- function(x, bins = 15, probs = c(.10, .50, .90)) {
  
  x <- x[is.finite(x)]
  n <- length(x)
  stopifnot(n >= 10, sd(x) > 0)
  
  # Histogram information
  h <- hist(x, breaks = bins, plot = FALSE)
  width <- h$breaks[2] - h$breaks[1]
  
  dat <- data.frame(
    bin    = factor(seq_along(h$counts)),
    center = h$mids,
    count  = h$counts,
    prop   = h$counts / n,
    dens   = h$density
  )
  
  # Consistent colors for all plots
  colors <- setNames(
    hcl.colors(nrow(dat), "Viridis"),
    levels(dat$bin)
  )
  
  theme_common <- theme_bw(base_size = 20) + theme(legend.position = "none")
  # theme_common <- theme_minimal(base_size = 18) +
  #   theme(
  #     panel.grid.minor = element_blank(),
  #     legend.position = "none"
  #   )
  
  percent_axis <- scale_y_continuous(
    breaks = seq(0, 1, .2),
    labels = function(z) paste0(round(100 * z), "%"),
    limits = c(0, 1)
  )
  
  p0 <- ggplot(dat, aes(center, dens)) +
    geom_col(width = width, fill = "grey65", color = "white") +
    labs(
      title = "1. Histogram",
      x = "Residual",
      y = "Frequency"
    ) +
    theme_common

  # 1. Ordinary histogram
  p1 <- ggplot(dat, aes(center, count)) +
    geom_col(width = width, fill = "grey65", color = "white") +
    labs(
      title = "1. Histogram",
      x = "Residual",
      y = "Frequency"
    ) +
    theme_common
  
  # 2. Colored histogram
  p2 <- ggplot(dat, aes(center, count, fill = bin)) +
    geom_col(width = width, color = "white") +
    scale_fill_manual(values = colors) +
    labs(
      title = "2. From left to right",
      x = "Residual",
      y = "Frequency"
    ) +
    theme_common
  
  # 3. Cumulative histogram
  # For every bin i, stack the contributions of bins 1,...,i.
  stacked <- do.call(rbind, lapply(seq_len(nrow(dat)), function(i) {
    data.frame(
      center = dat$center[i],
      contribution = dat$prop[seq_len(i)],
      source = factor(seq_len(i), levels = seq_len(nrow(dat)))
    )
  }))
  
  p3 <- ggplot(stacked, aes(center, contribution, fill = source)) +
    geom_col(
      width = width,
      color = "white",
      linewidth = 0.15,
      position = position_stack(reverse = TRUE)
    ) +
    scale_fill_manual(values = colors) +
    percent_axis +
    labs(
      title = "3. Cumulative distribution",
      x = "Residual",
      y = "Percentage below"
    ) +
    theme_common
  
  # Exact empirical CDF
  values <- sort(unique(x))
  cdf <- data.frame(
    value = c(min(x), values),
    F = c(0, ecdf(x)(values))
  )
  
  # Select observations near the requested percentiles.
  # Using ppoints ensures these are also exact Q-Q plot points.
  ordered <- sort(x)
  pp <- ppoints(n)
  indices <- unique(pmax(1, pmin(n, round(probs * n))))
  
  selected <- data.frame(
    value = ordered[indices],
    prob = pp[indices],
    theoretical = qnorm(pp[indices], mean(x), sd(x)),
    label = paste0(round(100 * pp[indices]), "%"),
    xmin = min(x)
  )
  
  # Reuse histogram colors for selected quantiles
  selected_bins <- as.integer(cut(
    selected$value,
    breaks = h$breaks,
    include.lowest = TRUE
  ))
  
  selected_colors <- setNames(
    unname(colors[as.character(selected_bins)]),
    selected$label
  )
  
  # 4. Empirical CDF with quantiles
  p4 <- ggplot(cdf, aes(value, F)) +
    geom_step(color = "grey30", linewidth = 1) +
    geom_segment(
      data = selected,
      aes(x = xmin, xend = value,
          y = prob, yend = prob, color = label),
      inherit.aes = FALSE,
      linetype = "dashed"
    ) +
    geom_segment(
      data = selected,
      aes(x = value, xend = value,
          y = 0, yend = prob, color = label),
      inherit.aes = FALSE,
      linetype = "dashed"
    ) +
    geom_point(
      data = selected,
      aes(x = value, y = prob, color = label),
      inherit.aes = FALSE,
      size = 3
    ) +
    scale_color_manual(values = selected_colors) +
    percent_axis +
    labs(
      title = "4. From percentages to quantiles",
      x = "Residual",
      y = "Percentage below"
    ) +
    theme_common
  
  # 5. Normal Q-Q plot
  qq <- data.frame(
    theoretical = qnorm(pp, mean(x), sd(x)),
    observed = ordered
  )
  
  p5 <- ggplot(qq, aes(theoretical, observed)) +
    geom_abline(
      slope = 1, intercept = 0,
      color = "grey50", linetype = "dashed"
    ) +
    geom_point(color = "grey65", size = 1.7) +
    geom_point(
      data = selected,
      aes(x = theoretical, y = value, color = label),
      inherit.aes = FALSE,
      size = 3
    ) +
    scale_color_manual(values = selected_colors) +
    coord_equal() +
    labs(
      title = "5. Normal Q-Q plot",
      x = "Theoretical normal quantiles",
      y = "Observed quantiles"
    ) +
    theme_common
  
  list(
    histogram_dens = p0,
    histogram  = p1,
    colored    = p2,
    cumulative = p3,
    quantiles  = p4,
    qq         = p5
  )
}


path <- "~/github/education/omi/attribute_means.csv"
dat <- readr::read_csv(path)

plots <- make_qq_plots(residuals(all_fits$happy))


plot_spacer() + plots$histogram
plots$histogram + plots$colored
plots$colored + plots$cumulative
plots$cumulative + plots$quantiles
plots$quantiles + plots$qq

plots <- make_qq_plots(residuals(all_fits$godly))

plot_spacer() + plots$histogram
plots$histogram + plots$colored
plots$colored + plots$cumulative
plots$cumulative + plots$quantiles
plots$quantiles + plots$qq


set.seed(42)
# Dummy residuals; replace with your own data
residuals <- rnorm(500, mean = 0, sd = 2)


plots0 <- make_qq_plots(residuals, bins = 60)
plots1 <- make_qq_plots(residuals, bins = 30)
plots <- make_qq_plots(residuals, bins = 15)

scales <- list(
  scale_x_continuous(limits = c(-6, 6), breaks = seq(-6, 6, 3)),
  scale_y_continuous(limits = c(0, .3), breaks = seq(0, .3, .1))
)
h100 <- plots0$histogram_dens + stat_function(fun = dnorm, args = list(mean = mean(residuals), sd = sd(residuals)), colour = "dodgerblue") + ggtitle("60 bins") + scales
h50  <- plots1$histogram_dens + stat_function(fun = dnorm, args = list(mean = mean(residuals), sd = sd(residuals)), colour = "dodgerblue") + ggtitle("30 bins") + scales
h15  <- plots$histogram_dens + stat_function(fun = dnorm, args = list(mean = mean(residuals), sd = sd(residuals)), colour = "dodgerblue") + ggtitle("15 bins") + scales

save_figs((h100 + h50 + h15) + plot_layout(axes = "collect"),
  width = 40,
  .base = fs::path(out_dir, "hist_bins", ext = "svg")
)


plot_spacer() + plots$histogram
plots$histogram + plots$colored
plots$colored + plots$cumulative
plots$cumulative + plots$quantiles
plots$quantiles + plots$qq

save_figs(
  plots$histogram + plot_spacer(),
  plots$histogram + plots$colored,
  plots$colored + plots$cumulative,
  plots$cumulative + plots$quantiles,
  plots$quantiles + plots$qq,
  width = 40,
  .base = fs::path(out_dir, "hist_qq_sim_%d", ext = "svg")
)



plots <- make_qq_plots(dat$trustworthy)
