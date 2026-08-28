rm(list = ls())
library(ggplot2)
library(tibble)

dtriangle <- function(x, a = -1, b = 1, c = 0) {
  ifelse(x < a, 0,
  ifelse(b < x, 0, 
  ifelse(a <= x & x < c, 2 * (x - a) / ((b - a) * (c - a)),
  ifelse(c < x & x <= b, 2 * (b - x) / ((b - a) * (b - c)),
    # x == c
    2 / (b - a)
  ))))
}

bimu1 <- -1
bisd1 <- .4
bimu2 <- 1
bisd2 <- .5
bimix <- .8


tb <- tribble(
  ~distribution, ~fun,
  "Normal",       \(x) dnorm(x, 0, .5),
  "Triangle",     \(x) dtriangle(x, -1.5, 1.5),
  "Uniform",      \(x) dunif(x, -1, 1),
  "Bimodal",      \(x) EnvStats::dnormMix(x, bimu1, bisd1, bimu2, bisd2, bimix),
  "Skew",         \(x) dgamma(x+2, 2, 2)
)
tb$distribution <- factor(tb$distribution, levels = tb$distribution)
tb$means <- c(0, 0, 0, bimix * bimu2 + (1 - bimix) * bimu1, 1 - 2)
rand_bimodal   <- 
median_bimodal <- EnvStats::qnormMix(.5, bimu1, bisd1, bimu2, bisd2, bimix)
tb$medians <- c(0, 0, 0, median_bimodal, qgamma(.5, 2, 2) - 2)

layers <- lapply(seq_len(nrow(tb)), \(i) {
  geom_function(data = tb[i, ], fun = tb$fun[[i]], xlim = c(-2, 2))
})


plot <- ggplot(tb) + layers + 
  geom_vline(aes(xintercept = means), color = "gold") +
  facet_grid(cols = vars(distribution)) + theme_bw(base_size = 20) +
  labs(x = NULL, y = NULL) +
  theme(strip.text = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

ggsave(filename = "lecture2/scripts/fig_means.jpg", plot = plot)


plot2 <- ggplot(tb) + layers + 
  geom_vline(aes(xintercept = means), color = "gold") +
  geom_vline(aes(xintercept = medians), color = "springgreen", linetype = 2) +
  facet_grid(cols = vars(distribution)) + theme_bw(base_size = 20) +
  labs(x = NULL, y = NULL) +
  theme(strip.text = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

ggsave(filename = "lecture2/scripts/fig_means_medians.jpg", plot = plot)