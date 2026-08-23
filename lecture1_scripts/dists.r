library(ggplot2)
x0  <- seq(-5, 5, .1)
x1  <- seq(0, 5, .05)
y0 <- dcauchy(x0, 0, 1)
y1 <- dexp(x1, 2)
y2 <- rev(dgamma(x1, 5, 2))

make_plt <- function(x, y) {
  dff <- data.frame(x = x, y = y)
  ggplot(dff, aes(x = x, y = y)) +
    geom_line() +
    theme_bw(base_size = 20) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text = element_blank()
    )
}
ggsave(filename = "lecture1_images/dist_0.jpg", make_plt(x0, y0))
ggsave(filename = "lecture1_images/dist_1.jpg", make_plt(x1, y1))
ggsave(filename = "lecture1_images/dist_2.jpg", make_plt(x1, y2))


set.seed(42)
xx <- c(rgamma(10000, 2, 2), rnorm(5000, 4, 1))

dd <- density(xx)
gg <- make_plt(dd$x, dd$y)
# ggsave(filename = "lecture1_images/dist_3.jpg", gg)

imax <- which.max(dd$y)
x_mode <- dd$x[imax]
y_mode <- dd$y[imax]

x_mean <- mean(xx)
y_mean <- dd$y[which.min(abs(dd$x - x_mean))]

x_median <- median(xx)
y_median <- dd$y[which.min(abs(dd$x - x_median))]

df_pt <- data.frame(x = c(x_mode, x_mean, x_median), y = c(y_mode, y_mean, y_median), label = "?")

gg2 <- make_plt(dd$x, dd$y) +
  geom_point(data = df_pt, aes(x = x, y = y), shape = 21, fill = "grey", size = 10) +
  geom_text(data = df_pt, aes(x = x, y = y, label = label), size = 8) +
  labs(x = NULL, y = NULL) + theme(axis.ticks = element_blank())
gg2
ggsave(filename = "lecture1_images/dist_3.jpg", gg2)
# ggsave(filename = "lecture1_images/dist_3_answer.jpg", gg2)
