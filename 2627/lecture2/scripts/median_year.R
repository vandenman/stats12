rm(list = ls())
library(ggplot2)

d <- read.csv("lecture2/scripts/movies.csv")

h <- hist(d$year, plot = FALSE)
plot <- ggplot(d, aes(year)) + 
  geom_histogram(binwidth = h$breaks[2] - h$breaks[1], color = "#E40303", fill = scales::alpha("grey80", .5), linewidth = 1.5) + 
  theme_bw(base_size = 20)

ggsave(filename = "lecture2/scripts/fig_median_year0.jpg", plot = plot)
plot1 <- plot + geom_vline(xintercept = mean(d$year), color = "gold", linewidth = 2)

ggsave(filename = "lecture2/scripts/fig_median_year1.jpg", plot = plot1)

plot2 <- plot1 + geom_vline(xintercept = median(d$year), color = "springgreen", linewidth = 2)

ggsave(filename = "lecture2/scripts/fig_median_year2.jpg", plot = plot2)


mode <- as.numeric(names(which.max(table(d$year))))

plot3 <- plot2 + geom_vline(xintercept = mode, color = "darkorange1", linewidth = 2)

ggsave(filename = "lecture2/scripts/fig_median_year3.jpg", plot = plot2)

hist(d$year, probability = TRUE)
h <- hist(d$year, plot = FALSE)
plot4 <- ggplot(d, aes(year, after_stat(count) / sum(after_stat(count)))) + 
  geom_histogram(binwidth = h$breaks[2] - h$breaks[1], color = "#E40303", fill = scales::alpha("grey80", .5), linewidth = 1.5) + 
  ylab("Probability") + 
  theme_bw(base_size = 20)

ggsave(filename = "lecture2/scripts/fig_freq_year.jpg", plot = plot4)


plot(tapply(d$binary == "PASS", d$year, mean))
plot(tapply(d$binary == "PASS", d$year, length))
