rm(list = ls())
library(ggplot2)

d <- read.csv("lecture2/scripts/movies.csv")

h <- hist(d$year, plot = FALSE)
ggplot(d, aes(year)) + 
  geom_histogram(binwidth = h$breaks[2] - h$breaks[1], color = "#E40303", fill = "#FFED00", linewidth = 1.5) + 
  theme_bw(base_size = 20)
