rm(list = ls())
# renv::install("datasauRus")
library(ggplot2)
library(datasauRus)
library(tibble)
library(dplyr)
data("anscombe")
data("datasaurus_dozen")

anscombe_tib <- tibble(
  x = unlist(anscombe[, 1:4]),
  y = unlist(anscombe[, 5:8]),
  dataset = rep(c("linear", "nonlinear", "robust", "outlier"), each = nrow(anscombe))
)

plot1 <- ggplot(anscombe_tib, aes(x = x, y = y, fill = dataset))+
  geom_point(size = 3, colour = "black", alpha = .6, shape = 21) +
  theme(legend.position = "none") +
  facet_wrap(~dataset, ncol = 2) +
  theme_bw(base_size = 20) +
  theme(strip.text = element_text(size = 12))
plot1

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

ggsave(filename = "lecture3/images/fig_anscombe.jpg",   plot = plot1)
ggsave(filename = "lecture3/images/fig_datasaurus.jpg", plot = plot2)

anscombe_tib
sum_ans <- anscombe_tib |> 
  group_by(dataset) |> 
  summarise(
    mean_x = mean(x),
    mean_y = mean(y),
    sd_x   = sd(x),
    sd_y   = sd(y),
    cor    = round(cor(x, y), 3)
  )

sum_dd <- datasaurus_dozen |> 
  filter(dataset %in% keep) |> 
  group_by(dataset) |> 
  summarise(
    mean_x = mean(x),
    mean_y = mean(y),
    sd_x   = sd(x),
    sd_y   = sd(y),
    cor    = round(cor(x, y), 3)
  )

write.csv(sum_ans, file = "lecture3/scripts/sum_anscombe.csv")
write.csv(sum_dd,  file = "lecture3/scripts/sum_datasaurus.csv")