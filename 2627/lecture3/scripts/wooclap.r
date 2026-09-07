
d <- readRDS("lecture3/scripts/data/gap2022.rds")
g1 <- ggplot(d, aes(x = child_mortality, y = fertility)) + 
  geom_point(size = 3, shape = 21, fill = "grey", alpha = .7) + 
  labs(x = "Child mortality", y = "Fertility") +
  theme_bw(base_size = 24) 
g2 <- ggplot(d, aes(x = child_mortality, y = life_expectancy)) + 
  geom_point(size = 3, shape = 21, fill = "grey", alpha = .7) + 
  labs(x = "Child mortality", y = "Life expectancy") +
  theme_bw(base_size = 24) 
g3 <- ggplot(d, aes(x = fertility, y = life_expectancy)) + 
  geom_point(size = 3, shape = 21, fill = "grey", alpha = .7) + 
  labs(x = "Fertility", y = "Life expectancy") +
  theme_bw(base_size = 24) 

set.seed(42)
g4 <- ggplot(d |> mutate(average_height = rnorm(nrow(d), 170, 10)), aes(x = average_height, y = life_expectancy)) + 
  geom_point(size = 3, shape = 21, fill = "grey", alpha = .7) + 
  labs(x = "Average height", y = "Life expectancy") +
  theme_bw(base_size = 24) 

d |> select(child_mortality:daily_income) |> as.matrix() |> cor(use = "pairwise")

ggsave(filename = "lecture3/images/wooclap_q2_1.png", g1 + labs(x = NULL, y = NULL), width = 7, height = 7)
ggsave(filename = "lecture3/images/wooclap_q2_2.png", g2 + labs(x = NULL, y = NULL), width = 7, height = 7)
ggsave(filename = "lecture3/images/wooclap_q2_3.png", g4 + labs(x = NULL, y = NULL), width = 7, height = 7)

library(patchwork)
g123 <- g1 + g2 + g3
ggsave(filename = "lecture3/images/wooclap_q2.png", g123, width = 21, height = 7)
d
