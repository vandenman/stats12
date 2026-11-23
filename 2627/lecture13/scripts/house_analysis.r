# Run from the 2627 project directory with Rscript lecture13/scripts/house_analysis.r.
library(ggplot2)

raw_data <- read.csv(
  file.path("lecture13", "data", "rtf74-osfstorage-archive", "harry_all.csv"),
  sep = ";"
)

# Match the age and single-house filters in data_example.r.
houses <- c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin")
age <- ifelse(raw_data$age == 1987, 2016 - 1987, raw_data$age)
house_data <- data.frame(
  House = factor(raw_data$Sorting_house, levels = houses),
  Machiavellianism = raw_data$SD3_Machiavellianism
)
house_data <- house_data[which(age >= 18 & age <= 100 & complete.cases(house_data)), ]

fit <- lm(Machiavellianism ~ House, data = house_data,
          contrasts = list(House = contr.treatment(houses)))
fit_summary <- summary(fit)

model_summary <- data.frame(
  R = sqrt(fit_summary$r.squared),
  R_Square = fit_summary$r.squared,
  Adjusted_R_Square = fit_summary$adj.r.squared,
  Std_Error_of_the_Estimate = fit_summary$sigma
)
print(model_summary, digits = 7, row.names = FALSE)

# SPSS reports standardized slopes for the individual dummy variables.
dummy_sds <- apply(model.matrix(fit)[, -1], 2, sd)
standardized_slopes <- coef(fit)[-1] * dummy_sds / sd(house_data$Machiavellianism)
coefficients <- data.frame(
  House = c("(Constant)", houses[-1]),
  B = coef(fit_summary)[, "Estimate"],
  Std_Error = coef(fit_summary)[, "Std. Error"],
  Beta = c(NA, standardized_slopes),
  t = coef(fit_summary)[, "t value"],
  Sig = coef(fit_summary)[, "Pr(>|t|)"],
  row.names = NULL
)
print(coefficients, digits = 7, row.names = FALSE)
print(fit_summary$fstatistic)
print(anova(fit), digits = 7)
print(sum((house_data$Machiavellianism - mean(house_data$Machiavellianism))^2))

print(aggregate(Machiavellianism ~ House, house_data,
                function(scores) c(N = length(scores), Mean = mean(scores), SD = sd(scores))))
# SPSS's default Levene test uses absolute deviations from group means.
print(car::leveneTest(fit, center = mean))

house_colors <- c(
  Gryffindor = "#7F0909",
  Hufflepuff = "#f0c75e",
  Ravenclaw = "#08245e",
  Slytherin = "#2a623d"
)
house_data$Residual <- residuals(fit)
qq_plot <- ggplot(house_data, aes(sample = Residual)) +
  stat_qq_line(color = "grey45") +
  stat_qq(aes(fill = House), shape = 21, size = 2, stroke = 0.25) +
  facet_wrap(vars(House), nrow = 1) +
  scale_fill_manual(values = house_colors, guide = "none") +
  labs(x = "Theoretical normal quantiles", y = "Residual quantiles") +
  theme_bw(base_size = 24) +
  theme(strip.text = element_text(face = "plain"))

ggsave(file.path("lecture13", "figures", "hp_mach_qq.svg"), qq_plot,
       width = 40, height = 13, units = "cm")

house_data$Predicted <- fitted(fit)
house_data$Standardized_Residual <- rstandard(fit)
residual_plot <- ggplot(house_data, aes(Predicted, Standardized_Residual, colour = House)) +
  geom_hline(yintercept = 0, colour = "grey55", linetype = "dashed") +
  geom_point(size = 2, alpha = 0.65) +
  scale_colour_manual(values = house_colors) +
  labs(x = "Predicted Machiavellianism", y = "Standardized residual") +
  theme_bw(base_size = 22) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave(file.path("lecture13", "figures", "hp_mach_resid_vs_predicted.svg"), residual_plot,
       width = 32, height = 13, units = "cm")
