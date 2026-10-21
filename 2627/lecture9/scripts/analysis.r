root <- "lecture9/data/"
file <- file.path(root, 'Data.csv')
if (!file.exists(file)) stop('Download Study1_raw.csv from Dryad and put it in the working directory.')
d <- read.csv(file, check.names = FALSE)

head(d)
dim(d)

# Sample composition
dim(d)
table(d$Country, useNA = "ifany")
table(d$`Filter 2`, useNA = "ifany")

# Available composite scores
vars <- c(
  "NPQC-R",
  "NPI-13",
  "Social Media Use",
  "SCA",
  "Active Facebook Use",
  "FB Years",
  "Facebook Friends"
)

sapply(d[vars], class)

# Available observations
colSums(!is.na(d[vars]))

data.frame(
  Variable = vars,
  N = sapply(d[vars], function(x) sum(!is.na(x))),
  Mean = sapply(d[vars], function(x) mean(x, na.rm = TRUE)),
  SD = sapply(d[vars], function(x) sd(x, na.rm = TRUE))
)

check_cor <- function(x, y) {
  ok <- complete.cases(x, y)
  
  test <- cor.test(x[ok], y[ok])
  
  c(
    N = sum(ok),
    r = unname(test$estimate),
    t = unname(test$statistic),
    df = unname(test$parameter),
    p = test$p.value
  )
}

rbind(
  NPQC_R = check_cor(d$`Social Media Use`, d$`NPQC-R`),
  NPI_13 = check_cor(d$`Social Media Use`, d$`NPI-13`),
  SCA_NPQC = check_cor(d$SCA, d$`NPQC-R`),
  SCA_NPI = check_cor(d$SCA, d$`NPI-13`)
)


dat <- na.omit(d[c("SCA", "NPI-13")])

fit1 <- lm(`NPI-13` ~ SCA, data = dat)
fit2 <- lm(SCA ~ `NPI-13`, data = dat)

summary(fit1)
summary(fit2)

cor(dat$SCA, dat$`NPI-13`)

c(
  R2_forward = summary(fit1)$r.squared,
  R2_reverse = summary(fit2)$r.squared
)

library(ggplot2)
library(patchwork)

dat <- na.omit(d[c("SCA", "NPI-13")])
fit <- lm(`NPI-13` ~ SCA, data = dat)

p1 <- ggplot(dat, aes(SCA, `NPI-13`)) +
  geom_jitter(width = 0.04, height = 0.08, alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Self-centered appearance use and narcissism",
    x = "Self-centered appearance use",
    y = "Narcissism (NPI-13)"
  ) +
  theme_bw(base_size = 14)

diag <- data.frame(
  fitted = fitted(fit),
  residual = residuals(fit)
)

p2 <- ggplot(diag, aes(fitted, residual)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals versus fitted values",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_bw(base_size = 14)

p3 <- ggplot(
  data.frame(residual = rstandard(fit)),
  aes(sample = residual)
) +
  stat_qq(alpha = 0.5) +
  stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q plot") +
  theme_bw(base_size = 14)

(p1 | p2) / p3
