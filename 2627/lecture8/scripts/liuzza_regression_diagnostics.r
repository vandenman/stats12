# Liuzza et al. (2018): BODS -> Right-Wing Authoritarianism, Study 1
# Original raw data: https://datadryad.org/dataset/doi:10.5061/dryad.6t536
# Download Study1_raw.csv and place next to this script.
# Raw data require scale scoring from README_for_Study1_raw.rtf when no composite variables exist.

root <- "lecture8/data/"
file <- file.path(root, 'Study1_raw.csv')
if (!file.exists(file)) stop('Download Study1_raw.csv from Dryad and put it in the working directory.')
d <- read.csv(file, check.names = FALSE)

regular <- paste0("RWA", seq(1, 15, 2))
reverse <- paste0("RWA", seq(2, 14, 2), "_R")

# Run on the original data, before any recoding
cor(
  rowMeans(d[regular], na.rm = TRUE),
  rowMeans(d[reverse], na.rm = TRUE),
  use = "complete.obs"
)
library(dplyr)

bods_items <- paste0("BODS", 1:12)

rwa_items <- c(
  paste0("RWA", seq(1, 15, 2)),
  paste0("RWA", seq(2, 14, 2), "_R")
)
reverse_items <- paste0("RWA", seq(2, 14, 2), "_R")

d <- d |>
  mutate(across(all_of(reverse_items), ~ 8 - .x))

d <- d |>
  mutate(
    
    BODS = rowMeans(across(all_of(bods_items)), na.rm = TRUE),
    RWA  = rowMeans(across(all_of(rwa_items)), na.rm = TRUE)
  )

# IMPORTANT: Verify variable names and scoring against the authors' codebook.
# If only questionnaire ITEMS are present, calculate composite scores using their scoring key.
# Do not guess which items are reverse coded.
BODS_COLUMN <- 'BODS'
RWA_COLUMN  <- 'RWA'

z <- data.frame(bods = as.numeric(d[[BODS_COLUMN]]), rwa  = as.numeric(d[[RWA_COLUMN]]))
z <- na.omit(z)
fit <- lm(rwa ~ bods, data = z)
cat('\nN complete cases:', nrow(z), '\n')
print(summary(fit)); print(confint(fit))
cat('\nCorrelation:', cor(z$bods,z$rwa), '\n')

pt <- geom_point(shape = 21, fill = "grey")
thm <- theme_bw(base_size = 24)

ggscatter <- ggplot(z, aes(x = bods, y = rwa)) +
  pt +
  geom_abline(slope = coef(fit)[2], intercept = coef(fit)[1]) +
  thm +
  labs(x='Body odour disgust sensitivity',y='Right-wing authoritarianism', title='Observed relationship')

ggsave("lecture8/figures/example_scatter.svg", ggscatter)

ggresidual <- ggplot(data.frame(f = fitted(fit),r = residuals(fit)), aes(x = f, y = r)) +
  pt + thm +
  geom_smooth() +
  labs(x='Fitted authoritarianism',y='Residual',title='Linearity and equal variance')

ggsave("lecture8/figures/example_residual.svg", ggresidual)

library(ggplot2)

res <- data.frame(residuals = rstandard(fit))

gghist <- ggplot(res, aes(x = residuals)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 20, fill = "steelblue",
                 color = "white") +
  stat_function(
    fun = dnorm,
    args = list(mean = 0, sd = 1),
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Normality: Histogram",
    x = "Standardized residuals",
    y = "Density"
  ) +
  thm

res <- sort(na.omit(rstandard(fit)))
n <- length(res)

pp <- data.frame(
  theoretical = pnorm(res),
  observed = ppoints(n)
)

ggpp <- ggplot(pp, aes(x = theoretical, y = observed)) +
  geom_point(size = 1.5) +
  geom_abline(
    intercept = 0, slope = 1,
    color = "red", linewidth = 1
  ) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title = "Normality: P-P plot",
    x = "Expected cumulative probability",
    y = "Observed cumulative probability"
  ) +
  thm

ggqq <- ggplot(data.frame(residuals = rstandard(fit)),
  aes(sample = residuals)) +
  stat_qq(size = 1.5) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(
    title = "Normality: Q-Q plot",
    x = "Theoretical quantiles",
    y = "Standardized residuals"
  ) +
  thm

gg3 <- gghist + ggpp + ggqq
w <- 7
ggsave(filename = "lecture8/figures/example_assumptions.svg", width = 3*w, height = w, plot = gg3)

n <- 150
set.seed(42)
df_good <- tibble::tibble(
  x = runif(n, -5, 5),
  y = scale(rnorm(n))
)
df_bad <- tibble::tibble(
  x = runif(n, -5, 5),
  y = scale(rnorm(n, 0, ((x + 5)^2 + .001)^(1/1.75)))
)
g1 <- ggplot(df_good, aes(x = x, y = y)) + geom_point(shape = 21, fill = "grey") + thm
g2 <- ggplot(df_bad,  aes(x = x, y = y)) + geom_point(shape = 21, fill = "grey") + thm

g12 <- (
  (g1 + ggtitle("Homoscedastic")) +
  (g2 + ggtitle("Heteroscedastic"))
)
g12
ggsave("lecture8/figures/homoheteroscedastic.svg", width = 10, height = 5, plot = g12)

par(mfrow=c(2,2))
plot(z$bods,z$rwa, pch=16, cex=.7, col=rgb(0,0,0,.3),
     xlab='Body odour disgust sensitivity',ylab='Right-wing authoritarianism', main='Observed relationship')
abline(fit, lwd=2, col='steelblue')
plot(fitted(fit),residuals(fit),pch=16,cex=.7,col=rgb(0,0,0,.3),
     xlab='Fitted authoritarianism',ylab='Residual',main='Linearity and equal variance')
abline(h=0,lty=2); lines(lowess(fitted(fit),residuals(fit)),col='red',lwd=2)
qqnorm(rstandard(fit),pch=16,cex=.7,main='Normality: Q-Q plot'); qqline(rstandard(fit),col='red',lwd=2)
plot(fitted(fit),sqrt(abs(rstandard(fit))),pch=16,cex=.7,
     xlab='Fitted authoritarianism',ylab='sqrt(|standardized residual|)',main='Scale-location (homoscedasticity)')
lines(lowess(fitted(fit),sqrt(abs(rstandard(fit)))),col='red',lwd=2)
dev.off()

# Auxiliary regression: Breusch-Pagan LM for a single predictor without external packages
resid2 <- residuals(fit)^2
aux <- lm(resid2 ~ fitted(fit))
LM <- nrow(z)*summary(aux)$r.squared
cat('\nBreusch-Pagan LM statistic:',LM,'p =',pchisq(LM, df=1, lower.tail=FALSE),'\n')
cat('Shapiro-Wilk residual test (sensitive to departures for larger N):\n')
print(shapiro.test(residuals(fit)))
cd <- cooks.distance(fit)
cat('Largest Cook distance:',max(cd),'\n')
cat('Number with Cook distance > 4/N:',sum(cd>4/nrow(z)),'\n')
cat('Reminder: tests do not substitute for visual diagnostics; ordinal composite scales are bounded.\n')

