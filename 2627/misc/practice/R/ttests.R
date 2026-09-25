# ---------------------------------------------------------------------------
# One-sample t-tests.
#
# The same logic as the Z-test, but the population standard deviation is
# unknown and is replaced by the sample standard deviation, so the test
# statistic follows a t distribution with n - 1 degrees of freedom:
#
#   t = (xbar - mu_0) / (s / sqrt(n))
#
# Critical values are looked up in tables/t-table.csv, which is shown on the
# page.  Sample sizes are kept at 31 or below, so that n - 1 is always a row
# of that table.
# ---------------------------------------------------------------------------

# Sample sizes with a whole-number square root, so that s / sqrt(n) is tidy.
ttest_sample_sizes <- c(9, 16, 25)

# Sample standard deviations that give a tidy standard error.
ttest_sds <- c(6, 9, 10, 12, 15, 20, 24, 30)

# The two-tailed significance levels, as the columns of the t table.
ttest_alphas <- c(0.05, 0.02, 0.01, 0.10)

# The t values used for the test statistics: tenths, so that t * SE is tidy.
ttest_values <- round(seq(0.3, 3, by = 0.1), 1)

# All (n, s) combinations whose standard error is a whole number or a half.
ttest_setups <- function() {
  setups <- list()
  for (n in ttest_sample_sizes) {
    for (s in ttest_sds) {
      se <- s / sqrt(n)
      if (se < 1) next
      if (se * 2 != round(se * 2)) next
      setups[[length(setups) + 1]] <- list(n = n, s = s, se = se, df = n - 1)
    }
  }
  setups
}

# The critical value, to three decimals, exactly as the table shows it.
ttest_critical <- function(df, alpha) {
  round(qt(1 - alpha / 2, df), 3)
}

# One t-test question.
#   difficulty 1: the t statistic from the sample mean and standard deviation
#   difficulty 2: the critical value, from the t table
#   difficulty 3: the t statistic, starting from the raw observations
generate_t_test_question <- function(difficulty = 1) {
  difficulty <- clamp(difficulty, 1, 3, 1)
  if (difficulty == 1) return(ttest_question_statistic(difficulty))
  if (difficulty == 2) return(ttest_question_critical(difficulty))
  ttest_question_data(difficulty)
}

# The closing lines of a solution: the degrees of freedom, the critical value
# and what it means.
ttest_conclusion_lines <- function(df, t_stat) {
  critical <- ttest_critical(df, 0.05)
  conclusion <- if (abs(t_stat) > critical) "we reject H0" else "we do not reject H0"

  paste0(
    "There are n - 1 = ", df, " degrees of freedom. The critical value for a ",
    "two-tailed test at alpha = 0.05 is ", sprintf("%.3f", critical),
    "; since |t| = ", sprintf("%.2f", abs(t_stat)),
    if (abs(t_stat) > critical) " > " else " < ",
    sprintf("%.3f", critical), ", ", conclusion, ".\n"
  )
}

# Difficulty 1: the sample mean and standard deviation are given.
ttest_question_statistic <- function(difficulty) {
  setup <- pick_one(ttest_setups())
  mu <- pick_one(clt_means[clt_means > 3 * setup$se])
  t_stat <- pick_one(ttest_values) * pick_one(c(-1, 1))
  xbar <- mu + t_stat * setup$se

  list(
    topic = "t",
    type = "t_statistic",
    difficulty = difficulty,
    prompt = paste0(
      "A sample of n = ", setup$n, " observations has a mean of ", fmt_num(xbar),
      " and a standard deviation of ", fmt_num(setup$s), ".\n\n",
      "We test H0: mu = ", mu, " against H1: mu != ", mu, ".\n\n",
      "Calculate the value of the t statistic. Give your answer to two decimal places."
    ),
    answer = round(t_stat, 2),
    tolerance = 0.01,
    hint = paste0(
      "The t statistic works the same way as Z, but with the sample standard ",
      "deviation: first SE = s / sqrt(n), then t = (xbar - mu) / SE."
    ),
    solution = paste0(
      "First the standard error:\n\n",
      "  SE = s / sqrt(n) = ", fmt_num(setup$s), " / sqrt(", setup$n, ") = ",
      fmt_num(setup$s), " / ", round(sqrt(setup$n)), " = ", fmt_num(setup$se), "\n\n",
      "Then the test statistic:\n\n",
      "  t = (xbar - mu) / SE = (", fmt_num(xbar), " - ", mu, ") / ",
      fmt_num(setup$se), " = ", sprintf("%.2f", t_stat), "\n\n",
      ttest_conclusion_lines(setup$df, t_stat)
    )
  )
}

# Difficulty 2: a table look-up.  The student has to work out the degrees of
# freedom before they can find the row.
ttest_question_critical <- function(difficulty) {
  n <- pick_one(c(9, 10, 16, 17, 20, 25, 26, 31))
  df <- n - 1
  alpha <- pick_one(ttest_alphas)
  critical <- ttest_critical(df, alpha)

  list(
    topic = "t",
    type = "critical_value",
    difficulty = difficulty,
    prompt = paste0(
      "A study uses a sample of n = ", n, " observations and tests H0: mu = 100 ",
      "against H1: mu != 100 at alpha = ", sprintf("%.2f", alpha),
      " (two-tailed).\n\n",
      "Look up the critical value t* in the t table. Give your answer to three ",
      "decimal places."
    ),
    answer = critical,
    tolerance = 0.002,
    hint = paste0(
      "The t table is indexed by the degrees of freedom, which is n - 1 here, ",
      "and by the two-tailed significance level alpha."
    ),
    solution = paste0(
      "The degrees of freedom are n - 1 = ", n, " - 1 = ", df, ".\n\n",
      "In the t table, the row for df = ", df, " and the column for a two-tailed ",
      "alpha = ", sprintf("%.2f", alpha), " give t* = ",
      sprintf("%.3f", critical), ".\n"
    )
  )
}

# Difficulty 3: the observations themselves are given, so the mean, the sum of
# squared deviations and the standard deviation all have to be worked out
# first.  The data are built symmetrically around the mean, which makes the
# standard deviation a whole number and the whole calculation checkable by hand.
ttest_question_data <- function(difficulty) {
  n <- 9
  step <- pick_one(c(3, 6))                # the sample standard deviation
  centre <- pick_one(c(50, 100, 150, 200)) # the sample mean
  half <- (n - 1) / 2
  data <- sample(centre + c(rep(-step, half), 0, rep(step, half)))

  xbar <- mean(data)
  ss <- sum((data - xbar)^2)
  s <- sqrt(ss / (n - 1))
  se <- s / sqrt(n)
  df <- n - 1

  t_stat <- pick_one(c(1.5, 2, 2.5, 3)) * pick_one(c(-1, 1))
  mu <- round(xbar - t_stat * se, 1)
  t_actual <- (xbar - mu) / se

  list(
    topic = "t",
    type = "t_statistic_from_data",
    difficulty = difficulty,
    prompt = paste0(
      "A sample of ", n, " observations gave the following values:\n\n",
      "  ", paste(data, collapse = "   "), "\n\n",
      "We test H0: mu = ", fmt_num(mu), " against H1: mu != ", fmt_num(mu), ".\n\n",
      "Calculate the value of the t statistic. Give your answer to two decimal places."
    ),
    answer = round(t_actual, 2),
    tolerance = 0.02,
    hint = paste0(
      "Start from the data: the sample mean first, then the sum of squared ",
      "deviations SS = sum((x - xbar)^2), then the sample standard deviation ",
      "s = sqrt(SS / (n - 1)). Finally SE = s / sqrt(n) and t = (xbar - mu) / SE."
    ),
    solution = paste0(
      "The sample mean:\n\n",
      "  sum(x) = ", fmt_num(sum(data)), ", so xbar = ", fmt_num(sum(data)),
      " / ", n, " = ", fmt_num(xbar), "\n\n",
      "The sum of squared deviations from the mean, and the sample standard ",
      "deviation:\n\n",
      "  SS = ", fmt_num(ss), "\n",
      "  s^2 = SS / (n - 1) = ", fmt_num(ss), " / ", df, " = ",
      fmt_num(ss / df), "\n",
      "  s = sqrt(", fmt_num(ss / df), ") = ", fmt_num(s), "\n\n",
      "Then the standard error and the test statistic:\n\n",
      "  SE = s / sqrt(n) = ", fmt_num(s), " / ", round(sqrt(n)), " = ",
      fmt_num(se), "\n",
      "  t = (xbar - mu) / SE = (", fmt_num(xbar), " - ", fmt_num(mu), ") / ",
      fmt_num(se), " = ", sprintf("%.2f", t_actual), "\n\n",
      ttest_conclusion_lines(df, t_actual)
    )
  )
}
