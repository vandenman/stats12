# ---------------------------------------------------------------------------
# One-sample Z-tests, with a known population standard deviation.
#
# A Z-test applies the central limit theorem to a hypothesis about the mean:
# standardise the sample mean with
#
#   Z = (xbar - mu_0) / SE,   where SE = sigma / sqrt(n),
#
# and then read the p-value off the z table.
#
# The (sigma, n) combinations, the population means and the z values all come
# from R/clt.R, so the numbers here are exactly as tidy as the CLT questions:
# that file is always loaded, whichever topics the student selects.
# ---------------------------------------------------------------------------

# A Z-test setup: a tidy standard error, a population mean and a z value with
# a sign, plus the sample mean that goes with them.
#   direction = "two"     give z either sign
#   direction = "greater" give a positive z, for H1: mu > mu_0
#   direction = "less"    give a negative z, for H1: mu < mu_0
ztest_setup <- function(direction = "two") {
  setup <- pick_one(clt_setups(whole_se = TRUE))
  setup$mu <- pick_one(clt_means[clt_means > 2.5 * setup$se])

  magnitude <- pick_one(clt_interval_z())
  sign <- if (direction == "less") {
    -1
  } else if (direction == "greater") {
    1
  } else {
    pick_one(c(-1, 1))
  }

  setup$z <- sign * magnitude
  setup$xbar <- setup$mu + setup$z * setup$se
  setup
}

# The standard error, as it appears in a worked solution.
ztest_se_lines <- function(setup) {
  paste0(
    "  SE = sigma / sqrt(n) = ", setup$sigma, " / sqrt(", setup$n, ") = ",
    setup$sigma, " / ", round(sqrt(setup$n)), " = ", fmt_num(setup$se), "\n"
  )
}

# Difficulty 1: the standard error is given, so only the standardisation is
# left to do.
ztest_question_statistic <- function(difficulty) {
  setup <- ztest_setup()

  list(
    topic = "Z",
    type = "z_statistic_given_se",
    difficulty = difficulty,
    prompt = paste0(
      "A sample of n = ", setup$n, " observations has a mean of ",
      fmt_num(setup$xbar), ". The standard error of the mean is ",
      fmt_num(setup$se), ".\n\n",
      "We test H0: mu = ", setup$mu, " against H1: mu != ", setup$mu, ".\n\n",
      "Calculate the value of the Z statistic. Give your answer to two decimal places."
    ),
    answer = round(setup$z, 2),
    tolerance = 0.01,
    hint = "The Z statistic standardises the sample mean: Z = (xbar - mu) / SE.",
    solution = paste0(
      "Standardise the sample mean:\n\n",
      "  Z = (xbar - mu) / SE = (", fmt_num(setup$xbar), " - ", setup$mu,
      ") / ", fmt_num(setup$se), " = ", fmt_z(setup$z), "\n"
    )
  )
}

# Difficulty 2: the standard error has to be worked out first.
ztest_question_full <- function(difficulty) {
  setup <- ztest_setup()

  list(
    topic = "Z",
    type = "z_statistic",
    difficulty = difficulty,
    prompt = paste0(
      "A population has standard deviation sigma = ", setup$sigma,
      ". A sample of n = ", setup$n, " observations has a mean of ",
      fmt_num(setup$xbar), ".\n\n",
      "We test H0: mu = ", setup$mu, " against H1: mu != ", setup$mu, ".\n\n",
      "Calculate the value of the Z statistic. Give your answer to two decimal places."
    ),
    answer = round(setup$z, 2),
    tolerance = 0.01,
    hint = paste0(
      "Two steps: first the standard error, SE = sigma / sqrt(n), then ",
      "Z = (xbar - mu) / SE."
    ),
    solution = paste0(
      "First the standard error:\n\n",
      ztest_se_lines(setup), "\n",
      "Then standardise the sample mean:\n\n",
      "  Z = (xbar - mu) / SE = (", fmt_num(setup$xbar), " - ", setup$mu,
      ") / ", fmt_num(setup$se), " = ", fmt_z(setup$z), "\n"
    )
  )
}

# Difficulty 3: the whole test, up to and including the p-value.  The
# conclusion is in the worked solution, because the answer is a number.
ztest_question_pvalue <- function(difficulty) {
  direction <- pick_one(c("two", "greater", "less"))
  setup <- ztest_setup(direction = direction)
  magnitude <- abs(setup$z)
  upper_tail <- round_prob(1 - phi_from_table(magnitude))

  if (direction == "two") {
    answer <- round_prob(2 * upper_tail)
    alternative <- paste0("H1: mu != ", setup$mu)
    p_lines <- paste0(
      "  P(Z > ", fmt_z(magnitude), ") = 1 - Phi(", fmt_z(magnitude), ") = 1 - ",
      sprintf("%.4f", phi_from_table(magnitude)), " = ",
      sprintf("%.4f", upper_tail), "\n",
      "  two-tailed p = 2 * ", sprintf("%.4f", upper_tail), " = ",
      sprintf("%.4f", answer), "\n"
    )
  } else if (direction == "greater") {
    answer <- upper_tail
    alternative <- paste0("H1: mu > ", setup$mu)
    p_lines <- paste0(
      "  P(Z > ", fmt_z(magnitude), ") = 1 - Phi(", fmt_z(magnitude), ") = 1 - ",
      sprintf("%.4f", phi_from_table(magnitude)), " = ",
      sprintf("%.4f", answer), "\n"
    )
  } else {
    answer <- phi_from_table(-magnitude)
    alternative <- paste0("H1: mu < ", setup$mu)
    p_lines <- paste0("  ", phi_line(-magnitude), "\n")
  }

  conclusion <- if (answer < 0.05) "we reject H0" else "we do not reject H0"

  list(
    topic = "Z",
    type = paste0("p_value_", direction),
    difficulty = difficulty,
    prompt = paste0(
      "A population has standard deviation sigma = ", setup$sigma,
      ". A sample of n = ", setup$n, " observations has a mean of ",
      fmt_num(setup$xbar), ".\n\n",
      "We test H0: mu = ", setup$mu, " against ", alternative,
      ", using alpha = 0.05.\n\n",
      "Calculate the p-value. Give your answer to four decimal places."
    ),
    answer = answer,
    tolerance = 0.0015,
    hint = paste0(
      "Standardise first: SE = sigma / sqrt(n), then Z = (xbar - mu) / SE. ",
      "Then look up the area beyond Z in the z table. A two-tailed test uses ",
      "both tails, so double the area beyond the test statistic."
    ),
    solution = paste0(
      "First the standard error:\n\n",
      ztest_se_lines(setup), "\n",
      "Then the test statistic:\n\n",
      "  Z = (xbar - mu) / SE = (", fmt_num(setup$xbar), " - ", setup$mu,
      ") / ", fmt_num(setup$se), " = ", fmt_z(setup$z), "\n\n",
      "The p-value is the probability of a Z at least this extreme:\n\n",
      p_lines, "\n",
      "So p = ", sprintf("%.4f", answer), ". Since ",
      sprintf("%.4f", answer), " ", if (answer < 0.05) "<" else ">",
      " 0.05, ", conclusion, ".\n"
    )
  )
}

# One Z-test question.
#   difficulty 1: the Z statistic, with the standard error given
#   difficulty 2: the Z statistic, from sigma and n
#   difficulty 3: the p-value
generate_z_test_question <- function(difficulty = 1) {
  difficulty <- clamp(difficulty, 1, 3, 1)
  if (difficulty == 1) return(ztest_question_statistic(difficulty))
  if (difficulty == 2) return(ztest_question_full(difficulty))
  ztest_question_pvalue(difficulty)
}
