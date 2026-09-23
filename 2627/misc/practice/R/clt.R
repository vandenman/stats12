# ---------------------------------------------------------------------------
# Central limit theorem: the sampling distribution of the mean.
#
# The standard error is built first, and sigma follows from it with
# sigma = SE * sqrt(n).  Thresholds are then built from a nice z value with
# xbar = mu + z * SE, so the numbers in a question stay tidy.  The worked
# solution always shows SE and z explicitly, because that is the point of the
# exercise.
# ---------------------------------------------------------------------------

# Sample sizes with a whole-number square root.
clt_sample_sizes <- c(4, 9, 16, 25, 36, 49, 64, 100, 144)

# Candidate population standard deviations.
clt_sigmas <- c(12, 20, 24, 30, 36, 40, 50, 60, 100, 120)

# Population means.
clt_means <- c(50, 60, 80, 100, 120, 200, 500)

# z values for the probability questions: tenths, so that z * SE is tidy, and
# never close to 0 (a probability of 0.5 is not worth practising).
clt_interval_z <- function() round(seq(0.3, 2.5, by = 0.1), 1)
clt_z_values <- round(c(seq(-2.5, -0.3, by = 0.1), clt_interval_z()), 1)

# All (sigma, n) combinations whose standard error is a whole number or a
# half.  With whole-number standard errors the thresholds stay tidy too,
# which is what the probability questions need.
clt_setups <- function(whole_se = FALSE) {
  setups <- list()
  for (n in clt_sample_sizes) {
    for (sigma in clt_sigmas) {
      se <- sigma / sqrt(n)
      if (se < 1) next
      if (se * 2 != round(se * 2)) next
      if (whole_se && se != round(se)) next
      setups[[length(setups) + 1]] <- list(sigma = sigma, n = n, se = se)
    }
  }
  setups
}

# One question about the sampling distribution of the mean.
#   difficulty 1: work out the standard error
#   difficulty 2: a probability about the sample mean
#   difficulty 3: a large sample, so the central limit theorem really is what
#                 makes the sample mean normal, plus an interval or a
#                 "work backwards" question
generate_clt_question <- function(difficulty = 1) {
  difficulty <- clamp(difficulty, 1, 3, 1)

  if (difficulty == 1) {
    setup <- pick_one(clt_setups())
  } else if (difficulty == 2) {
    setup <- pick_one(clt_setups(whole_se = TRUE))
  } else {
    large_sample <- Filter(function(s) s$n >= 36, clt_setups(whole_se = TRUE))
    setup <- pick_one(large_sample)
  }
  # Keep the mean comfortably larger than any threshold we might build from
  # it, so that sample means in the questions stay sensible values.
  setup$mu <- pick_one(clt_means[clt_means > 2.5 * setup$se])

  if (difficulty == 1) {
    clt_question_se(setup, difficulty)
  } else if (difficulty == 2) {
    clt_question_probability(setup, difficulty)
  } else if (pick_one(c("interval", "reverse")) == "interval") {
    clt_question_interval(setup, difficulty)
  } else {
    clt_question_reverse(setup, difficulty)
  }
}

# The standard error, as it appears in every worked solution.
clt_se_lines <- function(setup) {
  paste0(
    "The standard error of the sample mean is\n\n",
    "  SE = sigma / sqrt(n) = ", setup$sigma, " / sqrt(", setup$n, ") = ",
    setup$sigma, " / ", round(sqrt(setup$n)), " = ", fmt_num(setup$se), "\n"
  )
}

# The sentence that says what distribution we are using.  For the large
# samples this really is the central limit theorem; for the smaller ones we
# simply say the population is normal.
clt_distribution_lines <- function(setup, large_sample) {
  if (large_sample) {
    paste0(
      "The population is not normal, but the sample is large (n = ", setup$n,
      "), so by the central limit theorem the sample mean Xbar is ",
      "approximately normally distributed, with\n\n",
      "  mean mu = ", setup$mu, " and standard deviation SE = ",
      fmt_num(setup$se), ".\n"
    )
  } else {
    paste0(
      "The population is normally distributed, so the sample mean Xbar is ",
      "normally distributed as well, with\n\n",
      "  mean mu = ", setup$mu, " and standard deviation SE = ",
      fmt_num(setup$se), ".\n"
    )
  }
}

# Difficulty 1: just the standard error.
clt_question_se <- function(setup, difficulty) {
  list(
    topic = "clt",
    type = "standard_error",
    difficulty = difficulty,
    prompt = paste0(
      "A population has mean mu = ", setup$mu, " and standard deviation ",
      "sigma = ", setup$sigma, ". A random sample of n = ", setup$n,
      " observations is taken. Calculate the standard error of the sample ",
      "mean. Give your answer to two decimal places."
    ),
    answer = setup$se,
    tolerance = 0.01,
    hint = paste0(
      "The standard error is the standard deviation of the sample mean: ",
      "SE = sigma / sqrt(n)."
    ),
    solution = paste0(
      clt_se_lines(setup), "\n",
      "So SE = ", fmt_num(setup$se), ".\n"
    )
  )
}

# Difficulty 2: a probability about the sample mean.
clt_question_probability <- function(setup, difficulty) {
  z <- pick_one(clt_z_values)
  threshold <- round(setup$mu + z * setup$se, 1)
  z <- round((threshold - setup$mu) / setup$se, 2)
  phi <- phi_from_table(z)

  if (pick_one(c("above", "below")) == "above") {
    answer <- round_prob(1 - phi)
    asked <- paste0("P(Xbar > ", fmt_num(threshold), ")")
    working <- paste0("P(Xbar > ", fmt_num(threshold), ") = 1 - Phi(",
                      fmt_z(z), ")")
    last_line <- paste0("= 1 - ", sprintf("%.4f", phi), " = ",
                        sprintf("%.4f", answer))
  } else {
    answer <- phi
    asked <- paste0("P(Xbar < ", fmt_num(threshold), ")")
    working <- paste0("P(Xbar < ", fmt_num(threshold), ") = Phi(", fmt_z(z), ")")
    last_line <- paste0("= ", sprintf("%.4f", answer))
  }

  list(
    topic = "clt",
    type = "sample_mean_probability",
    difficulty = difficulty,
    prompt = paste0(
      "A normally distributed population has mean mu = ", setup$mu,
      " and standard deviation sigma = ", setup$sigma, ". A random sample of ",
      "n = ", setup$n, " observations is taken, with sample mean Xbar. ",
      "Calculate ", asked, ". Give your answer to four decimal places."
    ),
    answer = answer,
    tolerance = 0.001,
    hint = paste0(
      "Two steps: work out the standard error SE = sigma / sqrt(n) first, ",
      "then standardise with z = (Xbar - mu) / SE."
    ),
    solution = paste0(
      clt_se_lines(setup), "\n",
      clt_distribution_lines(setup, large_sample = FALSE), "\n",
      "Standardise the value:\n\n",
      "  z = (", fmt_num(threshold), " - ", setup$mu, ") / ",
      fmt_num(setup$se), " = ", fmt_z(z), "\n\n",
      "Look z up in the z table:\n\n",
      "  ", phi_line(z), "\n\n",
      "Therefore:\n\n",
      "  ", working, "\n",
      "  ", last_line, "\n"
    )
  )
}

# Difficulty 3, first variant: an interval for the sample mean.
clt_question_interval <- function(setup, difficulty) {
  # The upper limit is drawn first, and the lower one from the remaining
  # values, so that a and b are never the same number.
  z_high <- pick_one(clt_interval_z())
  z_low <- -pick_one(clt_interval_z()[clt_interval_z() != z_high])

  a <- round(setup$mu + z_low * setup$se, 1)
  b <- round(setup$mu + z_high * setup$se, 1)
  z_low <- round((a - setup$mu) / setup$se, 2)
  z_high <- round((b - setup$mu) / setup$se, 2)

  phi_low <- phi_from_table(z_low)
  phi_high <- phi_from_table(z_high)
  answer <- round_prob(phi_high - phi_low)

  list(
    topic = "clt",
    type = "sample_mean_interval",
    difficulty = difficulty,
    prompt = paste0(
      "A population has mean mu = ", setup$mu, " and standard deviation ",
      "sigma = ", setup$sigma, ". The population distribution is not normal, ",
      "but the sample is large: n = ", setup$n, ". The sample mean is Xbar. ",
      "Use the central limit theorem to calculate P(", fmt_num(a),
      " < Xbar < ", fmt_num(b), "). Give your answer to four decimal places."
    ),
    answer = answer,
    tolerance = 0.001,
    hint = paste0(
      "Find SE = sigma / sqrt(n), then standardise both limits with ",
      "z = (Xbar - mu) / SE, and use ",
      "P(a < Xbar < b) = Phi(z_upper) - Phi(z_lower)."
    ),
    solution = paste0(
      clt_se_lines(setup), "\n",
      clt_distribution_lines(setup, large_sample = TRUE), "\n",
      "Standardise both limits:\n\n",
      "  z_lower = (", fmt_num(a), " - ", setup$mu, ") / ", fmt_num(setup$se),
      " = ", fmt_z(z_low), "\n",
      "  z_upper = (", fmt_num(b), " - ", setup$mu, ") / ", fmt_num(setup$se),
      " = ", fmt_z(z_high), "\n\n",
      "Look both up in the z table:\n\n",
      "  ", phi_line(z_low), "\n",
      "  ", phi_line(z_high), "\n\n",
      "Therefore:\n\n",
      "  P(", fmt_num(a), " < Xbar < ", fmt_num(b), ")\n",
      "  = Phi(", fmt_z(z_high), ") - Phi(", fmt_z(z_low), ")\n",
      "  = ", sprintf("%.4f", phi_high), " - ", sprintf("%.4f", phi_low), "\n",
      "  = ", sprintf("%.4f", answer), "\n"
    )
  )
}

# Difficulty 3, second variant: the probability is given and a value of the
# sample mean is asked for.
clt_question_reverse <- function(setup, difficulty) {
  level <- pick_one(z_percentiles)
  p <- level$p
  z <- level$z
  threshold <- round(setup$mu + z * setup$se, 2)

  if (z >= 0) {
    lookup <- paste0("  The table gives Phi(", fmt_z(z), ") = ",
                     sprintf("%.4f", phi_from_table(z)), ", the entry closest ",
                     "to ", sprintf("%.2f", p), ", so z = ", fmt_z(z), ".\n")
    conversion <- paste0("  c = mu + z * SE = ", setup$mu, " + ",
                         fmt_z(abs(z)), " * ", fmt_num(setup$se), " = ",
                         fmt_num(threshold))
  } else {
    lookup <- paste0("  The table only lists nonnegative z.  Phi(",
                     fmt_z(abs(z)), ") = ",
                     sprintf("%.4f", phi_from_table(abs(z))), ", so by ",
                     "symmetry Phi(", fmt_z(z), ") = 1 - ",
                     sprintf("%.4f", phi_from_table(abs(z))), " = ",
                     sprintf("%.4f", phi_from_table(z)), ", the entry closest ",
                     "to ", sprintf("%.2f", p), ", so z = ", fmt_z(z), ".\n")
    conversion <- paste0("  c = mu + z * SE = ", setup$mu, " - ",
                         fmt_z(abs(z)), " * ", fmt_num(setup$se), " = ",
                         fmt_num(threshold))
  }

  list(
    topic = "clt",
    type = "sample_mean_reverse",
    difficulty = difficulty,
    prompt = paste0(
      "A population has mean mu = ", setup$mu, " and standard deviation ",
      "sigma = ", setup$sigma, ". The population distribution is not normal, ",
      "but the sample is large: n = ", setup$n, ". The sample mean is Xbar. ",
      "Use the central limit theorem to find the value c for which P(Xbar < c) = ",
      sprintf("%.2f", p), ". Use the closest z value in the z table, and give ",
      "your answer to two decimal places."
    ),
    answer = threshold,
    tolerance = 0.5,
    hint = paste0(
      "Find SE = sigma / sqrt(n) first. Then search the body of the z table ",
      "for the entry closest to the probability, read off z, and convert back ",
      "with c = mu + z * SE."
    ),
    solution = paste0(
      clt_se_lines(setup), "\n",
      clt_distribution_lines(setup, large_sample = TRUE), "\n",
      "We need the z value whose table entry is closest to ",
      sprintf("%.2f", p), ".\n\n",
      lookup, "\n",
      "Now convert z back to the scale of the sample mean:\n\n",
      conversion, "\n\n",
      "So P(Xbar < ", fmt_num(threshold), ") = ", sprintf("%.2f", p),
      " to the accuracy of the table.\n"
    )
  )
}
