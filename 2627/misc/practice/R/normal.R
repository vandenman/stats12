# ---------------------------------------------------------------------------
# Normal distribution, solved with the z table.
#
# The z value is chosen first, from the hundredths grid of a standard table,
# and x is built from it as x = mu + z * sigma.  Working in that direction is
# what keeps the exercises tidy: every answer can be read off the z table
# without any awkward arithmetic in between.
# ---------------------------------------------------------------------------

# z values on the table grid.  We stay inside |z| <= 2.5, so that the
# probabilities are not so close to 0 or 1 that the table tells us nothing,
# and we skip z near 0, where a "what is P(X < mu)" question would be too
# easy to be interesting.
normal_z_values <- function() {
  z <- round(seq(-2.5, 2.5, by = 0.05), 2)
  z[abs(z) >= 0.2]
}

# z values for the two limits of an interval question.
normal_interval_z <- function() {
  round(seq(0.2, 2.5, by = 0.05), 2)
}

# Means and standard deviations that give tidy x values: a 0.05 step times
# any of these standard deviations has at most one decimal.
normal_means <- c(40, 50, 60, 70, 80, 100, 120)
normal_sds <- c(8, 10, 12, 20, 40, 100)

# Mean and standard deviation for the ordinary questions.  We keep
# mu > 2.5 * sigma, so that even the most extreme value of x in a question is
# still a sensible number rather than something far below zero.
normal_mu_sigma <- function() {
  pairs <- expand.grid(mu = normal_means, sigma = normal_sds)
  pairs <- pairs[pairs$mu > 2.5 * pairs$sigma, , drop = FALSE]
  pair <- pairs[pick_one(seq_len(nrow(pairs))), ]
  list(mu = pair$mu, sigma = pair$sigma)
}

# For the "work backwards" questions z comes from the hundredths grid (1.28,
# 2.33, ...) rather than the 0.05 one, so we use standard deviations that keep
# z * sigma tidy, and means that keep x positive.
normal_percentile_means <- c(250, 300, 400, 500, 600, 800)
normal_percentile_sds <- c(10, 20, 50, 100)

normal_percentile_mu_sigma <- function() {
  list(mu = pick_one(normal_percentile_means),
       sigma = pick_one(normal_percentile_sds))
}

# One question about the normal distribution.
#   difficulty 1: one look-up, one tail
#   difficulty 2: an interval, two look-ups
#   difficulty 3: the probability is given, x is asked for
generate_normal_question <- function(difficulty = 1) {
  difficulty <- clamp(difficulty, 1, 3, 1)

  if (difficulty == 3) {
    pair <- normal_percentile_mu_sigma()
  } else {
    pair <- normal_mu_sigma()
  }

  if (difficulty == 1) {
    normal_question_one_sided(pair$mu, pair$sigma, difficulty)
  } else if (difficulty == 2) {
    normal_question_interval(pair$mu, pair$sigma, difficulty)
  } else {
    normal_question_percentile(pair$mu, pair$sigma, difficulty)
  }
}

# P(X < x) or P(X > x): one z value, one look-up.
normal_question_one_sided <- function(mu, sigma, difficulty) {
  z <- pick_one(normal_z_values())
  x <- round(mu + z * sigma, 1)
  z <- round((x - mu) / sigma, 2)      # exactly the z we started from
  phi <- phi_from_table(z)

  if (pick_one(c("left", "right")) == "left") {
    answer <- phi
    asked <- paste0("P(X < ", fmt_num(x), ")")
    working <- paste0("P(X < ", fmt_num(x), ") = Phi(", fmt_z(z), ")")
    last_line <- paste0("= ", sprintf("%.4f", phi))
  } else {
    answer <- round_prob(1 - phi)
    asked <- paste0("P(X > ", fmt_num(x), ")")
    working <- paste0("P(X > ", fmt_num(x), ") = 1 - Phi(", fmt_z(z),
                      ") = 1 - ", sprintf("%.4f", phi))
    last_line <- paste0("= ", sprintf("%.4f", answer))
  }

  list(
    topic = "normal",
    type = "one_sided",
    difficulty = difficulty,
    prompt = paste0(
      "Let X ~ N(", mu, ", ", sigma, "^2). Calculate ", asked,
      ". Give your answer to four decimal places."
    ),
    answer = answer,
    tolerance = 0.001,
    hint = paste0(
      "Standardise the value first: z = (x - mu) / sigma. Then look Phi(z) ",
      "up in the z table."
    ),
    solution = paste0(
      "Standardise the value:\n\n",
      "  z = (", fmt_num(x), " - ", mu, ") / ", sigma, " = ", fmt_z(z), "\n\n",
      "Look z up in the z table:\n\n",
      "  ", phi_line(z), "\n\n",
      "Therefore:\n\n",
      "  ", working, "\n",
      "  ", last_line, "\n"
    )
  )
}

# P(a < X < b): two z values, two look-ups.
normal_question_interval <- function(mu, sigma, difficulty) {
  # The upper limit is drawn first, and the lower one from the remaining
  # values, so that a and b are never the same number.
  z_high <- pick_one(normal_interval_z())
  z_low <- -pick_one(normal_interval_z()[normal_interval_z() != z_high])

  a <- round(mu + z_low * sigma, 1)
  b <- round(mu + z_high * sigma, 1)
  z_low <- round((a - mu) / sigma, 2)
  z_high <- round((b - mu) / sigma, 2)

  phi_low <- phi_from_table(z_low)
  phi_high <- phi_from_table(z_high)
  answer <- round_prob(phi_high - phi_low)

  list(
    topic = "normal",
    type = "interval",
    difficulty = difficulty,
    prompt = paste0(
      "Let X ~ N(", mu, ", ", sigma, "^2). Calculate P(", fmt_num(a),
      " < X < ", fmt_num(b), "). Give your answer to four decimal places."
    ),
    answer = answer,
    tolerance = 0.001,
    hint = paste0(
      "You need two z values, one for each limit: z_lower = (a - mu) / sigma ",
      "and z_upper = (b - mu) / sigma. Then ",
      "P(a < X < b) = Phi(z_upper) - Phi(z_lower)."
    ),
    solution = paste0(
      "Standardise both limits:\n\n",
      "  z_lower = (", fmt_num(a), " - ", mu, ") / ", sigma, " = ", fmt_z(z_low), "\n",
      "  z_upper = (", fmt_num(b), " - ", mu, ") / ", sigma, " = ", fmt_z(z_high), "\n\n",
      "Look both up in the z table:\n\n",
      "  ", phi_line(z_low), "\n",
      "  ", phi_line(z_high), "\n\n",
      "Therefore:\n\n",
      "  P(", fmt_num(a), " < X < ", fmt_num(b), ")\n",
      "  = Phi(", fmt_z(z_high), ") - Phi(", fmt_z(z_low), ")\n",
      "  = ", sprintf("%.4f", phi_high), " - ", sprintf("%.4f", phi_low), "\n",
      "  = ", sprintf("%.4f", answer), "\n"
    )
  )
}

# The probability is given and x is asked for: work backwards through the
# table, then convert with x = mu + z * sigma.
normal_question_percentile <- function(mu, sigma, difficulty) {
  level <- pick_one(z_percentiles)
  p <- level$p
  z <- level$z
  x <- round(mu + z * sigma, 1)

  if (z >= 0) {
    lookup <- paste0(
      "  The table gives Phi(", fmt_z(z), ") = ",
      sprintf("%.4f", phi_from_table(z)), ", which is close to ",
      sprintf("%.2f", p), ".\n"
    )
    conversion <- paste0("  x = mu + z * sigma = ", mu, " + ", fmt_z(abs(z)),
                         " * ", sigma, " = ", fmt_num(x))
  } else {
    lookup <- paste0(
      "  The table only lists nonnegative z.  Phi(", fmt_z(abs(z)), ") = ",
      sprintf("%.4f", phi_from_table(abs(z))), ", so by symmetry Phi(",
      fmt_z(z), ") = 1 - ", sprintf("%.4f", phi_from_table(abs(z))), " = ",
      sprintf("%.4f", phi_from_table(z)), ", which is close to ",
      sprintf("%.2f", p), ".\n"
    )
    conversion <- paste0("  x = mu + z * sigma = ", mu, " - ", fmt_z(abs(z)),
                         " * ", sigma, " = ", fmt_num(x))
  }

  list(
    topic = "normal",
    type = "percentile",
    difficulty = difficulty,
    prompt = paste0(
      "Let X ~ N(", mu, ", ", sigma, "^2). Find the value of x for which ",
      "P(X < x) = ", sprintf("%.2f", p), ". Use the closest z value in the ",
      "z table, and give your answer to one decimal place."
    ),
    answer = x,
    tolerance = 0.5,
    hint = paste0(
      "This time the probability is known and z is not. Search the body of ",
      "the z table for the entry closest to the probability, read off z, then ",
      "convert back with x = mu + z * sigma."
    ),
    solution = paste0(
      "We need the z value whose table entry is closest to ",
      sprintf("%.2f", p), ".\n\n",
      lookup, "\n",
      "So z = ", fmt_z(z), ".\n\n",
      "Now convert z back to the original scale:\n\n",
      conversion, "\n\n",
      "So P(X < ", fmt_num(x), ") = ", sprintf("%.2f", p),
      " to the accuracy of the table.\n"
    )
  )
}
