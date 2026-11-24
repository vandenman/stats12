exam2_templates <- list(
  regression = list(
    c("prediction", "raw_score_change", "standardized_change"),
    c("f_from_r_squared", "standardize_slope", "directed_slope_t", "signed_correlation", "r_squared_from_output", "inverse_prediction"),
    c("prediction_from_correlation", "directed_slope_p", "original_score_from_z", "intercept_from_two_points", "residual_sd")
  ),
  diagnostics = list(
    c("residual_from_fit", "largest_absolute_residual"),
    c("slope_change", "residual_sum_of_squares"),
    c("influence_prediction", "influence_r_squared")
  ),
  dummies = list(
    c("fitted_group_mean", "reference_difference", "coded_group_mean"),
    c("nonreference_difference", "coefficient_from_means", "group_residual"),
    c("change_reference", "weighted_grand_mean_dummies", "directed_group_t")
  ),
  independent_t = list(
    c("independent_t_from_se", "cohens_d_from_pooled_sd", "student_df"),
    c("welch_standard_error", "student_standard_error", "pooled_sd", "directed_independent_p"),
    c("welch_t", "levene_t", "cohens_d_from_groups", "levene_directed_p")
  ),
  anova = list(
    c("within_df", "between_df", "anova_f_from_ms"),
    c("eta_squared", "weighted_grand_mean_anova", "between_ss"),
    c("f_from_sums_of_squares", "f_from_group_means", "f_from_two_group_t", "between_ss_unequal")
  )
)

generate_exam2_question <- function(topic, difficulty, type = NA_character_) {
  if (is.na(type)) type <- pick_one(exam2_templates[[topic]][[difficulty]])
  switch(topic,
    regression    = exam2_regression_question(difficulty, type),
    diagnostics   = exam2_diagnostic_question(difficulty, type),
    dummies       = exam2_dummy_question(difficulty, type),
    independent_t = exam2_independent_t_question(difficulty, type),
    anova         = exam2_anova_question(difficulty, type)
  )
}

# Use the same decimal precision as the SPSS tables, including in the solutions.
exam2_displayed <- function(x) as.numeric(.spss_number(x))

exam2_regression_table <- function(fit, tables = "coefficients", hide_beta = FALSE) {
  spss_regression_html(fit, title = NULL, format = "testvision", tables = tables,
                       term_labels = c(x = "Study time"), hide_beta = hide_beta)
}

exam2_regression_data <- function(slope = pick_one(c(-1, -.5, .5, 1))) {
  count <- pick_one(6:9)
  x <- rep(seq_len(count), each = 2)
  a <- pick_one(8:16)
  # The paired deviations keep the fitted intercept and slope simple.
  magnitudes <- sample(c(2:4, sample(2:4, count - 3, replace = TRUE)))
  error <- rep(c(-1, 1), count) * rep(magnitudes, each = 2)
  data.frame(x, y = a + slope * x + error)
}

exam2_regression_question <- function(difficulty, type) {
  question <- function(...) practice_number_question("regression", type, difficulty, ...)
  if (type == "prediction") {
    a <- pick_one(8:16)
    b <- pick_one(c(-1, -.5, .5, 1, 1.5, 2))
    x <- pick_one(3:8)
    operator <- if (b < 0) "-" else "+"
    return(question(
      sprintf("A fitted regression has y-hat = %s %s %s × x. What is the predicted score at x = %s?",
              a, operator, abs(b), x),
      a + b * x, "Substitute the given x into the regression equation.",
      sprintf("y-hat = %s %s %s × %s", a, operator, abs(b), x)))
  }

  data <- if (type == "directed_slope_p") exam2_regression_data(pick_one(c(-.5, -.3, .3, .5))) else
    exam2_regression_data()
  fit <- lm(y ~ x, data)
  estimates <- coef(summary(fit))["x", ]
  a <- exam2_displayed(coef(fit)[1])
  b <- exam2_displayed(estimates[1])
  r <- exam2_displayed(cor(data$x, data$y))
  s <- exam2_displayed(c(sd(data$x), sd(data$y)))

  if (type == "signed_correlation") {
    r2 <- exam2_displayed(summary(fit)$r.squared)
    return(question("What is the signed Pearson correlation between study time and score? Use the displayed R Square.",
      sign(b) * sqrt(r2), "R Square gives the magnitude after taking the square root. Where can you find the sign?",
      sprintf("Square root of R Square = sqrt(%s). The slope B = %s gives the sign.", r2, b),
      spss_regression_html(fit, title = NULL, format = "testvision", term_labels = c(x = "Study time"),
                           hide_beta = TRUE, hide_model = "R")))
  }

  if (type == "r_squared_from_output") {
    ss <- anova(fit)[["Sum Sq"]]
    rss <- exam2_displayed(ss[1])
    tss <- exam2_displayed(sum(ss))
    return(question("What proportion of the variation in score does this regression explain?",
      rss / tss, "Look for the regression and total sums of squares.",
      sprintf("R² = Regression SS / Total SS = %s / %s", rss, tss),
      spss_regression_html(fit, title = NULL, format = "testvision", term_labels = c(x = "Study time"),
                           hide_beta = TRUE, hide_model = c("R", "R Square", "Adjusted R Square"))))
  }

  if (type == "inverse_prediction" || type == "intercept_from_two_points") {
    x <- sample(2:8, 2)
    fitted_scores <- a + b * x
    if (type == "inverse_prediction") {
      return(question(
        sprintf("A regression predicts score from study time as y-hat = %s + (%s) × x. At how many hours is the predicted score %s?",
                a, b, fitted_scores[1]), (fitted_scores[1] - a) / b,
        "Solve the fitted equation for x.",
        sprintf("x = (predicted score - intercept) / slope = (%s - %s) / (%s)", fitted_scores[1], a, b)))
    }
    return(question(
      sprintf("A straight fitted line predicts %s points at %s hours of study and %s points at %s hours. What is its intercept?",
              fitted_scores[1], x[1], fitted_scores[2], x[2]),
      a, "Use the two predictions to find the slope, then use one prediction to find the intercept.",
      sprintf("Slope = (%s - %s) / (%s - %s) = %s\nIntercept = %s - (%s) × %s",
              fitted_scores[2], fitted_scores[1], x[2], x[1], b, fitted_scores[1], b, x[1])))
  }

  if (type == "residual_sd") {
    n <- nrow(data)
    sse <- exam2_displayed(deviance(fit))
    return(question(
      sprintf(paste("A simple regression with an intercept fits %s observations and has SSE = %s.",
        "The residual standard deviation is the square root of the residual mean square. What is it, in score points?"), n, sse),
      sqrt(sse / (n - 2L)), "The regression estimates an intercept and a slope.",
      sprintf("Residual SD = sqrt(SSE / (N - 2)) = sqrt(%s / (%s - 2))", sse, n)))
  }

  if (type == "raw_score_change" || type == "standardized_change") {
    change <- pick_one(c(1.5, 2, 2.5, 3))
    if (type == "raw_score_change") {
      return(question(
        sprintf(paste("The model predicts score from hours studied.",
          "What is the predicted change in score, in points, for a student who studies %s hours more?"), change),
        b * change, "Which slope expresses score points per extra hour?",
        sprintf("Use unstandardized B. Predicted change = %s × %s", b, change),
        exam2_regression_table(fit)))
    }
    return(question(
      sprintf(paste("A student studies %s standard deviations more than the average student.",
        "What is the student's predicted score in standard deviation units?"), change),
      r * change, "The regression of standardized scores has intercept zero. Use the standardized slope.",
      sprintf("Predicted z-score = Beta × z_study time = %s × %s", r, change),
      exam2_regression_table(fit)))
  }

  if (type == "f_from_r_squared") {
    n <- nrow(data)
    r2 <- exam2_displayed(summary(fit)$r.squared)
    return(question(
      sprintf(paste("The Model Summary is for a simple regression with N = %s observations",
        "and p = 2 fitted coefficients, including the intercept. Calculate the overall F statistic."), n),
      (n - 2) * r2 / (1 - r2), "Use R Square from the Model Summary and the overall F formula.",
      sprintf("F = [(N - p) / (p - 1)] × [R² / (1 - R²)]\n  = [(%s - 2) / (2 - 1)] × [%s / (1 - %s)]",
              n, r2, r2), exam2_regression_table(fit, "model")))
  }

  if (type == "standardize_slope") {
    return(question(
      sprintf(paste("A simple regression predicts score from study time. The SDs of study time",
        "and score are %s and %s, respectively. What is the standardized slope?"), s[1], s[2]),
      b * s[1] / s[2],
      "Convert an increase of one predictor SD to score points using B, then express that change in outcome SDs.",
      sprintf("Beta = B × (s_x / s_y) = %s × (%s / %s)", b, s[1], s[2]),
      exam2_regression_table(fit, hide_beta = TRUE)))
  }

  if (type == "directed_slope_t") {
    benchmark <- pick_one(c(-.25, .25, .75))
    direction <- pick_one(c("greater than", "less than"))
    se <- exam2_displayed(estimates[2])
    return(question(
      sprintf(paste("A researcher predicts that the population slope for study time will be %s %s",
        "points per hour. What is the t statistic for this test?"), direction, benchmark),
      (b - benchmark) / se, "The value of the slope at the null boundary goes in the numerator.",
      sprintf("t = (B - null value) / SE = (%s - (%s)) / %s", b, benchmark, se),
      exam2_regression_table(fit)))
  }

  if (type == "directed_slope_p") {
    direction <- pick_one(c(-1, 1))
    wording <- if (direction > 0) "a positive" else "a negative"
    p <- as.numeric(.spss_significance(estimates[4]))
    directed <- practice_directed_p(b, p, direction)
    return(question(
      sprintf("A researcher hypothesizes %s population slope for study time. What is the one-sided p-value?", wording),
      directed$p, "Compare the sign of B with the direction of the hypothesis before using Sig.",
      directed$working, exam2_regression_table(fit), digits = 4L))
  }

  if (type == "original_score_from_z") {
    z_x <- pick_one(c(-2, -1.5, 1.5, 2))
    ybar <- exam2_displayed(mean(data$y))
    z_y <- r * z_x
    return(question(
      sprintf(paste("The mean score is %s points and its SD is %s points. A student's study time",
        "has a z-score of %s. What is the predicted score in points?"), ybar, s[2], z_x),
      ybar + z_y * s[2], "Use Beta to predict a z-score, then convert it to score points.",
      sprintf("Predicted z-score = %s × (%s) = %s\nPredicted score = %s + (%s) × %s",
              r, z_x, fmt_num(z_y, 8), ybar, fmt_num(z_y, 8), s[2]),
      exam2_regression_table(fit)))
  }

  x <- pick_one(3:6)
  b <- r * s[2] / s[1]
  question(
    sprintf(paste("In a simple regression that predicts score (y) from study time (x),",
      "s_x = %s, s_y = %s, and the intercept is %s. Use the Correlations table",
      "to predict the score at x = %s."), s[1], s[2], a, x),
    a + b * x, "Find the slope from the correlation and the two standard deviations first.",
    sprintf("b = r × s_y / s_x = %s × %s / %s = %s\n\ny-hat = a + b × x = %s + (%s) × %s",
            r, s[2], s[1], fmt_num(b, 8), a, fmt_num(b, 8), x),
    spss_correlation_html(data$x, data$y, labels = c("Study time", "Score")))
}

exam2_diagnostic_question <- function(difficulty, type) {
  question <- function(...) practice_number_question("diagnostics", type, difficulty, ...)
  data <- exam2_regression_data()
  fit <- lm(y ~ x, data)

  if (type == "residual_from_fit") {
    i <- sample.int(nrow(data), 1)
    x <- data$x[i]
    y <- data$y[i]
    a <- exam2_displayed(coef(fit)[1])
    b <- exam2_displayed(coef(fit)[2])
    return(question(
      sprintf("A student who studied %s hours scored %s points. What is this student's residual?", x, y),
      y - (a + b * x), "Predict the score using the unstandardized coefficients.",
      sprintf("Predicted score = %s + (%s) × %s = %s\nResidual = %s - (%s)",
              a, b, x, a + b * x, y, a + b * x), exam2_regression_table(fit)))
  }

  if (type == "largest_absolute_residual" || type == "residual_sum_of_squares") {
    candidates <- seq(1, nrow(data), 2) + sample(0:1, nrow(data) / 2, replace = TRUE)
    if (type == "largest_absolute_residual") {
      candidates <- candidates[!duplicated(abs(round(resid(fit)[candidates], 6)))]
    }
    i <- sample(candidates, 3)
    y <- data$y[i]
    predicted <- exam2_displayed(fitted(fit)[i])
    residuals <- y - predicted
    rows <- vapply(seq_along(i), function(j) .spss_row(j, c(y[j], predicted[j])), character(1))
    stimulus <- .spss_output(list(.spss_table("Predicted scores",
      .spss_headings(c("Observation", "Observed score", "Predicted score")), rows)),
      NULL, NULL, "testvision")
    working <- paste("Residuals (observed - predicted) are", paste(fmt_num(residuals), collapse = ", "))
    if (type == "largest_absolute_residual") {
      return(question("Which observation has the largest absolute residual? Give its observation number.",
        which.max(abs(residuals)), "Compare the magnitudes of the prediction errors.",
        working, stimulus, whole = TRUE))
    }
    return(question(
      "How much do the three observations shown contribute in total to the model's error sum of squares?",
      sum(residuals^2), "Find each residual before adding its squared contribution.",
      paste0(working, "\nError sum of squares for these observations = ",
             paste0("(", fmt_num(residuals), ")²", collapse = " + ")), stimulus))
  }

  outlying_x <- max(data$x) + 5
  outlying_y <- predict(fit, data.frame(x = outlying_x)) + pick_one(c(-8:-4, 4:8))
  data <- rbind(data, data.frame(x = outlying_x, y = outlying_y))
  before <- lm(y ~ x, data)
  distances <- cooks.distance(before)
  i <- which.max(distances)
  after <- lm(y ~ x, data[-i, ])
  intro <- sprintf(paste("Observation %s had the largest Cook's distance (%s).",
    "The table shows the fit with all observations and the fit after leaving that observation out."), i,
    .spss_number(distances[i]))
  tables <- function(selection = "coefficients") {
    paste0("<p><strong>All observations</strong></p>", exam2_regression_table(before, selection),
           "<p><strong>Observation left out</strong></p>", exam2_regression_table(after, selection))
  }
  if (type == "slope_change") {
    slopes <- exam2_displayed(c(coef(before)["x"], coef(after)["x"]))
    return(question(paste(intro, "By how much did the slope change (after minus before)?"),
      slopes[2] - slopes[1], "Use the unstandardized B for study time in each table.",
      sprintf("Change in the slope = %s - (%s)", slopes[2], slopes[1]), tables()))
  }
  if (type == "influence_r_squared") {
    r2 <- exam2_displayed(c(summary(before)$r.squared, summary(after)$r.squared))
    return(question(paste(intro, "What is the change in explained variance (after minus before), in percentage points?"),
      100 * (r2[2] - r2[1]), "Compare the two R Square values. Convert their difference to percentage points.",
      sprintf("Change = (%s - %s) × 100 percentage points", r2[2], r2[1]), tables("model")))
  }

  x <- pick_one(unique(data$x[-i]))
  a <- exam2_displayed(c(coef(before)[1], coef(after)[1]))
  b <- exam2_displayed(c(coef(before)[2], coef(after)[2]))
  predicted <- a + b * x
  question(
    paste(intro, sprintf("By how many points did the predicted score at %s hours change (after minus before)?", x)),
    predicted[2] - predicted[1], "Predict a score from each table using its intercept and unstandardized slope.",
    sprintf(paste("Predicted before = %s + (%s) × %s = %s\nPredicted after = %s + (%s) × %s = %s",
                  "\nChange = %s - (%s)"),
            a[1], b[1], x, fmt_num(predicted[1]), a[2], b[2], x, fmt_num(predicted[2]),
            fmt_num(predicted[2]), fmt_num(predicted[1])), tables())
}

exam2_group_samples <- function(k, means = sample(14:23, k), sizes = sample(6:10, k, replace = TRUE),
                                sds = sample(2:5, k, replace = TRUE)) {
  # Rescale small samples to have calculator-friendly displayed means and SDs.
  Map(function(n, mean, sd) mean + sd * as.numeric(scale(seq_len(n))), sizes, means, sds)
}

exam2_group_data <- function(samples) {
  data.frame(group = factor(rep(LETTERS[seq_along(samples)], lengths(samples))),
             score = unlist(samples, use.names = FALSE))
}

exam2_group_summary_html <- function(sizes, means, groups = LETTERS[seq_along(sizes)]) {
  rows <- vapply(seq_along(groups), function(i) {
    .spss_row(paste("Group", groups[i]), c(sizes[i], .spss_number(means[i])))
  }, character(1))
  .spss_output(list(.spss_table("Group Statistics", .spss_headings(c("Group", "N", "Mean")), rows)),
               NULL, NULL, "testvision")
}

exam2_dummy_question <- function(difficulty, type) {
  question <- function(...) practice_number_question("dummies", type, difficulty, ...)
  samples <- exam2_group_samples(if (type == "coded_group_mean") 4L else pick_one(3:4))
  data <- exam2_group_data(samples)
  reference <- pick_one(levels(data$group))
  other_groups <- sample(setdiff(levels(data$group), reference), 2)
  first <- other_groups[1]
  second <- other_groups[2]
  data$group <- relevel(data$group, reference)
  fit <- lm(score ~ group, data)
  a <- exam2_displayed(coef(fit)[1])
  b <- setNames(exam2_displayed(coef(fit)[-1]), levels(data$group)[-1])
  stimulus <- spss_regression_html(fit, title = NULL, format = "testvision", tables = "coefficients",
    term_labels = setNames(paste("Group", names(b)), paste0("group", names(b))))
  introduction <- sprintf("The outcome is score and group %s is the reference group.", reference)
  means <- exam2_displayed(vapply(samples, mean, numeric(1)))

  if (type == "coded_group_mean") {
    groups <- LETTERS[seq_along(samples)]
    codes <- paste0("D", seq_along(b))
    indicators <- model.matrix(fit)[match(groups, data$group), -1, drop = FALSE]
    rows <- vapply(seq_along(groups), function(i) {
      .spss_row(paste("Group", groups[i]), indicators[i, ])
    }, character(1))
    coding <- .spss_output(list(.spss_table("Dummy coding",
      .spss_headings(c("Group", codes)), rows)), NULL, NULL, "testvision")
    coefficients <- spss_regression_html(fit, title = NULL, format = "testvision",
      tables = "coefficients", term_labels = setNames(codes, paste0("group", names(b))))
    selected <- pick_one(groups)
    values <- indicators[match(selected, groups), ]
    return(question(sprintf("What is the fitted mean score for group %s?", selected),
      a + sum(values * b), "Use the dummy values in this group's row and the unstandardized B coefficients.",
      sprintf("Fitted mean = %s + %s", a, paste(sprintf("(%s) × %s", b, values), collapse = " + ")),
      paste0(coding, coefficients)))
  }

  if (type == "fitted_group_mean") {
    return(question(paste(introduction, sprintf("What is the fitted mean for group %s?", first)),
      a + b[[first]], "Add the intercept and the unstandardized coefficient for the group.",
      sprintf("Fitted mean for group %s = %s + (%s)", first, a, b[[first]]), stimulus))
  }

  if (type == "nonreference_difference") {
    return(question(paste(introduction,
      sprintf("What is the mean difference between group %s and group %s (%s minus %s)?",
              first, second, first, second)),
      b[[first]] - b[[second]], "Both group means include the same intercept. It cancels in the difference.",
      sprintf("Mean difference = b_%s - b_%s = %s - (%s)", first, second, b[[first]], b[[second]]), stimulus))
  }

  if (type == "reference_difference") {
    return(question(paste(introduction,
      sprintf("What is the mean difference between group %s and group %s (%s minus %s)?",
              reference, first, reference, first)),
      -b[[first]], "An unstandardized dummy coefficient measures the difference relative to the reference group.",
      sprintf("Mean difference = a - (a + b_%s) = -(%s)", first, b[[first]]), stimulus))
  }

  if (type == "coefficient_from_means") {
    return(question(paste(introduction, sprintf("What is the unstandardized coefficient for group %s?", first)),
      b[[first]], "Use the means for this group and the reference group.",
      sprintf("B = mean for group %s - mean for group %s = %s - %s",
              first, reference, a + b[[first]], a),
      exam2_group_summary_html(lengths(samples), means)))
  }

  if (type == "group_residual") {
    observed <- pick_one(samples[[match(first, LETTERS)]])
    observed <- exam2_displayed(observed)
    predicted <- a + b[[first]]
    return(question(paste(introduction,
      sprintf("A participant in group %s scored %s points. What is this participant's residual?", first, observed)),
      observed - predicted, "Predict the score for the participant's group first.",
      sprintf("Predicted score = %s + (%s) = %s\nResidual = %s - %s", a, b[[first]], predicted, observed, predicted),
      stimulus))
  }

  if (type == "weighted_grand_mean_dummies") {
    groups <- LETTERS[seq_along(samples)]
    sizes <- lengths(samples)
    sample_sizes <- paste(paste0("Group ", groups, " has ", sizes, " observations"), collapse = ". ")
    working_means <- sprintf("Group %s mean = %s + (%s) = %s", names(b), a, b, a + b)
    return(question(paste(introduction, paste0(sample_sizes, ". What is the overall mean?")),
      sum(sizes * means) / sum(sizes),
      "Recover the fitted group means using B, then weight each mean by the number of observations.",
      paste(c(sprintf("Reference group %s mean = %s", reference, a), working_means,
        sprintf("Overall mean = (%s) / %s", paste(paste(sizes, "×", means), collapse = " + "), sum(sizes))),
        collapse = "\n"), stimulus))
  }

  if (type == "directed_group_t") {
    benchmark <- pick_one(c(1.5, 2.5, 3.5))
    se <- exam2_displayed(coef(summary(fit))[paste0("group", first), 2])
    return(question(paste(introduction, sprintf(paste("A researcher predicts that group %s's",
      "population mean exceeds the reference group's mean by more than %s points. What is the t statistic for this test?"),
      first, benchmark)), (b[[first]] - benchmark) / se,
      "Use the dummy's unstandardized coefficient and standard error with the hypothesized boundary.",
      sprintf("t = (B - null difference) / SE = (%s - %s) / %s", b[[first]], benchmark, se), stimulus))
  }

  question(paste(introduction,
    sprintf("If group %s becomes the reference, what is the new unstandardized coefficient for group %s?", first, second)),
    b[[second]] - b[[first]], "Find the two fitted group means. Use the new reference mean as the baseline.",
    sprintf("New reference mean = %s + (%s) = %s\nOther group mean = %s + (%s) = %s\nNew coefficient = %s - %s",
      a, b[[first]], a + b[[first]], a, b[[second]], a + b[[second]], a + b[[second]], a + b[[first]]),
    stimulus)
}

exam2_independent_t_question <- function(difficulty, type) {
  question <- function(...) practice_number_question("independent_t", type, difficulty, ...)
  use_levene <- type %in% c("levene_t", "levene_directed_p")
  if (use_levene) {
    spread <- pick_one(list(c(3, 4), c(4, 3), c(2, 8), c(8, 2)))
    sizes <- c(pick_one(12:15), pick_one(8:10))
    # Small mean differences keep both displayed p-values usable for directed tests.
    center <- pick_one(17:22)
    samples <- exam2_group_samples(2, c(center, center + pick_one(c(-2:-1, 1:2))), sizes, spread)
  } else if (type == "directed_independent_p") {
    center <- pick_one(17:22)
    samples <- exam2_group_samples(2, c(center, center + pick_one(c(-3:-1, 1:3))),
                                   sds = sample(3:5, 2, replace = TRUE))
  } else {
    # Unequal sample sizes make the pooled and Welch formulas distinguishable.
    samples <- exam2_group_samples(2, sizes = sample(6:10, 2))
  }
  n <- lengths(samples)
  means <- exam2_displayed(vapply(samples, mean, numeric(1)))
  s <- exam2_displayed(vapply(samples, sd, numeric(1)))
  difference <- means[1] - means[2]
  pooled_var <- sum((n - 1) * s^2) / (sum(n) - 2)
  pooled_sd <- sqrt(pooled_var)
  student_se <- sqrt(pooled_var * sum(1 / n))
  welch_se <- sqrt(sum(s^2 / n))
  table <- function(display = "full") spss_t_test_html(samples[[1]], samples[[2]], display = display)
  pooled_work <- sprintf("Pooled variance = [(%s - 1) × %s² + (%s - 1) × %s²] / (%s + %s - 2) = %s",
                          n[1], s[1], n[2], s[2], n[1], n[2], fmt_num(pooled_var, 8))
  student_work <- paste0(pooled_work, "\n",
    sprintf("Student SE = sqrt(%s × (1/%s + 1/%s)) = %s",
            fmt_num(pooled_var, 8), n[1], n[2], fmt_num(student_se, 8)))
  welch_work <- sprintf("Welch SE = sqrt(s_1² / n_1 + s_2² / n_2)\n  = sqrt(%s² / %s + %s² / %s) = %s",
                        s[1], n[1], s[2], n[2], fmt_num(welch_se, 8))
  difference_prompt <- "Use Group 1 minus Group 2."

  if (type == "independent_t_from_se") {
    se <- exam2_displayed(student_se)
    return(question(
      sprintf("The standard error of the difference between the two means is %s. What is Student's t statistic? %s",
              se, difference_prompt),
      difference / se, "Divide the observed mean difference by its standard error.",
      sprintf("t = (%s - %s) / %s", means[1], means[2], se), table("statistics")))
  }

  if (type == "cohens_d_from_pooled_sd") {
    sd <- exam2_displayed(pooled_sd)
    return(question(sprintf("The pooled standard deviation is %s. What is Cohen's d? %s", sd, difference_prompt),
      difference / sd, "Express the signed mean difference in pooled standard deviation units.",
      sprintf("d = (%s - %s) / %s", means[1], means[2], sd), table("statistics")))
  }

  if (type == "student_df") {
    return(question("What are the degrees of freedom for Student's independent-samples t-test?",
      sum(n) - 2L, "Use the two sample sizes.",
      sprintf("df = n_1 + n_2 - 2 = %s + %s - 2", n[1], n[2]), table("statistics"), whole = TRUE))
  }

  if (type == "welch_standard_error") {
    return(question(
      "What is Welch's standard error of the difference between the two means?",
      welch_se, "Use both group variances and sample sizes in the Welch formula.", welch_work, table()))
  }

  if (type == "student_standard_error") {
    return(question(
      "What is Student's standard error of the difference between the two means?",
      student_se, "Find the pooled variance and use it in the standard error formula.",
      student_work, table()))
  }

  if (type == "pooled_sd") {
    return(question("What is the pooled standard deviation?",
      pooled_sd, "Pool the two variances using their degrees of freedom, then return to the original units.",
      paste0(pooled_work, "\nPooled standard deviation = sqrt(", fmt_num(pooled_var, 8), ")"), table("statistics")))
  }

  if (type == "cohens_d_from_groups") {
    return(question(paste("What is Cohen's d for the difference between the two groups?", difference_prompt),
      difference / pooled_sd, "Divide the mean difference by a pooled standard deviation.",
      paste0(pooled_work, "\n", sprintf("d = (%s - %s) / sqrt(%s)", means[1], means[2], fmt_num(pooled_var, 8))),
      table()))
  }

  if (type == "welch_t") {
    return(question(paste("What is Welch's t statistic when the null difference is zero?", difference_prompt),
      difference / welch_se, "Find Welch's standard error, then divide the mean difference by it.",
      paste0(welch_work, "\n", sprintf("t = (%s - %s) / %s", means[1], means[2], fmt_num(welch_se, 8))),
      table("hide_t")))
  }

  welch <- TRUE
  test_selection <- ""
  if (use_levene) {
    sample_groups <- factor(rep(1:2, n))
    levene <- .spss_levene_result(unlist(samples), sample_groups)
    levene_p <- .spss_significance(levene$p.value)
    welch <- as.numeric(.spss_number(levene$p.value)) < .05
    test_selection <- sprintf("Levene's p %s. Use the %s row.\n",
      if (startsWith(levene_p, "<")) paste0("< ", substring(levene_p, 2)) else paste0("= ", levene_p),
      if (welch) "Equal variances not assumed (Welch)" else "Equal variances assumed (Student)")
  }

  if (type == "levene_t") {
    se <- if (welch) welch_se else student_se
    return(question(
      paste("Choose the test using Levene's result at α = .05. What is the t statistic?", difference_prompt),
      difference / se, "Choose the equal- or unequal-variances row first. Use the corresponding standard error.",
      paste0(test_selection, if (welch) welch_work else student_work, "\n",
             sprintf("t = (%s - %s) / %s", means[1], means[2], fmt_num(se, 8))), table("hide_t")))
  }

  direction <- pick_one(c(-1, 1))
  hypothesis <- sprintf("A researcher expects Group 1's population mean to be %s than Group 2's.",
                        if (direction > 0) "higher" else "lower")
  instruction <- if (use_levene) "Use Levene's result at α = .05 to choose the t-test." else "Use Welch's t-test."
  test <- t.test(samples[[1]], samples[[2]], var.equal = !welch)
  p <- as.numeric(.spss_significance(test$p.value))
  directed <- practice_directed_p(difference, p, direction)
  question(paste(hypothesis, instruction, "What is the one-sided p-value?"),
    directed$p, "Check which row applies and whether the sign of its t agrees with the direction of the hypothesis.",
    paste0(test_selection, directed$working), table(), digits = 4L)
}

exam2_anova_question <- function(difficulty, type) {
  question <- function(...) practice_number_question("anova", type, difficulty, ...)
  k <- pick_one(3:4)
  if (type == "f_from_two_group_t") {
    samples <- exam2_group_samples(2, sizes = sample(6:10, 2), sds = pick_one(list(c(3, 4), c(4, 3))))
    test <- t.test(samples[[1]], samples[[2]], var.equal = TRUE)
    t <- exam2_displayed(test$statistic)
    return(question(
      paste("The same two groups are compared in a classical one-way ANOVA.",
            "What is the omnibus F statistic?"),
      t^2, "Think about the relationship between the pooled two-group t-test and the ANOVA F-test.",
      sprintf("F = t² using the equal-variances row. F = (%s)²", t),
      spss_t_test_html(samples[[1]], samples[[2]])))
  }

  if (type == "f_from_group_means" || type == "between_ss") {
    k <- 3L
    group_size <- pick_one(5:8)
    center <- pick_one(17:22)
    means <- sample(center + c(-3, 0, 3) * pick_one(c(1, 1.5, 2)))
    samples <- exam2_group_samples(k, means = means, sizes = rep(group_size, k))
  } else if (type == "between_ss_unequal") {
    samples <- exam2_group_samples(k, sizes = sample(6:10, k))
  } else {
    samples <- exam2_group_samples(k)
  }
  data <- exam2_group_data(samples)
  n <- nrow(data)
  if (type == "within_df") {
    return(question(
      sprintf("A one-way ANOVA compares %s groups using %s observations. What are the within-groups degrees of freedom?", k, n),
      n - k, "Use the total sample size and the number of groups.",
      sprintf("Within-groups df = N - k = %s - %s", n, k), whole = TRUE))
  }
  if (type == "between_df") {
    return(question(
      sprintf("A one-way ANOVA compares %s groups using %s observations. What are the between-groups degrees of freedom?", k, n),
      k - 1L, "Count the independent group mean deviations from the grand mean.",
      sprintf("Between-groups df = k - 1 = %s - 1", k), whole = TRUE))
  }

  fit <- aov(score ~ group, data)
  ss_raw <- anova(fit)[["Sum Sq"]]
  ss <- exam2_displayed(ss_raw)
  ms <- ss / c(k - 1, n - k)
  if (type == "anova_f_from_ms") {
    displayed_ms <- exam2_displayed(anova(fit)[["Mean Sq"]])
    return(question(
      sprintf("In a one-way ANOVA, the between-groups mean square is %s and the within-groups mean square is %s. What is F?",
              displayed_ms[1], displayed_ms[2]),
      displayed_ms[1] / displayed_ms[2], "Which mean square goes in the numerator?",
      sprintf("F = MSR / MSE = %s / %s", displayed_ms[1], displayed_ms[2])))
  }

  if (type == "eta_squared") {
    total <- exam2_displayed(sum(ss_raw))
    return(question(
      "Calculate eta squared from the ANOVA sums of squares. Give a proportion.",
      ss[1] / total, "Divide the Between Groups sum of squares by the Total sum of squares.",
      sprintf("Eta squared = RSS / TSS = %s / %s", ss[1], total), spss_anova_html(fit)))
  }

  if (type %in% c("weighted_grand_mean_anova", "between_ss", "f_from_group_means", "between_ss_unequal")) {
    means <- exam2_displayed(vapply(samples, mean, numeric(1)))
    sizes <- lengths(samples)
    grand_mean <- sum(sizes * means) / n
    stimulus <- exam2_group_summary_html(sizes, means)
    mean_work <- sprintf("Grand mean = (%s) / %s = %s",
      paste(paste(sizes, "×", means), collapse = " + "), n, fmt_num(grand_mean, 8))
    if (type == "weighted_grand_mean_anova") {
      return(question("What is the grand mean across all observations?",
        grand_mean, "Weight each group mean by its sample size.", mean_work, stimulus))
    }
    rss <- sum(sizes * (means - grand_mean)^2)
    rss_work <- sprintf("RSS = %s = %s",
      paste0(sizes, " × (", means, " - ", fmt_num(grand_mean, 9), ")²", collapse = " + "), fmt_num(rss, 9))
    if (type == "between_ss") {
      return(question(
        sprintf("The grand mean is %s. What is the between-groups sum of squares?", grand_mean),
        rss, "For each group, multiply its squared deviation from the grand mean by its sample size.",
        rss_work, stimulus))
    }
    if (type == "between_ss_unequal") {
      return(question("What is the between-groups sum of squares, in points squared?",
        rss, "Calculate the weighted grand mean before finding each group's contribution.",
        paste(mean_work, rss_work, sep = "\n"), stimulus))
    }
    mse <- exam2_displayed(anova(fit)[["Mean Sq"]][2])
    return(question(
      sprintf(paste("A one-way ANOVA of the groups shown gives a within-groups mean square of %s.",
        "What is the omnibus F statistic?"), mse),
      (rss / (k - 1L)) / mse, "Find the grand mean and the between-groups sum of squares first.",
      paste(mean_work, rss_work, sprintf("MSR = %s / %s = %s\nF = %s / %s",
        rss, k - 1L, rss / (k - 1L), rss / (k - 1L), mse), sep = "\n"), stimulus))
  }

  question(
    "What is the omnibus F statistic for this one-way ANOVA?",
    ms[1] / ms[2], "Divide each sum of squares by its degrees of freedom before taking the ratio.",
    sprintf("MSR = %s / %s = %s\nMSE = %s / %s = %s\n\nF = MSR / MSE = %s / %s",
            ss[1], k - 1, fmt_num(ms[1], 8), ss[2], n - k, fmt_num(ms[2], 8),
            fmt_num(ms[1], 8), fmt_num(ms[2], 8)),
    spss_anova_html(fit, hide_calculations = TRUE))
}
