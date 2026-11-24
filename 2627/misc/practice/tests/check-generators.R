# Run from the practice folder with Rscript --vanilla tests/check-generators.R.
files <- c("R/utils.R", "R/probability.R", "R/normal.R", "R/clt.R", "R/bayes.R",
           "R/ztests.R", "R/ttests.R", "../../lecture13/scripts/spss_regression_html.r",
           "R/descriptives.R", "R/exam2.R", "R/assumptions.R", "R/generators.R")
invisible(lapply(files, source, encoding = "UTF-8"))

matches <- function(text, pattern) regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]
numbers <- function(text) as.numeric(matches(text, "-?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)"))
table_row <- function(html, label) {
  lines <- strsplit(html, "\n", fixed = TRUE)[[1]]
  lines[grepl("<td>", lines, fixed = TRUE) &
          grepl(paste0("<th>", label, "</th>"), lines, fixed = TRUE)]
}
cells <- function(row) gsub("</?td>", "", matches(row, "<td>[^<]*</td>"))
table_values <- function(html, label) suppressWarnings(as.numeric(cells(table_row(html, label)[1])))
group_labels <- function(prompt) sub("^group", "Group", matches(prompt, "[Gg]roup [A-D]\\b"))

ttest_row <- function(html, welch) {
  table_values(html, if (welch) "Equal variances not assumed" else "Equal variances assumed")
}

levene_selects_welch <- function(html) {
  p <- cells(table_row(html, "Equal variances assumed"))[2]
  stopifnot(p != ".050")
  startsWith(p, "&lt;") || as.numeric(p) < .05
}

one_sided_p <- function(t, p, positive) {
  stopifnot(is.finite(p), p > 0, p < 1, t != 0)
  if (positive == (t > 0)) p / 2 else 1 - p / 2
}

# Solve each question independently from the values supplied to students.
solve_question <- function(q) {
  x <- numbers(sub("- ", "-", q$prompt, fixed = TRUE))
  html <- q$stimulus_html
  group <- function(label) table_values(html, label)
  if (q$topic == "independent_t") {
    a <- group("Group 1")
    b <- group("Group 2")
    difference <- a[2] - b[2]
    pooled <- ((a[1] - 1) * a[3]^2 + (b[1] - 1) * b[3]^2) / (a[1] + b[1] - 2)
    student_se <- sqrt(pooled * (1 / a[1] + 1 / b[1]))
    welch_se <- sqrt(a[3]^2 / a[1] + b[3]^2 / b[1])
  }

  switch(q$type,
    prediction = x[1] + x[2] * x[3],
    raw_score_change = group("Study time")[2] * x[1],
    standardized_change = group("Study time")[4] * x[1],
    f_from_r_squared = {
      stopifnot(!grepl("<th>F</th>", html, fixed = TRUE))
      r2 <- table_values(html, "1")[2]
      (x[1] - x[2]) / (x[2] - 1) * r2 / (1 - r2)
    },
    signed_correlation = {
      stopifnot(cells(table_row(html, "1"))[1] == "", cells(table_row(html, "Study time"))[4] == "")
      sign(group("Study time")[2]) * sqrt(group("1")[2])
    },
    r_squared_from_output = {
      stopifnot(all(cells(table_row(html, "1"))[1:3] == ""), cells(table_row(html, "Study time"))[4] == "")
      group("Regression")[2] / group("Total")[2]
    },
    inverse_prediction = (x[3] - x[1]) / x[2],
    intercept_from_two_points = x[1] - (x[1] - x[3]) / (x[2] - x[4]) * x[2],
    residual_sd = sqrt(x[2] / (x[1] - 2)),
    prediction_from_correlation = {
      r <- tail(table_values(html, "Pearson Correlation"), 1)
      x[3] + r * x[2] / x[1] * x[4]
    },
    standardize_slope = {
      stopifnot(cells(table_row(html, "Study time"))[4] == "")
      group("Study time")[2] / x[2] * x[1]
    },
    directed_slope_t = {
      slope <- group("Study time")
      (slope[2] - x[1]) / slope[3]
    },
    directed_slope_p = {
      slope <- group("Study time")
      one_sided_p(slope[5], slope[6], grepl("a positive", q$prompt, fixed = TRUE))
    },
    original_score_from_z = x[1] + x[2] * group("Study time")[4] * x[3],
    residual_from_fit = x[2] - (group("(Constant)")[2] + group("Study time")[2] * x[1]),
    largest_absolute_residual = {
      r <- vapply(as.character(1:3), function(i) diff(group(i)), numeric(1))
      stopifnot(length(unique(abs(r))) == 3L)
      which.max(abs(r))
    },
    residual_sum_of_squares = sum(vapply(as.character(1:3), function(i) diff(group(i))^2, numeric(1))),
    slope_change = {
      rows <- table_row(html, "Study time")
      stopifnot(length(rows) == 2L, grepl("Cook's distance", q$prompt, fixed = TRUE), x[2] > 0)
      diff(vapply(rows, function(row) as.numeric(cells(row)[2]), numeric(1)))
    },
    influence_prediction = {
      stopifnot(grepl("Cook's distance", q$prompt, fixed = TRUE), x[2] > 0)
      coefs <- function(label) vapply(table_row(html, label), function(row) as.numeric(cells(row)[2]), numeric(1))
      diff(coefs("(Constant)") + x[3] * coefs("Study time"))
    },
    influence_r_squared = {
      stopifnot(grepl("Cook's distance", q$prompt, fixed = TRUE), x[2] > 0)
      100 * diff(vapply(table_row(html, "1"), function(row) as.numeric(cells(row)[2]), numeric(1)))
    },
    fitted_group_mean = {
      groups <- group_labels(q$prompt)
      table_values(html, "(Constant)")[2] + table_values(html, groups[2])[2]
    },
    coded_group_mean = {
      values <- group(group_labels(q$prompt)[1])
      slopes <- vapply(1:3, function(i) group(paste0("D", i))[2], numeric(1))
      group("(Constant)")[2] + sum(values * slopes)
    },
    nonreference_difference = {
      groups <- group_labels(q$prompt)
      stopifnot(length(unique(groups)) == 3L)
      means <- vapply(groups[2:3], function(group) table_values(html, group)[2], numeric(1))
      means[1] - means[2]
    },
    reference_difference = {
      groups <- group_labels(q$prompt)
      stopifnot(groups[1] == groups[2])
      -group(groups[3])[2]
    },
    coefficient_from_means = {
      groups <- group_labels(q$prompt)
      group(groups[2])[2] - group(groups[1])[2]
    },
    group_residual = {
      groups <- group_labels(q$prompt)
      x[1] - (group("(Constant)")[2] + group(groups[2])[2])
    },
    change_reference = {
      groups <- group_labels(q$prompt)
      means <- table_values(html, "(Constant)")[2] +
        vapply(groups[2:3], function(group) table_values(html, group)[2], numeric(1))
      means[2] - means[1]
    },
    weighted_grand_mean_dummies = {
      groups <- group_labels(q$prompt)
      means <- group("(Constant)")[2] +
        vapply(groups[-1], function(label) if (label == groups[1]) 0 else group(label)[2], numeric(1))
      weighted.mean(means, x[seq_along(means)])
    },
    directed_group_t = {
      slope <- group(group_labels(q$prompt)[2])
      (slope[2] - x[1]) / slope[3]
    },
    independent_t_from_se = {
      stopifnot(!grepl("Independent Samples Test", html, fixed = TRUE))
      difference / x[1]
    },
    cohens_d_from_pooled_sd = difference / x[1],
    student_df = {
      stopifnot(!grepl("Independent Samples Test", html, fixed = TRUE))
      a[1] + b[1] - 2
    },
    welch_standard_error = welch_se,
    student_standard_error = student_se,
    pooled_sd = sqrt(pooled),
    directed_independent_p = {
      test <- ttest_row(html, TRUE)
      one_sided_p(test[3], test[5], grepl("higher than", q$prompt, fixed = TRUE))
    },
    welch_t = ,
    levene_t = {
      for (label in c("Equal variances assumed", "Equal variances not assumed")) {
        row <- cells(table_row(html, label))
        stopifnot(all(row[c(3, 5)] == ""), as.numeric(row[4]) > 0)
      }
      welch <- q$type == "welch_t" || levene_selects_welch(html)
      difference / (if (welch) welch_se else student_se)
    },
    cohens_d_from_groups = difference / sqrt(pooled),
    levene_directed_p = {
      test <- ttest_row(html, levene_selects_welch(html))
      one_sided_p(test[3], test[5], grepl("higher than", q$prompt, fixed = TRUE))
    },
    within_df = x[2] - x[1],
    between_df = x[1] - 1L,
    anova_f_from_ms = x[1] / x[2],
    eta_squared = table_values(html, "Between Groups")[1] / table_values(html, "Total")[1],
    weighted_grand_mean_anova = {
      stats <- lapply(paste("Group", LETTERS[1:4]), group)
      stats <- do.call(rbind, Filter(length, stats))
      weighted.mean(stats[, 2], stats[, 1])
    },
    between_ss_unequal = {
      stats <- do.call(rbind, Filter(length, lapply(paste("Group", LETTERS[1:4]), group)))
      stopifnot(!anyDuplicated(stats[, 1]))
      sum(stats[, 1] * (stats[, 2] - weighted.mean(stats[, 2], stats[, 1]))^2)
    },
    between_ss = ,
    f_from_group_means = {
      stats <- vapply(paste("Group", LETTERS[1:3]), group, numeric(2))
      grand <- if (q$type == "between_ss") x[1] else weighted.mean(stats[2, ], stats[1, ])
      rss <- sum(stats[1, ] * (stats[2, ] - grand)^2)
      if (q$type == "between_ss") rss else rss / (ncol(stats) - 1) / x[1]
    },
    f_from_sums_of_squares = {
      between <- cells(table_row(html, "Between Groups"))
      within <- cells(table_row(html, "Within Groups"))
      stopifnot(all(between[3:5] == ""), all(within[3:5] == ""))
      (as.numeric(between[1]) / as.numeric(between[2])) / (as.numeric(within[1]) / as.numeric(within[2]))
    },
    f_from_two_group_t = ttest_row(html, FALSE)[3]^2,
    stop("Unknown question type: ", q$type)
  )
}

types <- character()
levene_decisions <- logical()
tails <- logical()
for (seed in 1:100) {
  for (level in 1:3) {
    questions <- generate_practice_set(seed, 15, level, exam = "exam2")
    topics <- vapply(questions, `[[`, "", "topic")
    stopifnot(length(questions) == 15L, setequal(topics, exam2_topics), all(table(topics) == 3L))
    for (topic in exam2_topics) {
      used <- vapply(questions[topics == topic], `[[`, "", "type")
      available <- exam2_templates[[topic]][[level]]
      stopifnot(length(unique(used)) == min(length(used), length(available)),
                all(used %in% available))
    }
    for (q in questions) {
      types <- union(types, q$type)
      computed <- solve_question(q)
      digits <- if (q$tolerance == 0) 0L else if (q$tolerance < .001) 4L else 2L
      expected <- unname(round(computed, digits))
      if (!is.finite(q$answer) || !isTRUE(all.equal(q$answer, unname(computed)))) {
        stop(sprintf("Seed %s, %s: expected %s, got %s.", seed, q$type, computed, q$answer))
      }
      shown <- as.numeric(sub(".*\nAnswer = ", "", q$solution))
      stopifnot(
        check_answer_text(as.character(expected), q$answer, q$tolerance) == "Correct!",
        check_answer_text(as.character(expected + 1), q$answer, q$tolerance) != "Correct!",
        isTRUE(all.equal(shown, expected)), nzchar(q$hint))
      if (q$type == "levene_t") {
        levene_decisions <- union(levene_decisions, levene_selects_welch(q$stimulus_html))
      }
      if (q$type %in% c("directed_slope_p", "directed_independent_p", "levene_directed_p")) {
        tails <- union(tails, q$answer < .5)
      }
    }
  }
}
stopifnot(setequal(types, unlist(exam2_templates)), length(types) == 50L,
          setequal(levene_decisions, c(FALSE, TRUE)), setequal(tails, c(FALSE, TRUE)))

for (size in c(5, 10, 15)) {
  for (exam in c("exam1", "exam2", "resit")) {
    questions <- generate_practice_set(18492, size, 2, exam = exam)
    stopifnot(length(questions) == size,
              identical(questions, generate_practice_set(18492, size, 2, exam = exam)))
    if (exam == "resit") {
      from_exam1 <- sum(vapply(questions, `[[`, "", "topic") %in% known_topics)
      stopifnot(abs(size - 2L * from_exam1) <= 1L)
    }
  }
}
stopifnot(identical(generate_practice_set(18492, 5, 2),
                    generate_practice_set(18492, 5, 2, exam = "exam1")),
          all(vapply(generate_practice_set(1234, 5, topics = "anova", exam = "resit"),
                     `[[`, "", "topic") == "anova"))

assumption_plots <- setNames(rep(list(rep("data:image/png;base64,test", 2)), 4),
                             c("qq", "funnel", "curve", "influence"))
first_half_types <- character()
z_wrong_tail <- t_wrong_tail <- FALSE
shapiro_decisions <- logical()
independence_examples <- character()
for (seed in 1:60) {
  set.seed(seed)
  for (level in 1:3) {
    desc <- generate_question("descriptives", level)
    values <- numbers(sub("\n\n.*", "", desc$prompt))
    answer <- if (level == 1) mean(values) else sd(values)
    stopifnot(isTRUE(all.equal(desc$answer, answer)))
    prob <- generate_question("probability", level)
    if (prob$type == "independent_joint") {
      first_half_types <- union(first_half_types, prob$type)
      stopifnot(isTRUE(all.equal(prob$answer, prod(numbers(prob$prompt)[1:2]))))
    }
    t <- generate_question("t", level)
    if (t$type == "directed_one_sample_t") {
      first_half_types <- union(first_half_types, t$type)
      # The first Score row is the descriptives. The next is the test.
      row <- as.numeric(cells(table_row(t$stimulus_html, "Score")[2]))
      p <- one_sided_p(row[1], row[3], grepl("higher than", t$prompt, fixed = TRUE))
      stopifnot(isTRUE(all.equal(t$answer, p)))
      t_wrong_tail <- t_wrong_tail || p > .5
    }
    z <- generate_question("Z", level)
    if (z$type %in% c("p_value_greater", "p_value_less")) {
      vals <- numbers(z$prompt)
      statistic <- (vals[3] - vals[5]) / (vals[1] / sqrt(vals[2]))
      p <- pnorm(statistic, lower.tail = z$type == "p_value_less")
      stopifnot(abs(z$answer - p) <= .0001)
      z_wrong_tail <- z_wrong_tail || z$answer > .5
    }
  }

  questions <- generate_practice_set(seed, 7, 3, "assumptions", "exam2")
  stopifnot(setequal(vapply(questions, "[[", "", "type"), assumption_templates[[3]]))
  for (q in questions) {
    stopifnot(length(q$choices) == 4L, !anyDuplicated(q$choices),
              q$answer %in% 1:4, grepl('class="pq-choices"', question_html(q, 1), fixed = TRUE))
    if (q$type == "shapiro") {
      p <- tail(cells(table_row(q$stimulus_html, "Regression residuals")), 1)
      significant <- startsWith(p, "&lt;") || as.numeric(p) < .05
      stopifnot(grepl(if (significant) "There is evidence" else "There is insufficient evidence",
                      q$choices[q$answer], fixed = TRUE))
      shapiro_decisions <- union(shapiro_decisions, significant)
    } else if (q$type == "independence") {
      stopifnot(is.null(q$stimulus_html), grepl("sampling plan", q$prompt, fixed = TRUE))
      independence_examples <- union(independence_examples, q$choices[q$answer])
    } else if (q$type != "omnibus") {
      stopifnot(grepl('src="data:image/png;base64,', q$stimulus_html, fixed = TRUE))
      if (q$type == "curve") {
        stopifnot(q$choices[q$answer] == "The relationship between x and y is linear.")
      }
    }
  }
}
stopifnot(setequal(first_half_types, c("independent_joint", "directed_one_sample_t")),
          z_wrong_tail, t_wrong_tail, setequal(shapiro_decisions, c(FALSE, TRUE)),
          setequal(independence_examples, c(
            "Students in the same classroom share a teacher and classroom experiences.",
            "Each volunteer contributes ratings for both versions.",
            "Several people in the sample may live in the same household."
          )))

for (direction in c(-1L, 1L)) {
  funnel <- assumption_plot_data("funnel", direction)
  low <- funnel$y[funnel$x < quantile(funnel$x, .25)]
  high <- funnel$y[funnel$x > quantile(funnel$x, .75)]
  spread_ratio <- sd(high) / sd(low)
  stopifnot(if (direction == 1L) spread_ratio > 4 else spread_ratio < 1 / 4)

  curve <- assumption_plot_data("curve", direction)
  middle <- abs(curve$x - median(curve$x)) < 3
  ends <- abs(curve$x - median(curve$x)) > 7
  stopifnot(mean(curve$y[middle]) * direction < -1, mean(curve$y[ends]) * direction > 1)

  qq <- assumption_plot_data("qq", direction)
  stopifnot(shapiro.test(qq$y)$p.value < .001)
  influence <- assumption_plot_data("influence", direction)
  cook <- cooks.distance(influence$fit)[influence$labelled]
  stopifnot(cook[2] > 4 * cook[1])
  changes <- vapply(influence$labelled, function(i) {
    fit_without <- lm(influence$y[-i] ~ influence$x[-i])
    abs(coef(fit_without)[[2]] - coef(influence$fit)[[2]])
  }, numeric(1))
  stopifnot(changes[2] > 4 * changes[1])
}

cat("Validated 4,500 calculation questions across 50 Exam 2 templates, first-half additions, seven concept types, and seeded exam modes.\n")
