# SPSS-style output for exam questions, using base R.
# Use format = "testvision" for a question fragment or "standalone" for a styled page.
# Each generator returns an HTML string, invisibly when it is also written to file.

.spss_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

.spss_number <- function(x) {
  ifelse(is.na(x), "", formatC(x, digits = 3, format = "f", decimal.mark = "."))
}

.spss_df <- function(x) {
  ifelse(!is.na(x) & x == trunc(x), as.character(x), .spss_number(x))
}

.spss_omit_leading_zero <- function(x) sub("^(-?)0\\.", "\\1.", x)

.spss_significance <- function(p) {
  ifelse(!is.na(p) & p < .001, "<.001", .spss_omit_leading_zero(.spss_number(p)))
}

.spss_row <- function(label, values, leading = NULL) {
  first_cell <- if (is.null(leading)) "" else paste0("<td>", .spss_escape(leading), "</td>")
  paste0("    <tr>", first_cell, '<th scope="row">', .spss_escape(label), "</th>",
         paste0("<td>", .spss_escape(values), "</td>", collapse = ""), "</tr>")
}

.spss_headings <- function(labels) {
  paste0("    <tr>", paste0('<th scope="col">', .spss_escape(labels), "</th>", collapse = ""),
         "</tr>")
}

.spss_table <- function(caption, headers, rows, column_groups = NULL) {
  list(caption = caption, headers = headers, rows = rows, column_groups = column_groups)
}

.spss_anova_table <- function(model, regression = FALSE, f = NULL, hide_calculations = FALSE) {
  anova <- stats::anova(model)
  residual_row <- nrow(anova)
  model_ss <- sum(anova[-residual_row, "Sum Sq"])
  residual_ss <- anova[residual_row, "Sum Sq"]
  model_df <- sum(anova[-residual_row, "Df"])
  residual_df <- anova[residual_row, "Df"]
  model_ms <- model_ss / model_df
  residual_ms <- residual_ss / residual_df
  p <- NA_real_
  if (model_df > 0 && residual_df > 0) {
    if (is.null(f)) f <- model_ms / residual_ms
    p <- stats::pf(f, model_df, residual_df, lower.tail = FALSE)
  }
  labels <- if (regression) c("Regression", "Residual") else c("Between Groups", "Within Groups")
  model_tests <- if (hide_calculations) rep("", 3L) else
    c(.spss_number(model_ms), .spss_number(if (is.null(f)) NA_real_ else f), .spss_significance(p))
  residual_mean_square <- if (hide_calculations) "" else .spss_number(residual_ms)
  .spss_table(
    "ANOVA",
    .spss_headings(c(if (regression) "Model", "Source", "Sum of Squares", "df",
                     "Mean Square", "F", "Sig.")),
    c(.spss_row(labels[1], c(.spss_number(model_ss), model_df, model_tests), if (regression) "1"),
      .spss_row(labels[2], c(.spss_number(residual_ss), residual_df, residual_mean_square,
                             "", ""), if (regression) ""),
      .spss_row("Total", c(.spss_number(model_ss + residual_ss), model_df + residual_df,
                           "", "", ""), if (regression) ""))
  )
}

# term_labels is a named character vector keyed by coefficient name.
spss_regression_html <- function(model, file = NULL, title = "Regression output",
                                 term_labels = NULL, format = c("standalone", "testvision"),
                                 tables = c("model", "anova", "coefficients"), hide_beta = FALSE,
                                 hide_model = character()) {
  format <- match.arg(format)
  tables <- match.arg(tables, several.ok = TRUE)
  if (!identical(class(model), "lm")) {
    stop("model must be a single-response fit from lm().")
  }
  fit <- summary(model)
  fit_numbers <- .spss_number(c(sqrt(fit$r.squared), fit$r.squared, fit$adj.r.squared))
  fit_numbers[c("R", "R Square", "Adjusted R Square") %in% hide_model] <- ""
  model_summary <- .spss_table(
    "Model Summary",
    .spss_headings(c("Model", "R", "R Square", "Adjusted R Square", "Std. Error of the Estimate")),
    .spss_row("1", c(.spss_omit_leading_zero(fit_numbers), .spss_number(fit$sigma)))
  )
  anova_table <- .spss_anova_table(model, regression = TRUE, f = fit$fstatistic[["value"]])

  estimates <- fit$coefficients
  terms <- rownames(estimates)
  slopes <- terms != "(Intercept)"
  beta <- rep(NA_real_, nrow(estimates))
  if (any(slopes) && !hide_beta) {
    estimated_columns <- model$qr$pivot[seq_len(nrow(estimates))]
    design <- stats::model.matrix(model)[, estimated_columns[slopes], drop = FALSE]
    response <- model$fitted.values + model$residuals
    weights <- model$weights
    if (is.null(weights)) weights <- rep(1, length(response))
    # The common variance denominator cancels in the ratio of standard deviations.
    centered_ss <- function(x) sum(weights * (x - stats::weighted.mean(x, weights))^2)
    response_ss <- centered_ss(response)
    if (response_ss > 0) {
      beta[slopes] <- estimates[slopes, "Estimate"] *
        sqrt(apply(design, 2, centered_ss) / response_ss)
    }
  }
  labels <- ifelse(terms == "(Intercept)", "(Constant)", terms)
  if (!is.null(term_labels)) {
    custom <- terms %in% names(term_labels)
    labels[custom] <- term_labels[terms[custom]]
  }
  coefficient_headers <- c(
    '    <tr><th scope="col" rowspan="2">Model</th><th scope="col" rowspan="2">Term</th>',
    '        <th scope="colgroup" colspan="2">Unstandardized Coefficients</th>',
    '        <th scope="col">Standardized Coefficients</th>',
    '        <th scope="col" rowspan="2">t</th><th scope="col" rowspan="2">Sig.</th></tr>',
    .spss_headings(c("B", "Std. Error", "Beta"))
  )
  coefficient_rows <- vapply(seq_len(nrow(estimates)), function(i) {
    .spss_row(labels[i], c(.spss_number(estimates[i, c("Estimate", "Std. Error")]),
                           .spss_number(beta[i]), .spss_number(estimates[i, "t value"]),
                           .spss_significance(estimates[i, "Pr(>|t|)"])),
               if (i == 1L) "1" else "")
  }, character(1))
  coefficients_table <- .spss_table(
    "Coefficients", coefficient_headers, coefficient_rows,
    sprintf('  <colgroup span="%d"></colgroup>', c(2L, 2L, 1L, 2L))
  )
  available <- list(model = model_summary, anova = anova_table, coefficients = coefficients_table)
  .spss_output(available[tables], file, title, format)
}

.spss_output <- function(tables, file, title, format) {
  testvision <- format == "testvision"
  content <- unlist(lapply(tables, function(table) {
    caption <- .spss_escape(table$caption)
    body <- c("  <thead>", table$headers, "  </thead>",
              "  <tbody>", table$rows, "  </tbody>", "</table>")
    if (testvision) {
      # Match the elements and merged-cell attributes retained by Code View.
      body <- gsub('<th scope="[^"]+"', '<th', body)
      c(paste0("<p><strong>", caption, "</strong></p>"), "<table>", body)
    } else {
      c("<table>", paste0("  <caption>", caption, "</caption>"), table$column_groups, body)
    }
  }), use.names = FALSE)
  if (!is.null(title)) content <- c(paste0("<h1>", .spss_escape(title), "</h1>"), content)

  if (!testvision) content <- c(
    "<!doctype html>", '<html lang="en">', "<head>",
    '  <meta charset="utf-8">',
    '  <meta name="viewport" content="width=device-width, initial-scale=1">',
    paste0("  <title>", .spss_escape(if (is.null(title)) "Statistical output" else title), "</title>"),
    "  <style>",
    "    body { color: #202124; font: 16px/1.4 Arial, sans-serif; margin: 2rem; }",
    "    main { max-width: 920px; margin: auto; }",
    "    h1 { font-size: 1.35em; }",
    "    table { border-collapse: collapse; width: 100%; margin: 0 0 1.4em; break-inside: avoid; }",
    "    caption { text-align: left; font-weight: bold; margin-bottom: .35em; }",
    "    th, td { border: 1px solid #888; padding: .32em .55em; }",
    "    thead { background: #f3f3f3; }",
    "    th[scope=row] { text-align: left; font-weight: normal; }",
    "    td { text-align: right; font-variant-numeric: tabular-nums; }",
    "    @media print { body { margin: 0; } }",
    "  </style>", "</head>", "<body>", "<main>",
    content,
    "</main>", "</body>", "</html>"
  )
  html <- enc2utf8(paste(content, collapse = "\n"))
  if (is.null(file)) return(html)
  writeLines(html, file, useBytes = TRUE)
  invisible(html)
}

# model is a one-factor lm() or aov() fit, or a Welch result from oneway.test().
# hide_calculations blanks the mean squares, F and Sig. in a classical ANOVA.
spss_anova_html <- function(model, file = NULL, title = NULL,
                            format = c("testvision", "standalone"), hide_calculations = FALSE) {
  format <- match.arg(format)
  if (inherits(model, "htest") &&
      identical(model$method, "One-way analysis of means (not assuming equal variances)")) {
    if (hide_calculations) stop("This option is for classical one-way ANOVA.")
    table <- .spss_table(
      "Robust Tests of Equality of Means",
      .spss_headings(c("Method", "Statistic", "df1", "df2", "Sig.")),
      .spss_row("Welch", c(.spss_number(model$statistic), .spss_df(model$parameter),
                          .spss_significance(model$p.value)))
    )
  } else {
    if (!inherits(model, "lm") || inherits(model, c("mlm", "glm"))) {
      stop("Use a one-way fit from lm() or aov(), or a Welch result from oneway.test().")
    }
    terms <- stats::terms(model)
    if (!isTRUE(attr(terms, "intercept") == 1L) ||
        length(attr(terms, "term.labels")) != 1L || !is.null(model$offset)) {
      stop("A one-way fit must have an intercept and one categorical predictor.")
    }
    predictor <- stats::model.frame(model)[[2L]]
    if (!is.factor(predictor) && !is.character(predictor)) {
      stop("Use a factor or character vector as the ANOVA predictor.")
    }
    table <- .spss_anova_table(model, hide_calculations = hide_calculations)
  }
  .spss_output(list(table), file, title, format)
}

.spss_levene_result <- function(x, group) {
  deviations <- abs(x - stats::ave(x, group, FUN = mean))
  stats::oneway.test(deviations ~ group, var.equal = TRUE)
}

# x contains the scores and group contains the corresponding group labels.
spss_levene_html <- function(x, group, file = NULL, title = NULL,
                             format = c("testvision", "standalone")) {
  format <- match.arg(format)
  keep <- stats::complete.cases(x, group)
  group <- factor(group[keep])
  test <- .spss_levene_result(x[keep], group)
  table <- .spss_table(
    "Test of Homogeneity of Variances",
    .spss_headings(c("Method", "Levene Statistic", "df1", "df2", "Sig.")),
    .spss_row("Based on Mean", c(.spss_number(test$statistic), .spss_df(test$parameter),
                                .spss_significance(test$p.value)))
  )
  .spss_output(list(table), file, title, format)
}

# Independent samples show Student's and Welch's rows. Use y = NULL for a
# one-sample test, or paired = TRUE for a test on matched x - y differences.
# display can show only the descriptive table or leave t and its p-value blank.
spss_t_test_html <- function(x, y = NULL, paired = FALSE, mu = 0, group_labels = NULL,
                             file = NULL, title = NULL,
                             format = c("testvision", "standalone"),
                             display = c("full", "statistics", "hide_t")) {
  format <- match.arg(format)
  display <- match.arg(display)
  if (paired) {
    if (is.null(y) || length(x) != length(y)) {
      stop("A paired test needs two samples of the same length.")
    }
    keep <- stats::complete.cases(x, y)
    x <- x[keep]
    y <- y[keep]
  } else {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }
  samples <- if (is.null(y)) list(x) else list(x, y)
  independent <- length(samples) == 2L && !paired
  if (is.null(group_labels)) {
    group_labels <- if (independent) c("Group 1", "Group 2") else
      if (paired) c("Measure 1", "Measure 2") else "Sample"
  }
  if (length(group_labels) != length(samples)) {
    stop("Supply one group label for each sample.")
  }
  statistics_caption <- if (independent) "Group Statistics" else
    if (paired) "Paired Samples Statistics" else "One-Sample Statistics"
  statistics_rows <- vapply(seq_along(samples), function(i) {
    sample <- samples[[i]]
    sd <- stats::sd(sample)
    .spss_row(group_labels[i], c(length(sample), .spss_number(c(mean(sample), sd,
                                                              sd / sqrt(length(sample))))))
  }, character(1))
  statistics <- .spss_table(
    statistics_caption,
    .spss_headings(c(if (independent) "Group" else "Sample", "N", "Mean", "Std. Deviation",
                     "Std. Error Mean")),
    statistics_rows
  )
  if (display == "statistics") {
    return(.spss_output(list(statistics), file, title, format))
  }
  test_statistic <- function(test) {
    if (display == "hide_t") return(c("", ""))
    c(.spss_number(test$statistic), .spss_significance(test$p.value))
  }

  if (independent) {
    student <- stats::t.test(x, y, mu = mu, var.equal = TRUE)
    welch <- stats::t.test(x, y, mu = mu)
    group <- factor(rep(1:2, c(length(x), length(y))))
    levene <- .spss_levene_result(c(x, y), group)
    test_values <- function(test) {
      statistic <- test_statistic(test)
      c(statistic[1], .spss_df(test$parameter), statistic[2],
        .spss_number(test$estimate[1] - test$estimate[2]))
    }
    headers <- c(
      '    <tr><th scope="col" rowspan="2">Assumption</th>',
      '        <th scope="colgroup" colspan="2">Levene\'s Test for Equality of Variances</th>',
      '        <th scope="colgroup" colspan="4">t-test for Equality of Means</th></tr>',
      .spss_headings(c("F", "Sig.", "t", "df", "Sig. (2-tailed)", "Mean Difference"))
    )
    test_caption <- "Independent Samples Test"
    if (mu != 0) test_caption <- paste(test_caption, "(test value =", .spss_number(mu), ")")
    results <- .spss_table(
      test_caption, headers,
      c(.spss_row("Equal variances assumed", c(.spss_number(levene$statistic),
                                               .spss_significance(levene$p.value),
                                               test_values(student))),
        .spss_row("Equal variances not assumed", c("", "", test_values(welch)))),
      sprintf('  <colgroup span="%d"></colgroup>', c(1L, 2L, 4L))
    )
  } else {
    test <- stats::t.test(x, y, mu = mu, paired = paired)
    test_caption <- if (paired) "Paired Samples Test" else "One-Sample Test"
    test_caption <- paste0(test_caption, " (test value = ", .spss_number(mu), ")")
    comparison <- if (paired) paste(group_labels, collapse = " - ") else group_labels[1]
    # The one-sample test reports the mean and interval relative to the null value.
    shift <- if (paired) 0 else mu
    statistic <- test_statistic(test)
    results <- .spss_table(
      test_caption,
      .spss_headings(c("Sample", "t", "df", "Sig. (2-tailed)", "Mean Difference",
                       "95% CI Lower", "95% CI Upper")),
      .spss_row(comparison, c(statistic[1], .spss_df(test$parameter), statistic[2],
                              .spss_number(c(test$estimate, test$conf.int) - shift)))
    )
  }
  .spss_output(list(statistics, results), file, title, format)
}

# Pearson correlation using complete x/y pairs.
spss_correlation_html <- function(x, y, labels = c("X", "Y"), file = NULL, title = NULL,
                                  format = c("testvision", "standalone")) {
  format <- match.arg(format)
  if (length(labels) != 2L) stop("Supply two variable labels.")
  keep <- stats::complete.cases(x, y)
  test <- stats::cor.test(x[keep], y[keep], method = "pearson")
  r <- .spss_omit_leading_zero(.spss_number(test$estimate))
  p <- .spss_significance(test$p.value)
  n <- sum(keep)
  table <- .spss_table(
    "Correlations",
    .spss_headings(c("Variable", "Statistic", labels)),
    c(.spss_row("Pearson Correlation", c("1", r), labels[1]),
      .spss_row("Sig. (2-tailed)", c("", p), ""),
      .spss_row("N", c(n, n), ""),
      .spss_row("Pearson Correlation", c(r, "1"), labels[2]),
      .spss_row("Sig. (2-tailed)", c(p, ""), ""),
      .spss_row("N", c(n, n), ""))
  )
  .spss_output(list(table), file, title, format)
}

# Supply a named list of score or residual vectors, one per group or model.
spss_shapiro_html <- function(samples, file = NULL, title = NULL,
                              format = c("testvision", "standalone")) {
  format <- match.arg(format)
  rows <- vapply(names(samples), function(label) {
    scores <- samples[[label]]
    scores <- scores[!is.na(scores)]
    test <- stats::shapiro.test(scores)
    .spss_row(label, c(.spss_number(test$statistic), length(scores),
                      .spss_significance(test$p.value)))
  }, character(1))
  table <- .spss_table("Tests of Normality (Shapiro–Wilk)",
    .spss_headings(c("Sample", "Statistic", "df", "Sig.")), rows)
  .spss_output(list(table), file, title, format)
}
