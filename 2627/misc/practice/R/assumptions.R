assumption_templates <- list(
  c("qq", "funnel", "shapiro"),
  c("qq", "funnel", "curve", "influence", "shapiro", "omnibus"),
  c("qq", "funnel", "curve", "independence", "influence", "shapiro", "omnibus")
)

# Fixed patterns keep the visual signal clear across seeds. In the residual
# plots, balanced errors keep the centre at zero unless curvature is intended.
assumption_plot_data <- function(type, direction) {
  if (type == "qq") {
    y <- direction * qexp(ppoints(32))^1.5
    return(list(y = as.numeric(scale(y)), xlab = "Theoretical normal quantiles",
                ylab = "Ordered standardized residuals"))
  }

  if (type == "influence") {
    x <- rep(seq(-3, 3, length.out = 16), each = 2)
    y <- 18 + 1.3 * x + rep(c(-.75, .65), length(x) / 2)
    x <- direction * c(x, 0, 7.5)
    y <- c(y, 24, 15)
    fit <- lm(y ~ x)
    return(list(x = x, y = y, xlab = "Predictor", ylab = "Outcome",
                fit = fit, labelled = tail(seq_along(x), 2)))
  }

  x <- seq(-3, 3, length.out = 13)
  pairs <- rep(x, each = 4)
  spread <- if (type == "funnel") .20 + 2.8 * ((direction * x + 3) / 6)^1.25 else rep(.58, length(x))
  errors <- as.vector(outer(c(-1, -.42, .42, 1), spread) *
                        rep(1 + .13 * sin(seq_along(x) * 2), each = 4))
  curve <- if (type == "curve") direction * .85 * pairs^2 else 0
  fit <- lm((18 + 3 * pairs + curve + errors) ~ pairs)
  list(x = fitted(fit), y = residuals(fit), xlab = "Fitted value", ylab = "Residual")
}

build_assumption_plots <- function() {
  # Render once in Quarto. WebR samples the embedded plots without a graphics device.
  types <- c("qq", "funnel", "curve", "influence")
  plots <- lapply(types, function(type) {
    vapply(c(-1L, 1L), function(direction) {
      spec <- assumption_plot_data(type, direction)
      path <- tempfile(fileext = ".png")
      grDevices::png(path, width = 610, height = 370, res = 118)
      tryCatch({
        graphics::par(mar = c(4.35, 4.45, .55, .5), las = 1)
        if (type == "qq") {
          stats::qqnorm(spec$y, xlab = spec$xlab, ylab = spec$ylab, main = "",
                        pch = 19, col = "#205584")
          stats::qqline(spec$y, lty = 2, lwd = 1.4, col = "#535960")
        } else if (type == "influence") {
          graphics::plot(spec$x, spec$y, xlab = spec$xlab, ylab = spec$ylab,
                         pch = 19, col = "#205584", ylim = extendrange(spec$y, f = .13))
          graphics::abline(spec$fit, lty = 2, lwd = 1.4, col = "#535960")
          graphics::text(spec$x[spec$labelled], spec$y[spec$labelled],
                         c("A", "B"), pos = 3, font = 2)
        } else {
          graphics::plot(spec$x, spec$y, xlab = spec$xlab, ylab = spec$ylab,
                         pch = 19, col = "#205584", ylim = range(c(0, spec$y)) * 1.10)
          graphics::abline(h = 0, lty = 2, lwd = 1.4, col = "#535960")
        }
      }, finally = grDevices::dev.off())
      uri <- base64enc::dataURI(file = path, mime = "image/png")
      unlink(path)
      uri
    }, character(1))
  })
  setNames(plots, types)
}

practice_assumption_question <- function(difficulty, type = NA_character_) {
  if (is.na(type)) type <- pick_one(assumption_templates[[difficulty]])
  question <- function(prompt, choices, hint, solution, stimulus_html = NULL) {
    order <- sample.int(length(choices))
    list(topic = "assumptions", type = type, difficulty = difficulty, prompt = prompt,
         choices = unname(choices[order]), answer = match(1L, order),
         hint = hint, solution = solution, stimulus_html = stimulus_html)
  }

  if (type == "independence") {
    scenario <- pick_one(list(
      list(
        story = paste("A researcher compares reading scores across teaching methods.",
          "They recruit several students from each classroom and enter one row per student."),
        choices = c("Students in the same classroom share a teacher and classroom experiences.",
                    "Reading scores are recorded in points for every student.",
                    "Teaching method is a categorical variable.",
                    "There are more students than teaching methods."),
        why = "Students in the same classroom share experiences that may make their reading scores more similar, even after accounting for teaching method."
      ),
      list(
        story = paste("A study compares how clearly people understand two versions of a questionnaire.",
          "Each volunteer rates both versions and contributes a separate row for each rating.",
          "The researcher changes which version is shown first across volunteers."),
        choices = c("Each volunteer contributes ratings for both versions.",
                    "Different volunteers see the versions in different orders.",
                    "Participants use the same rating scale for both versions.",
                    "The versions ask about the same topic."),
        why = "Two ratings from the same person can be related. Changing the presentation order does not remove that link."
      ),
      list(
        story = paste("To study commuting stress, a researcher recruits employees",
          "and asks them to invite other members of their household.",
          "The analysis has one row per person and predicts stress from commute duration."),
        choices = c("Several people in the sample may live in the same household.",
                    "Commute duration is measured in minutes.",
                    "Some people volunteer to take part.",
                    "There is one row for each person."),
        why = "People from the same household can share routines and circumstances that affect stress. That shared setting can make their errors related."
      )
    ))
    return(question(paste(scenario$story, "Which feature of the sampling plan most directly puts independence at risk?"),
      scenario$choices, "Think about what each row represents and what different rows may share.",
      scenario$why))
  }

  if (type == "shapiro") {
    n <- pick_one(c(18L, 23L, 28L))
    skewed <- pick_one(c(FALSE, TRUE))
    residuals <- if (skewed) qexp(ppoints(n))^1.5 else qnorm(ppoints(n)) + .07 * sin(seq_len(n))
    residuals <- residuals - mean(residuals)
    test <- shapiro.test(residuals)
    reject <- test$p.value < .05
    p <- .spss_significance(test$p.value)
    choices <- c("There is evidence that the residual distribution departs from normality.",
                 "There is insufficient evidence of a departure from residual normality.",
                 "There is evidence that the residual variance changes across fitted values.",
                 "There is evidence that the predictor is not normally distributed.")
    if (!reject) choices[1:2] <- choices[2:1]
    comparison <- if (reject) "below .05" else "above .05"
    return(question(
      "A Shapiro–Wilk test was run on a regression's residuals. What does the output support at α = .05?",
      choices, "The null hypothesis concerns the shape of the residual distribution.",
      sprintf("The displayed p-value (%s) is %s. %s",
        p, comparison, if (reject) "Reject the null hypothesis of normality." else
          "Fail to reject normality. The result does not establish that the errors are normal."),
      spss_shapiro_html(list("Regression residuals" = residuals))))
  }

  if (type == "omnibus") {
    means <- pick_one(14:20) + c(-5, 0, 5)
    samples <- exam2_group_samples(3, means = means, sizes = c(8, 9, 10), sds = c(2, 2, 2))
    groups <- exam2_group_data(samples)
    fit <- aov(score ~ group, groups)
    return(question("What conclusion is supported by this one-way ANOVA at α = .05?",
      c("At least two population means differ.",
        "Every pair of population means differs.",
        "The population variances differ.",
        "The population means are all equal."),
      "Think about the null hypothesis of the overall F-test.",
      "The omnibus test rejects equality of the population means. It does not identify which pairs differ.",
      spss_anova_html(fit)))
  }

  variant <- sample.int(2, 1)
  if (type == "influence") {
    alt <- "Most points cluster along a trend. A lies above the middle of the cluster. B is far out on the predictor axis and away from the trend."
    return(question(
      "The dashed line is fitted to all observations. Which statement about omitting one labelled point is best supported?",
      c("Omitting B is likely to change the slope more because B is far out on the predictor axis and away from the trend.",
        "Omitting A is likely to change the slope more because A has the highest observed outcome.",
        "The slope must change by the same amount when either point is omitted.",
        "Omitting B could change the intercept only."),
      "Influence depends on the predictor position as well as the distance from the fitted line.",
      "B has high leverage and departs from the trend. Its Cook's distance in this fitted model is much larger than A's.",
      sprintf('<img class="pq-diagnostic-plot" src="%s" alt="%s">', assumption_plots[[type]][variant], alt)))
  }

  alt <- switch(type,
    qq = "The ordered residual points bend strongly away from the diagonal reference line, particularly in the tails.",
    funnel = sprintf("Residuals centre near the zero line. The points spread out on the %s side and bunch near zero on the other side.",
                     if (variant == 1) "left" else "right"),
    curve = sprintf("The middle fitted values lie mostly %s zero, while both ends mostly lie on the other side.",
                    if (variant == 1) "above" else "below"))
  choices <- c(normality = "The errors are normally distributed.",
               funnel = "The error variance is the same at all fitted values.",
               curve = "The relationship between x and y is linear.",
               independence = "The observations are independent.")
  hint <- switch(type,
    qq = "A normal Q–Q plot compares the residual quantiles with normal quantiles.",
    funnel = "Compare the vertical spread at small and large fitted values.",
    curve = "Look at where the residuals sit relative to zero across the plot.")
  explanation <- switch(type,
    qq = "The systematic bend away from the normal reference line is evidence of non-normal residuals.",
    funnel = "The size of the prediction errors changes with the fitted value. This is evidence of unequal error variance.",
    curve = "A straight line leaves a curved pattern in the residuals. That suggests the relationship between x and y is not linear.")
  stimulus <- sprintf('<img class="pq-diagnostic-plot" src="%s" alt="%s">',
                      assumption_plots[[type]][variant], esc(alt))
  if (type == "qq") {
    return(question("The figure shows a normal Q–Q plot of residuals. Which interpretation best matches the plot?",
      c("The points curve away from the line in the tails, suggesting non-normal residuals.",
        "The points follow the line closely, giving little evidence against normality.",
        "The error variance increases across the fitted values.",
        "The predictor has a skewed distribution."),
      hint, explanation, stimulus))
  }
  question("The figure shows residuals against fitted values. Which assumption does the pattern most directly call into question?",
           c(choices[type], choices[names(choices) != type]), hint, explanation, stimulus)
}
