# ---------------------------------------------------------------------------
# Bayes' theorem, in the shape of a diagnostic test.
#
# Every question is built from a 2x2 table of whole numbers of people, so the
# counts always add up.  Only tables whose percentages come out as whole
# numbers are offered, so a question can state the rates it was built from
# and the student's answer matches the question exactly.
# ---------------------------------------------------------------------------

bayes_population <- 1000

# People out of 1000 who have the condition: 1%, 2%, 2.5%, 5%, 10% or 20%.
bayes_cases <- c(10, 20, 25, 50, 100, 200)

# Sensitivity and false positive rate, in per cent.
bayes_sensitivities <- c(80, 85, 90, 95, 100)
bayes_false_positive_rates <- c(2, 4, 5, 8, 10, 20)

# Every frequency table whose rates are whole percentages.
#   positive_cases  people who have C and test positive
#   false_positives people who do not have C but test positive
bayes_tables <- function() {
  tables <- list()
  for (cases in bayes_cases) {
    healthy <- bayes_population - cases
    for (sensitivity in bayes_sensitivities) {
      positive_cases <- cases * sensitivity / 100
      if (positive_cases != round(positive_cases)) next
      for (fpr in bayes_false_positive_rates) {
        false_positives <- healthy * fpr / 100
        if (false_positives != round(false_positives)) next
        if (positive_cases < 1 || false_positives < 1) next
        tables[[length(tables) + 1]] <- list(
          cases = cases,
          healthy = healthy,
          positive_cases = positive_cases,
          negative_cases = cases - positive_cases,
          false_positives = false_positives,
          true_negatives = healthy - false_positives,
          sensitivity = sensitivity,
          false_positive_rate = fpr
        )
      }
    }
  }
  tables
}

# One of those tables.  `min_cases` and `keep` say which tables suit the
# question at hand; `keep` is a function of one table returning TRUE or FALSE.
bayes_pick_table <- function(min_cases = 1, keep = function(table) TRUE) {
  tables <- bayes_tables()
  tables <- Filter(function(table) table$cases >= min_cases && keep(table), tables)
  pick_one(tables)
}

# A percentage as it appears in a question, e.g. 2.5 for 25 people in 1000.
bayes_percent <- function(people) fmt_num(people / bayes_population * 100, digits = 3)

# The 2x2 table of counts, for the worked solution.
bayes_count_lines <- function(table) {
  pad <- function(x) formatC(x, width = 9)
  paste0(
    "                ", pad("has C"), pad("no C"), pad("total"), "\n",
    "  tests positive", pad(table$positive_cases), pad(table$false_positives),
    pad(table$positive_cases + table$false_positives), "\n",
    "  tests negative", pad(table$negative_cases), pad(table$true_negatives),
    pad(table$negative_cases + table$true_negatives), "\n",
    "  total         ", pad(table$cases), pad(table$healthy),
    pad(bayes_population), "\n"
  )
}

# Bayes' theorem written out with the numbers of a table.  The caller adds
# the heading, because the heading differs from question to question.
bayes_formula_lines <- function(table) {
  p_cases <- table$cases / bayes_population
  p_not_cases <- table$healthy / bayes_population
  p_sensitivity <- table$sensitivity / 100
  p_fpr <- table$false_positive_rate / 100

  numerator <- p_sensitivity * p_cases
  denominator <- numerator + p_fpr * p_not_cases

  paste0(
    "  P(C | positive)\n",
    "  = P(positive | C) * P(C)\n",
    "    / [ P(positive | C) * P(C) + P(positive | no C) * P(no C) ]\n",
    "  = (", fmt_num(p_sensitivity, 4), " * ", fmt_num(p_cases, 4), ") / (",
    fmt_num(p_sensitivity, 4), " * ", fmt_num(p_cases, 4), " + ",
    fmt_num(p_fpr, 4), " * ", fmt_num(p_not_cases, 4), ")\n",
    "  = ", fmt_num(numerator, 4), " / (", fmt_num(numerator, 4), " + ",
    fmt_num(p_fpr * p_not_cases, 4), ")\n",
    "  = ", fmt_num(numerator, 4), " / ", fmt_num(denominator, 4), "\n",
    "  = ", sprintf("%.4f", numerator / denominator), "\n"
  )
}

# Difficulty 1: the three quantities are given in plain language, and the
# question asks for P(C | positive) directly.
bayes_question_plain <- function(difficulty) {
  table <- bayes_pick_table(min_cases = 50)
  positives <- table$positive_cases + table$false_positives
  answer <- round_prob(table$positive_cases / positives)

  list(
    topic = "bayes",
    type = "test_positive",
    difficulty = difficulty,
    prompt = paste0(
      "In a hypothetical population, ", bayes_percent(table$cases),
      "% of people have a condition C. A test for C gives a positive result ",
      "for ", table$sensitivity, "% of the people who have C, and for ",
      table$false_positive_rate, "% of the people who do not have C.\n\n",
      "If a person tests positive, what is the probability that they have C? ",
      "Give your answer to four decimal places."
    ),
    answer = answer,
    tolerance = 0.005,
    hint = paste0(
      "Write the information down as counts for 1000 people, then read ",
      "P(C | positive) straight off the table."
    ),
    solution = paste0(
      "Work with whole people rather than probabilities. Take 1000 people:\n\n",
      bayes_count_lines(table), "\n",
      "  ", table$cases, " people have C, and the test is positive for ",
      table$sensitivity, "% of them.\n",
      "  ", table$healthy, " people do not have C, and the test is positive ",
      "for ", table$false_positive_rate, "% of them.\n\n",
      "Of the ", positives, " positive results, ", table$positive_cases,
      " come from people who have C:\n\n",
      "  P(C | positive)\n",
      "  = ", table$positive_cases, " / (", table$positive_cases, " + ",
      table$false_positives, ")\n",
      "  = ", table$positive_cases, " / ", positives, "\n",
      "  = ", sprintf("%.4f", answer), "\n\n",
      "The same calculation in probability form:\n\n",
      bayes_formula_lines(table)
    )
  )
}

# Difficulty 2: the same question, but in the language of tests, so the
# student has to translate the words into probabilities first.
bayes_question_jargon <- function(difficulty) {
  table <- bayes_pick_table(min_cases = 20)
  positives <- table$positive_cases + table$false_positives
  answer <- round_prob(table$positive_cases / positives)

  list(
    topic = "bayes",
    type = "prevalence_sensitivity",
    difficulty = difficulty,
    prompt = paste0(
      "In a hypothetical population the prevalence of a condition C is ",
      bayes_percent(table$cases), "%. A test for C has a sensitivity of ",
      table$sensitivity, "% and a false positive rate of ",
      table$false_positive_rate, "%.\n\n",
      "What is the probability that a person who tests positive actually ",
      "has C? Give your answer to four decimal places."
    ),
    answer = answer,
    tolerance = 0.005,
    hint = paste0(
      "The prevalence is P(C), the sensitivity is P(positive | C), and the ",
      "false positive rate is P(positive | no C)."
    ),
    solution = paste0(
      "First put the words into probabilities:\n\n",
      "  prevalence          P(C)               = ",
      fmt_num(table$cases / bayes_population, 4), "\n",
      "  sensitivity         P(positive | C)    = ",
      fmt_num(table$sensitivity / 100, 4), "\n",
      "  false positive rate P(positive | no C) = ",
      fmt_num(table$false_positive_rate / 100, 4), "\n",
      "  and therefore       P(no C)            = ",
      fmt_num(table$healthy / bayes_population, 4), "\n\n",
      "Bayes' theorem:\n\n",
      bayes_formula_lines(table), "\n",
      "The same answer, from a table of counts for 1000 people:\n\n",
      bayes_count_lines(table), "\n",
      "  P(C | positive) = ", table$positive_cases, " / ", positives, " = ",
      sprintf("%.4f", answer), "\n"
    )
  )
}

# Difficulty 3: a short word problem.  The counts are given, so the student
# has to work out which cells of the table the question is about.
bayes_question_counts <- function(difficulty) {
  table <- bayes_pick_table(
    min_cases = 20,
    keep = function(table) table$negative_cases >= 1 && table$false_positives >= 5
  )
  positives <- table$positive_cases + table$false_positives
  negatives <- table$negative_cases + table$true_negatives

  given <- paste0(
    "In a hypothetical population of ", bayes_population, " people:\n\n",
    "  - ", table$cases, " people have condition C, and ",
    table$positive_cases, " of them test positive\n",
    "  - ", table$healthy, " people do not have C, and ",
    table$false_positives, " of them test positive\n\n"
  )

  if (pick_one(c("negative", "positive")) == "negative") {
    # P(no C | tests negative).
    answer <- round_prob(table$true_negatives / negatives)
    return(list(
      topic = "bayes",
      type = "negative_predictive",
      difficulty = difficulty,
      prompt = paste0(
        given,
        "If a person tests negative, what is the probability that they do ",
        "not have C? Give your answer to four decimal places."
      ),
      answer = answer,
      tolerance = 0.005,
      hint = paste0(
        "Build the 2x2 table of counts first. Then count how many people ",
        "test negative, and how many of those do not have C."
      ),
      solution = paste0(
        "Fill in the whole table first:\n\n",
        bayes_count_lines(table), "\n",
        "  ", table$cases, " people have C, and ", table$positive_cases,
        " of them test positive, so ", table$negative_cases,
        " test negative.\n",
        "  ", table$healthy, " people do not have C, and ",
        table$false_positives, " of them test positive, so ",
        table$true_negatives, " test negative.\n\n",
        "The question is about the people who test negative:\n\n",
        "  P(no C | tests negative)\n",
        "  = ", table$true_negatives, " / (", table$true_negatives, " + ",
        table$negative_cases, ")\n",
        "  = ", table$true_negatives, " / ", negatives, "\n",
        "  = ", sprintf("%.4f", answer), "\n"
      )
    ))
  }

  # What share of the positive results are false positives?
  answer <- round_prob(table$false_positives / positives)
  list(
    topic = "bayes",
    type = "false_positive_share",
    difficulty = difficulty,
    prompt = paste0(
      given,
      "Of all the people who test positive, what proportion do not have C? ",
      "Give your answer to four decimal places."
    ),
    answer = answer,
    tolerance = 0.005,
    hint = paste0(
      "Build the 2x2 table of counts first. The question is about the ",
      "positive column of the table: which part of it comes from people who ",
      "do not have C?"
    ),
    solution = paste0(
      "Fill in the whole table first:\n\n",
      bayes_count_lines(table), "\n",
      "  ", table$cases, " people have C, and ", table$positive_cases,
      " of them test positive, so ", table$negative_cases,
      " test negative.\n",
      "  ", table$healthy, " people do not have C, and ",
      table$false_positives, " of them test positive, so ",
      table$true_negatives, " test negative.\n\n",
      "There are ", positives, " positive results in total, of which ",
      table$false_positives, " come from people who do not have C:\n\n",
      "  P(no C | positive)\n",
      "  = ", table$false_positives, " / ", positives, "\n",
      "  = ", sprintf("%.4f", answer), "\n"
    )
  )
}

# One Bayes question.
generate_bayes_question <- function(difficulty = 1) {
  difficulty <- clamp(difficulty, 1, 3, 1)

  if (difficulty == 1) {
    bayes_question_plain(difficulty)
  } else if (difficulty == 2) {
    bayes_question_jargon(difficulty)
  } else {
    bayes_question_counts(difficulty)
  }
}
