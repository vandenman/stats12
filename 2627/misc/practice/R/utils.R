# ---------------------------------------------------------------------------
# Shared helpers.
#
# Nothing in here is clever: these are the small functions that more than one
# topic generator needs.  If a helper is only used by one topic, it lives in
# that topic's file instead.  Base R only.
# ---------------------------------------------------------------------------

# The topics the practice tool knows about, in the order the form lists them.
known_topics <- c("probability", "Z", "t", "normal", "clt", "bayes", "descriptives")
exam2_topics <- c("regression", "diagnostics", "dummies", "independent_t", "anova")

practice_topics <- function(exam, selectable = FALSE) {
  second_half <- c(exam2_topics, if (selectable) "assumptions")
  switch(exam,
    exam1 = known_topics,
    exam2 = second_half,
    resit = c(known_topics, second_half),
    stop("Unknown exam: ", exam)
  )
}

# Probabilities are reported to four decimals, i.e. to the precision of the
# z table that students use.
round_prob <- function(x) round(x, 4)

# A number, printed without scientific notation and without trailing zeros.
fmt_num <- function(x, digits = 6) {
  format(x, digits = digits, scientific = FALSE, trim = TRUE)
}

# z values are always printed with two decimals, e.g. -1.00.
fmt_z <- function(z) sprintf("%.2f", z)

# Text that is placed inside HTML: escape the three characters that would
# otherwise be read as markup, so that "P(X < 78)" comes out intact.
esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# One random element of a vector or a list, using the seed set by set.seed().
# [[ is used rather than [ so that lists (such as the pools of standard
# errors and frequency tables) give back the element itself and not a
# one-element sub-list.
pick_one <- function(x) x[[sample.int(length(x), 1)]]

# A whole number kept inside a range.  Anything unusable falls back.
clamp <- function(x, low, high, fallback) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (length(x) == 0 || is.na(x) || !is.finite(x)) return(fallback)
  max(low, min(high, as.integer(x)))
}

# The seed has to survive set.seed().  Empty, negative, fractional or very
# large input falls back to 1234, which is also what the form starts with.
clean_seed <- function(seed) {
  seed <- suppressWarnings(as.numeric(seed)[1])
  if (length(seed) == 0 || is.na(seed) || !is.finite(seed)) return(1234L)
  seed <- as.integer(abs(round(seed)) %% 2147483646)
  if (is.na(seed) || seed < 1) return(1234L)
  seed
}

# Only the topics we know about, in the order the form lists them.
clean_topics <- function(topics, allowed = known_topics) {
  topics <- unique(topics[topics %in% allowed])
  if (length(topics) == 0) topics <- allowed
  topics
}

# A readable name for a topic.
topic_label <- function(topic) {
  switch(topic,
    probability = "Probability theory",
    Z           = "Z-test",
    t           = "t-test",
    normal      = "Normal distribution",
    clt         = "Central limit theorem",
    bayes       = "Bayes theorem",
    descriptives = "Descriptive statistics",
    regression  = "Regression and fit",
    diagnostics = "Regression diagnostics",
    dummies     = "Dummy coding",
    independent_t = "Independent-samples t-tests",
    anova       = "One-way ANOVA",
    assumptions = "Assumption checks and interpretation",
    topic
  )
}

# --- the z table -----------------------------------------------------------

# The value of Phi(z) that a student reads off the table.  Our table lists
# nonnegative z only, so for negative z we use Phi(z) = 1 - Phi(-z).
phi_from_table <- function(z) {
  if (z >= 0) return(round_prob(pnorm(z)))
  round_prob(1 - round_prob(pnorm(-z)))
}

# One line of working for a table look-up, e.g.
#   Phi(-1.00) = 1 - Phi(1.00) = 1 - 0.8413 = 0.1587
phi_line <- function(z) {
  if (z >= 0) {
    return(paste0("Phi(", fmt_z(z), ") = ", sprintf("%.4f", phi_from_table(z))))
  }
  paste0("Phi(", fmt_z(z), ") = 1 - Phi(", fmt_z(-z), ") = 1 - ",
         sprintf("%.4f", phi_from_table(-z)), " = ",
         sprintf("%.4f", phi_from_table(z)))
}

# Probabilities with their z values, for the questions that work backwards
# from a probability.  In each case the table entry is the closest one to the
# probability, so the exercise is unambiguous.
z_percentiles <- list(
  list(p = 0.90, z =  1.28),
  list(p = 0.80, z =  0.84),
  list(p = 0.75, z =  0.67),
  list(p = 0.99, z =  2.33),
  list(p = 0.25, z = -0.67),
  list(p = 0.10, z = -1.28),
  list(p = 0.01, z = -2.33)
)

# --- grading ---------------------------------------------------------------

# The only piece of grading there is: compare a typed answer with the correct
# answer, within the tolerance that comes with the question.  Returns the
# feedback text, which the browser shows below the answer box.
check_answer_text <- function(student, correct, tolerance) {
  student <- trimws(as.character(student))
  if (!nzchar(student)) {
    return("Type a number in the box first.")
  }

  value <- suppressWarnings(as.numeric(student))
  if (is.na(value)) {
    return("That is not a number. Try something like 0.6826")
  }

  if (abs(value - correct) <= tolerance) {
    return("Correct!")
  }

  # A very common slip: answering with a percentage instead of a probability.
  if (correct > 0 && correct < 1 && abs(value - 100 * correct) <= 100 * tolerance) {
    return("Not correct yet. That looks like a percentage; probabilities run from 0 to 1.")
  }

  "Not correct yet. Check which quantities the question asks for, and the arithmetic in your working."
}

practice_number_question <- function(topic, type, difficulty, prompt, answer, hint, working,
                                     stimulus_html = NULL, whole = FALSE, digits = 2L) {
  if (whole) digits <- 0L
  instruction <- switch(as.character(digits),
    "0" = "Give a whole number.",
    "2" = "Round your answer to two decimal places.",
    "4" = "Round your answer to four decimal places."
  )
  list(
    topic = topic, type = type, difficulty = difficulty,
    prompt = paste(prompt, instruction, sep = "\n\n"),
    answer = answer, tolerance = if (whole) 0 else 0.5 * 10^-digits + 1e-8,
    hint = hint,
    solution = paste0(working, "\n\nAnswer = ",
                      formatC(round(answer, digits), format = "f", digits = digits, decimal.mark = ".")),
    stimulus_html = stimulus_html
  )
}

# The table p-value is for a two-sided test against zero.
practice_directed_p <- function(estimate, two_sided, direction) {
  aligned <- estimate * direction > 0
  list(
    p = if (aligned) two_sided / 2 else 1 - two_sided / 2,
    working = if (aligned) {
      sprintf("The estimate points in the direction of H₁.\nOne-sided p = %s / 2", two_sided)
    } else {
      sprintf("The estimate points in the opposite direction to H₁.\nOne-sided p = 1 - %s / 2", two_sided)
    }
  )
}

# --- turning questions into HTML -------------------------------------------

# One question, as HTML.  The browser shows this and hands the typed answer
# back to check_answer_text(); everything else is already in the page.
#
# Questions that contain a small aligned block (a frequency table, a list of
# data values) get a prompt class that keeps the spaces and a monospace font,
# otherwise the columns of the block line up nowhere.  Everything else is
# ordinary prose.
question_html <- function(question, number) {
  prompt_class <- if (grepl("\n +", question$prompt)) {
    "pq-prompt pq-prompt-fixed"
  } else {
    "pq-prompt"
  }
  stimulus <- if (!is.null(question$stimulus_html) && nzchar(question$stimulus_html)) {
    # The stimulus is markup produced by the course's table formatters.
    paste0('<div class="pq-stimulus">\n', question$stimulus_html, '\n</div>\n')
  } else {
    ""
  }
  answer_widget <- if (is.null(question$choices)) {
    paste0('<p><label>Your answer: <input type="text" class="pq-answer" size="12"',
           ' data-correct="', fmt_num(question$answer, digits = 12), '"',
           ' data-tolerance="', fmt_num(question$tolerance, digits = 12), '"></label> ')
  } else {
    choices <- paste0(
      '<label><input type="radio" class="pq-choice" name="pq-choice-', number,
      '" value="', seq_along(question$choices), '"> ', esc(question$choices), '</label>'
    )
    paste0('<fieldset class="pq-choices" data-correct="', question$answer, '">',
           '<legend>Choose one answer</legend>', paste(choices, collapse = "\n"), '</fieldset>\n<p>')
  }

  paste0(
    '<div class="pq-question">\n',
    '<h3>Question ', number, ' <span class="pq-topic">',
    esc(topic_label(question$topic)), '</span></h3>\n',
    '<p class="', prompt_class, '">', esc(question$prompt), '</p>\n',
    stimulus,
    answer_widget,
    '<button type="button" class="pq-check">Check answer</button></p>\n',
    '<p class="pq-feedback"></p>\n',
    '<p><button type="button" class="pq-hint-button">Show hint</button> ',
    '<button type="button" class="pq-solution-button">Show worked solution</button></p>\n',
    '<div class="pq-hint" hidden><p><strong>Hint.</strong> ',
    esc(question$hint), '</p></div>\n',
    '<div class="pq-solution" hidden><p><strong>Worked solution.</strong></p><pre>',
    esc(question$solution), '</pre></div>\n',
    '</div>\n'
  )
}

# The whole practice set: the settings it was generated from, then the
# questions.  This is the function the browser calls.
practice_html <- function(seed, n_questions = 10, difficulty = 1,
                          topics = practice_topics(exam), exam = "exam1") {
  seed <- clean_seed(seed)
  n_questions <- clamp(n_questions, 1, 50, 10)
  difficulty <- clamp(difficulty, 1, 3, 1)
  topics <- clean_topics(topics, practice_topics(exam, selectable = TRUE))

  questions <- generate_practice_set(seed, n_questions, difficulty, topics, exam)

  header <- paste0(
    '<div class="pq-header">\n',
    '<p>Seed <strong>', seed, '</strong> &middot; difficulty <strong>',
    difficulty, '</strong> &middot; ', length(questions), ' questions &middot; ',
    esc(paste(vapply(topics, topic_label, ""), collapse = ", ")), '</p>\n',
    '<p class="pq-tip">Write down the seed, the difficulty and the question ',
    'number if you want to ask about one of these questions later.</p>\n',
    '</div>\n'
  )

  body <- vapply(seq_along(questions), function(i) {
    question_html(questions[[i]], i)
  }, "")

  paste0(header, paste(body, collapse = "\n"))
}
