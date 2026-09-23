# ---------------------------------------------------------------------------
# Shared helpers.
#
# Nothing in here is clever: these are the small functions that more than one
# topic generator needs.  If a helper is only used by one topic, it lives in
# that topic's file instead.  Base R only.
# ---------------------------------------------------------------------------

# The topics the practice tool knows about.
known_topics <- c("normal", "clt", "bayes")

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
clean_topics <- function(topics) {
  topics <- unique(topics[topics %in% known_topics])
  if (length(topics) == 0) topics <- known_topics
  topics
}

# A readable name for a topic.
topic_label <- function(topic) {
  switch(topic,
    normal = "Normal distribution",
    clt    = "Central limit theorem",
    bayes  = "Bayes theorem",
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

# --- turning questions into HTML -------------------------------------------

# One question, as HTML.  The browser shows this and hands the typed answer
# back to check_answer_text(); everything else is already in the page.
question_html <- function(question, number) {
  paste0(
    '<div class="pq-question">\n',
    '<h3>Question ', number, ' <span class="pq-topic">',
    esc(topic_label(question$topic)), '</span></h3>\n',
    '<p class="pq-prompt">', esc(question$prompt), '</p>\n',
    '<p><label>Your answer: <input type="text" class="pq-answer" size="12"',
    ' data-correct="', fmt_num(question$answer, digits = 12), '"',
    ' data-tolerance="', fmt_num(question$tolerance, digits = 12), '"></label> ',
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
                          topics = known_topics) {
  seed <- clean_seed(seed)
  n_questions <- clamp(n_questions, 1, 50, 10)
  difficulty <- clamp(difficulty, 1, 3, 1)
  topics <- clean_topics(topics)

  questions <- generate_practice_set(seed, n_questions, difficulty, topics)

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
