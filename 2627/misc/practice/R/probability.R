# ---------------------------------------------------------------------------
# Probability theory: joint, marginal and conditional probabilities, read off
# a small frequency table.
#
# Every table is built from a base table out of 100, scaled up to a round
# sample size.  That keeps the counts easy to add up, so the arithmetic stays
# in the background and the definitions are what the exercise is about.
# ---------------------------------------------------------------------------

# Base tables out of 100, given as the four cell counts:
#
#   f11  f12     <- first row
#   f21  f22     <- second row
#
# Each row adds up to 100.  Add a row to add another table to the pool.
probability_bases <- data.frame(
  f11 = c( 8, 12, 20, 25, 30, 10, 15, 40),
  f12 = c(32, 48, 30, 25, 20, 40, 35, 20),
  f21 = c(12, 18, 10, 15, 15,  5, 20, 10),
  f22 = c(48, 22, 40, 35, 35, 45, 30, 30)
)

# Sample sizes: the base table multiplied by one of these.
probability_scales <- c(1, 2, 5, 10)

# Neutral label sets, so that the same table structure looks like different
# little surveys.
probability_labels <- list(
  list(rows = c("Support", "Do not support"), cols = c("Group A", "Group B")),
  list(rows = c("Yes", "No"), cols = c("Treatment", "Control")),
  list(rows = c("Pass", "Fail"), cols = c("Female", "Male"))
)

# One table: the four cells and the margins.
probability_table <- function() {
  base <- probability_bases[pick_one(seq_len(nrow(probability_bases))), ]
  scale <- pick_one(probability_scales)
  f11 <- base$f11 * scale
  f12 <- base$f12 * scale
  f21 <- base$f21 * scale
  f22 <- base$f22 * scale

  list(
    f11 = f11, f12 = f12, f21 = f21, f22 = f22,
    row1 = f11 + f12, row2 = f21 + f22,
    col1 = f11 + f21, col2 = f12 + f22,
    total = f11 + f12 + f21 + f22
  )
}

# The table as plain text.  Cells named in `blank` are shown as "?".  The
# names are f11, f12, f21, f22 for the cells and row1, row2, col1, col2 and
# total for the margins.
probability_table_lines <- function(table, labels, blank = character()) {
  number <- function(name, value) {
    formatC(if (name %in% blank) "?" else value, width = 9)
  }

  paste0(
    "  ", formatC("", width = 15), formatC(labels$cols[1], width = 9),
    formatC(labels$cols[2], width = 9), formatC("Total", width = 9), "\n",
    "  ", formatC(labels$rows[1], width = 15), number("f11", table$f11),
    number("f12", table$f12), number("row1", table$row1), "\n",
    "  ", formatC(labels$rows[2], width = 15), number("f21", table$f21),
    number("f22", table$f22), number("row2", table$row2), "\n",
    "  ", formatC("Total", width = 15), number("col1", table$col1),
    number("col2", table$col2), number("total", table$total), "\n"
  )
}

# The opening lines of every probability question.
probability_intro <- function(table, labels, blank = character()) {
  paste0(
    "A sample of ", table$total, " people was classified in two ways:\n\n",
    probability_table_lines(table, labels, blank)
  )
}

# Difficulty 1: a joint or a marginal probability, straight off a table.
probability_question_simple <- function(difficulty) {
  table <- probability_table()
  labels <- pick_one(probability_labels)
  intro <- probability_intro(table, labels)

  if (pick_one(c("joint", "marginal")) == "joint") {
    i <- pick_one(1:2)
    j <- pick_one(1:2)
    count <- table[[paste0("f", i, j)]]
    answer <- round_prob(count / table$total)
    asked <- paste0("P(", labels$rows[i], " and ", labels$cols[j], ")")

    prompt <- paste0(
      intro, "\nWhat is the joint probability ", asked,
      "? That is, the probability that a randomly chosen person is in both the '",
      labels$rows[i], "' row and the '", labels$cols[j],
      "' column. Give your answer to four decimal places."
    )
    hint <- paste0(
      "A joint probability is one cell divided by the total: how many people ",
      "are in both categories at once, out of everybody."
    )
    solution <- paste0(
      "The cell for '", labels$rows[i], " and ", labels$cols[j], "' contains ",
      count, " of the ", table$total, " people:\n\n",
      "  ", asked, " = ", count, " / ", table$total, " = ",
      sprintf("%.4f", answer), "\n"
    )
    type <- "joint"
  } else {
    if (pick_one(c("row", "column")) == "row") {
      i <- pick_one(1:2)
      count <- table[[paste0("row", i)]]
      what <- labels$rows[i]
      where <- paste0("'", labels$rows[i], "' row")
      sum_line <- paste0(
        "Add up the '", labels$rows[i], "' row: ", table[[paste0("f", i, 1)]],
        " + ", table[[paste0("f", i, 2)]], " = ", count, " people."
      )
    } else {
      j <- pick_one(1:2)
      count <- table[[paste0("col", j)]]
      what <- labels$cols[j]
      where <- paste0("'", labels$cols[j], "' column")
      sum_line <- paste0(
        "Add up the '", labels$cols[j], "' column: ", table[[paste0("f1", j)]],
        " + ", table[[paste0("f2", j)]], " = ", count, " people."
      )
    }
    answer <- round_prob(count / table$total)
    asked <- paste0("P(", what, ")")

    prompt <- paste0(
      intro, "\nWhat is the marginal probability ", asked,
      "? That is, the probability that a randomly chosen person is in the ",
      where, ". Give your answer to four decimal places."
    )
    hint <- paste0(
      "A marginal probability uses a row or column total, not a single cell: ",
      "add up one row or one column first."
    )
    solution <- paste0(
      sum_line, " That total is the marginal count for '", what, "':\n\n",
      "  ", asked, " = ", count, " / ", table$total, " = ",
      sprintf("%.4f", answer), "\n"
    )
    type <- "marginal"
  }

  list(
    topic = "probability",
    type = type,
    difficulty = difficulty,
    prompt = prompt,
    answer = answer,
    tolerance = 0.002,
    hint = hint,
    solution = solution
  )
}

# Difficulty 2: a conditional probability, where the denominator is the group
# we condition on rather than the whole sample.
probability_question_conditional <- function(difficulty) {
  table <- probability_table()
  labels <- pick_one(probability_labels)
  i <- pick_one(1:2)
  j <- pick_one(1:2)

  count <- table[[paste0("f", i, j)]]
  group_size <- table[[paste0("col", j)]]
  answer <- round_prob(count / group_size)
  asked <- paste0("P(", labels$rows[i], " | ", labels$cols[j], ")")

  list(
    topic = "probability",
    type = "conditional",
    difficulty = difficulty,
    prompt = paste0(
      probability_intro(table, labels),
      "\nWhat is the conditional probability ", asked,
      "? That is, the probability that someone is in the '", labels$rows[i],
      "' row, given that they are in the '", labels$cols[j],
      "' column. Give your answer to four decimal places."
    ),
    answer = answer,
    tolerance = 0.002,
    hint = paste0(
      "A conditional probability divides by the group you condition on: ",
      "P(A | B) = P(A and B) / P(B). Here that is the cell divided by the '",
      labels$cols[j], "' column total."
    ),
    solution = paste0(
      "Condition on the '", labels$cols[j], "' column: it has ", group_size,
      " people, and ", count, " of them are in the '", labels$rows[i],
      "' row.\n\n",
      "  ", asked, " = ", count, " / ", group_size, " = ",
      sprintf("%.4f", answer), "\n\n",
      "The same thing in probabilities:\n\n",
      "  P(", labels$rows[i], " | ", labels$cols[j], ") = P(",
      labels$rows[i], " and ", labels$cols[j], ") / P(", labels$cols[j], ")\n",
      "  = ", sprintf("%.4f", count / table$total), " / ",
      sprintf("%.4f", group_size / table$total), " = ",
      sprintf("%.4f", answer), "\n"
    )
  )
}

# Difficulty 3: one value of the table is missing, and the question cannot be
# answered without working it out first.
probability_question_missing <- function(difficulty) {
  table <- probability_table()
  labels <- pick_one(probability_labels)
  i <- pick_one(1:2)
  j <- pick_one(1:2)

  cell_name <- paste0("f", i, j)
  count <- table[[cell_name]]
  group_size <- table[[paste0("col", j)]]
  row_total <- table[[paste0("row", i)]]
  other_in_row <- table[[paste0("f", i, 3 - j)]]
  answer <- round_prob(count / group_size)

  list(
    topic = "probability",
    type = "conditional_missing",
    difficulty = difficulty,
    prompt = paste0(
      "A sample of ", table$total, " people was classified in two ways. One of the ",
      "values in the table is missing:\n\n",
      probability_table_lines(table, labels, blank = cell_name),
      "\nWork out the missing value, then calculate the conditional probability P(",
      labels$rows[i], " | ", labels$cols[j], "). Give your answer to four decimal places."
    ),
    answer = answer,
    tolerance = 0.002,
    hint = paste0(
      "Fill in the missing cell first: the '", labels$rows[i],
      "' row has to add up to its total. Then divide that cell by the '",
      labels$cols[j], "' column total."
    ),
    solution = paste0(
      "The '", labels$rows[i], "' row adds up to ", row_total,
      ", and the other cell in that row is ", other_in_row, ":\n\n",
      "  missing value = ", row_total, " - ", other_in_row, " = ", count, "\n\n",
      "Now the conditional probability:\n\n",
      "  P(", labels$rows[i], " | ", labels$cols[j], ") = ", count, " / ",
      group_size, " = ", sprintf("%.4f", answer), "\n"
    )
  )
}

# One probability question.
#   difficulty 1: a joint or marginal probability from a complete table
#   difficulty 2: a conditional probability
#   difficulty 3: a conditional probability from a table with a value missing
generate_probability_question <- function(difficulty = 1) {
  difficulty <- clamp(difficulty, 1, 3, 1)
  if (difficulty == 1) return(probability_question_simple(difficulty))
  if (difficulty == 2) return(probability_question_conditional(difficulty))
  probability_question_missing(difficulty)
}
