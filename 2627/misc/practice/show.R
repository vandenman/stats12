# ---------------------------------------------------------------------------
# See what a practice set looks like, without opening a browser.
#
# Run it from this folder, with nothing but base R (no renv needed):
#
#   cd 2627/misc/practice
#   Rscript --vanilla show.R                        # all three difficulties, seed 1234
#   Rscript --vanilla show.R 18492 2 5 normal,clt   # seed 18492, difficulty 2, 5 questions
#
# The arguments are seed, difficulty (1-3), number of questions and topics (a
# comma-separated list of probability, Z, t, normal, clt, bayes).  Anything you
# leave out keeps its default, and no difficulty means "one set per
# difficulty", which is a quick check that all three levels still generate.
#
# What is printed here is exactly what the browser generates from the same
# seed, difficulty, number and topics.
# ---------------------------------------------------------------------------

if (!file.exists("R/generators.R")) {
  stop("Run this from the practice folder: cd <...>/practice && Rscript --vanilla show.R")
}

for (file in c("R/utils.R", "R/probability.R", "R/normal.R", "R/clt.R",
               "R/bayes.R", "R/ztests.R", "R/ttests.R", "R/generators.R")) {
  source(file)
}

args <- commandArgs(trailingOnly = TRUE)
seed <- if (length(args) >= 1) as.numeric(args[1]) else 1234
difficulty <- if (length(args) >= 2) as.integer(args[2]) else NA
n_questions <- if (length(args) >= 3) as.integer(args[3]) else 5
topics <- if (length(args) >= 4) strsplit(args[4], ",")[[1]] else known_topics

difficulties <- if (is.na(difficulty)) 1:3 else difficulty

show_question <- function(question, number) {
  cat("\n--- Question ", number, " (", topic_label(question$topic), ", ",
      question$type, ") ---\n", sep = "")
  cat(question$prompt, "\n")
  cat("answer: ", fmt_num(question$answer, 12),
      "   (tolerance ", fmt_num(question$tolerance, 12), ")\n", sep = "")
  cat("\nhint:\n", question$hint, "\n", sep = "")
  cat("\nsolution:\n", question$solution, "\n", sep = "")
}

for (level in difficulties) {
  cat("\n=================================================================\n")
  cat("seed ", seed, ", difficulty ", level, ", ", n_questions,
      " questions, topics: ", paste(topics, collapse = ", "), "\n", sep = "")
  cat("=================================================================\n")

  questions <- generate_practice_set(seed, n_questions, level, topics)
  for (i in seq_along(questions)) {
    show_question(questions[[i]], i)
  }
}

cat("\nDone: ", length(difficulties), " set(s) printed.\n", sep = "")
