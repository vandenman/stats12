# ---------------------------------------------------------------------------
# The coordinating layer.
#
# generate_question() picks a topic generator; generate_practice_set() builds
# a whole set of questions from a seed.
# ---------------------------------------------------------------------------

# One question, on one topic, at one difficulty.
generate_question <- function(topic, difficulty, type = NA_character_) {
  if (topic == "probability") return(generate_probability_question(difficulty))
  if (topic == "Z")           return(generate_z_test_question(difficulty))
  if (topic == "t")           return(generate_t_test_question(difficulty))
  if (topic == "normal")      return(generate_normal_question(difficulty))
  if (topic == "clt")         return(generate_clt_question(difficulty))
  if (topic == "bayes")       return(generate_bayes_question(difficulty))
  if (topic == "descriptives") return(practice_descriptive_question(difficulty))
  if (topic == "assumptions") return(practice_assumption_question(difficulty, type))
  if (topic %in% exam2_topics) return(generate_exam2_question(topic, difficulty, type))
  stop("Unknown topic: ", topic)
}

# A whole practice set: a list of questions, in the order they are shown.
# The same seed with the same number of questions, difficulty and topics
# always gives exactly the same set.
generate_practice_set <- function(seed, n_questions = 10, difficulty = 1,
                                  topics = practice_topics(exam), exam = "exam1") {
  seed <- clean_seed(seed)
  n_questions <- clamp(n_questions, 1, 50, 10)
  difficulty <- clamp(difficulty, 1, 3, 1)
  topics <- clean_topics(topics, practice_topics(exam, selectable = TRUE))

  set.seed(seed)

  # The original ordering and RNG calls keep existing Exam 1 seeds stable.
  plan <- rep(topics, length.out = n_questions)
  if (exam == "resit") {
    banks <- Filter(length, list(intersect(topics, known_topics), setdiff(topics, known_topics)))
    bank_plan <- rep(sample.int(length(banks)), length.out = n_questions)
    plan <- unlist(lapply(seq_along(banks), function(i) {
      bank <- banks[[i]]
      rep(bank[sample.int(length(bank))], length.out = sum(bank_plan == i))
    }), use.names = FALSE)
  }
  plan <- plan[sample.int(length(plan))]

  types <- rep(NA_character_, length(plan))
  for (topic in intersect(plan, exam2_topics)) {
    positions <- which(plan == topic)
    templates <- exam2_templates[[topic]][[difficulty]]
    # Offer every selected topic's procedures before repeating any of them.
    types[positions] <- rep(sample(templates), length.out = length(positions))
  }
  assumption_positions <- which(plan == "assumptions")
  if (length(assumption_positions)) {
    types[assumption_positions] <- rep(sample(assumption_templates[[difficulty]]),
                                       length.out = length(assumption_positions))
  }
  lapply(seq_along(plan), function(i) generate_question(plan[i], difficulty, types[i]))
}
