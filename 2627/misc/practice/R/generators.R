# ---------------------------------------------------------------------------
# The coordinating layer.
#
# generate_question() picks a topic generator; generate_practice_set() builds
# a whole set of questions from a seed.  This is the only file that needs to
# change when a fourth topic is added (plus its own R file, and the list of
# topics in utils.R).
# ---------------------------------------------------------------------------

# One question, on one topic, at one difficulty.
generate_question <- function(topic, difficulty) {
  if (topic == "probability") return(generate_probability_question(difficulty))
  if (topic == "Z")           return(generate_z_test_question(difficulty))
  if (topic == "t")           return(generate_t_test_question(difficulty))
  if (topic == "normal")      return(generate_normal_question(difficulty))
  if (topic == "clt")         return(generate_clt_question(difficulty))
  if (topic == "bayes")       return(generate_bayes_question(difficulty))
  stop("Unknown topic: ", topic)
}

# A whole practice set: a list of questions, in the order they are shown.
# The same seed with the same number of questions, difficulty and topics
# always gives exactly the same set.
generate_practice_set <- function(seed, n_questions = 10, difficulty = 1,
                                  topics = known_topics) {
  seed <- clean_seed(seed)
  n_questions <- clamp(n_questions, 1, 50, 10)
  difficulty <- clamp(difficulty, 1, 3, 1)
  topics <- clean_topics(topics)

  set.seed(seed)

  # Spread the selected topics over the questions, then shuffle the order, so
  # that the topics are mixed instead of grouped together.
  plan <- rep(topics, length.out = n_questions)
  plan <- plan[sample.int(length(plan))]

  lapply(plan, function(topic) generate_question(topic, difficulty))
}
