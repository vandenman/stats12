practice_descriptive_question <- function(difficulty) {
  n <- if (difficulty == 3) 6L else 5L
  scores <- sample(5:20, n)
  average <- mean(scores)
  if (difficulty == 1) {
    return(practice_number_question("descriptives", "sample_mean", difficulty,
      sprintf("The observed scores are %s. What is their mean?", toString(scores)),
      average, "Add up the scores and divide by the number of observations.",
      sprintf("Mean = (%s) / %s = %s / %s", paste(scores, collapse = " + "), n, sum(scores), n)))
  }

  squared_deviations <- (scores - average)^2
  practice_number_question("descriptives", "sample_sd", difficulty,
    sprintf("The observed scores are %s. What is their sample standard deviation?", toString(scores)),
    sd(scores), "Find the squared deviations from the mean. Use n - 1 in the denominator.",
    sprintf("Mean = %s / %s = %s\nSum of squared deviations = %s\nSample SD = sqrt(%s / (%s - 1))",
            sum(scores), n, fmt_num(average, 10), fmt_num(sum(squared_deviations), 10),
            fmt_num(sum(squared_deviations), 10), n))
}
