# Synthetic CBS-style dataset for the 2025 "Sociale samenhang en welzijn" example
#
# Purpose
# -------
# Simulate 7,551 synthetic respondents so that the aggregate results reproduce
# the three figures in the CBS news article "9 op de 10 volwassenen vinden het
# leven de moeite waard" (29 July 2026) very closely.
#
# IMPORTANT: These are synthetic teaching data. They are NOT CBS microdata.
#
# The script:
#   1. simulates respondent characteristics;
#   2. calibrates the two main outcomes to the published CBS percentages;
#   3. creates five additional dimensions with the exact published margins;
#   4. writes a CSV file;
#   5. reproduces the three figures from the news article;
#   6. prints a validation table comparing simulated and published percentages.

# -----------------------------------------------------------------------------
# Packages
# -----------------------------------------------------------------------------

library(ggplot2)

# -----------------------------------------------------------------------------
# Settings and CBS targets
# -----------------------------------------------------------------------------

set.seed(20260820)

n <- 7551L

age_levels <- c(
  "18 - 25",
  "25 - 35",
  "35 - 45",
  "45 - 55",
  "55 - 65",
  "65 - 75",
  "above 75"
)

# Plausible adult age distribution chosen so that the published age-specific
# contribution percentages also average to approximately the published 69.8%.
# These age shares themselves are NOT taken from CBS SSW microdata.
age_prob <- c(0.11, 0.16, 0.15, 0.16, 0.16, 0.14, 0.12)

# Figure 1: overall percentages
fig1_targets <- c(
  worthwhile = 0.898,
  contributes = 0.698,
  social_contacts_important = 0.893,
  personal_development_important = 0.824,
  autonomy = 0.793,
  useful = 0.778,
  hopeful = 0.682
)

# Figure 2: feeling that one contributes, by age
contribution_age_targets <- c(
  "18 - 25" = 0.588,
  "25 - 35" = 0.701,
  "35 - 45" = 0.751,
  "45 - 55" = 0.760,
  "55 - 65" = 0.718,
  "65 - 75" = 0.706,
  "above 75" = 0.603
)

# Figure 3: meaning in life by societal participation
contribution_volunteer_targets <- c(`FALSE` = 0.617, `TRUE` = 0.788)
contribution_contact_targets   <- c(`FALSE` = 0.561, `TRUE` = 0.705)

worthwhile_volunteer_targets <- c(`FALSE` = 0.867, `TRUE` = 0.933)
worthwhile_contact_targets   <- c(`FALSE` = 0.728, `TRUE` = 0.907)

# Original five-category response distributions from the longer CBS article.
# These are used only to create realistic raw response variables while
# preserving the calibrated binary outcomes.
worthwhile_likert_positive <- c(
  "Strongly Agree" = 43.0,
  "Agree" = 46.8
)
worthwhile_likert_negative <- c(
  "Undecided" = 8.2,
  "Disagree" = 1.6,
  "Strongly Disagree" = 0.5
)

contribution_likert_positive <- c(
  "Strongly Agree" = 20.5,
  "Agree" = 49.3
)
contribution_likert_negative <- c(
  "Undecided" = 22.6,
  "Disagree" = 6.5,
  "Strongly Disagree" = 1.1
)

# The article does not publish the five-category margins for the other five
# dimensions. Use a common, plausible split for those items. The calibrated
# binary indicator remains the sum of "Agree" and "Strongly Agree".
other_likert_positive <- c(
  "Strongly Agree" = 45,
  "Agree" = 55
)
other_likert_negative <- c(
  "Undecided" = 70,
  "Disagree" = 25,
  "Strongly Disagree" = 5
)

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

logit <- function(p) log(p / (1 - p))
inv_logit <- function(x) 1 / (1 + exp(-x))

# Shift a vector of baseline probabilities so that its mean equals target.
shift_probabilities_to_mean <- function(p, target) {
  f <- function(delta) mean(inv_logit(logit(p) + delta)) - target
  delta <- uniroot(f, c(-10, 10))$root
  inv_logit(logit(p) + delta)
}

# Weighted sampling without replacement, returning exactly k TRUE values.
weighted_binary_exact <- function(prob, k) {
  out <- rep(FALSE, length(prob))
  chosen <- sample.int(length(prob), size = k, replace = FALSE, prob = prob)
  out[chosen] <- TRUE
  out
}

# Create a numeric age within each age category.
simulate_age <- function(age_group) {
  ranges <- list(
    "18 - 25" = 18:24,
    "25 - 35" = 25:34,
    "35 - 45" = 35:44,
    "45 - 55" = 45:54,
    "55 - 65" = 55:64,
    "65 - 75" = 65:74,
    "above 75" = 75:90
  )

  vapply(
    as.character(age_group),
    function(g) sample(ranges[[g]], 1L),
    integer(1)
  )
}

# Build one row per age x volunteering x weekly-contact cell.
make_cells <- function(dat) {
  key <- interaction(
    dat$age_group,
    dat$volunteer,
    dat$weekly_social_contact,
    drop = TRUE,
    lex.order = TRUE
  )

  dat$cell_id <- as.integer(key)
  n_cells <- nlevels(key)

  first <- match(seq_len(n_cells), dat$cell_id)

  cells <- data.frame(
    cell_id = seq_len(n_cells),
    age_group = dat$age_group[first],
    volunteer = dat$volunteer[first],
    weekly_social_contact = dat$weekly_social_contact[first],
    n = tabulate(dat$cell_id, nbins = n_cells)
  )

  list(data = dat, cells = cells)
}

# Construct a linear constraint row that computes a group mean from cell
# probabilities.
constraint_row <- function(cells, mask) {
  w <- numeric(nrow(cells))
  denom <- sum(cells$n[mask])
  w[mask] <- cells$n[mask] / denom
  w
}

# Find cell probabilities that reproduce a set of aggregate targets while
# staying close to a sensible initial solution. This is a ridge-regularized
# least-squares calibration problem.
calibrate_cell_probabilities <- function(
    cells,
    overall_target,
    age_targets = NULL,
    volunteer_targets = NULL,
    contact_targets = NULL,
    initial,
    lambda = 1e-4) {

  A <- matrix(cells$n / sum(cells$n), nrow = 1L)
  b <- overall_target
  labels <- "Overall"

  if (!is.null(age_targets)) {
    for (g in names(age_targets)) {
      mask <- as.character(cells$age_group) == g
      A <- rbind(A, constraint_row(cells, mask))
      b <- c(b, age_targets[[g]])
      labels <- c(labels, paste("Age:", g))
    }
  }

  if (!is.null(volunteer_targets)) {
    for (g in names(volunteer_targets)) {
      value <- identical(g, "TRUE")
      mask <- cells$volunteer == value
      A <- rbind(A, constraint_row(cells, mask))
      b <- c(b, volunteer_targets[[g]])
      labels <- c(labels, paste("Volunteer:", g))
    }
  }

  if (!is.null(contact_targets)) {
    for (g in names(contact_targets)) {
      value <- identical(g, "TRUE")
      mask <- cells$weekly_social_contact == value
      A <- rbind(A, constraint_row(cells, mask))
      b <- c(b, contact_targets[[g]])
      labels <- c(labels, paste("Weekly contact:", g))
    }
  }

  m <- nrow(cells)
  lhs <- crossprod(A) + lambda * diag(m)
  rhs <- crossprod(A, b) + lambda * initial

  q <- as.vector(solve(lhs, rhs))

  if (any(q <= 0 | q >= 1)) {
    stop(
      "Calibration produced probabilities outside (0,1). ",
      "Try changing the simulated covariate prevalences."
    )
  }

  attr(q, "constraint_matrix") <- A
  attr(q, "targets") <- b
  attr(q, "labels") <- labels
  q
}

# Turn calibrated cell probabilities into binary respondent-level indicators.
# The exact number of TRUE observations in every cell is round(q * n_cell).
# The shared latent score makes the two main outcomes positively correlated.
assign_binary_by_cell <- function(dat, cells, q, score) {
  out <- rep(FALSE, nrow(dat))

  for (j in seq_len(nrow(cells))) {
    idx <- which(dat$cell_id == cells$cell_id[j])
    k <- round(q[j] * length(idx))

    if (k > 0L) {
      selected <- idx[order(score[idx], decreasing = TRUE)[seq_len(k)]]
      out[selected] <- TRUE
    }
  }

  out
}

# Create a binary variable with an exact marginal prevalence while retaining
# correlation with a supplied latent score.
make_binary_margin <- function(target, score) {
  k <- round(length(score) * target)
  out <- rep(FALSE, length(score))
  out[order(score, decreasing = TRUE)[seq_len(k)]] <- TRUE
  out
}

# Convert a calibrated binary variable back to a plausible five-category
# Likert response. CBS's published category percentages sum to 100.1% because
# of rounding, so category ratios are normalized within the positive and
# non-positive halves.
make_likert_from_binary <- function(binary, score, positive_pct, negative_pct) {
  result <- character(length(binary))

  pos_idx <- which(binary)
  neg_idx <- which(!binary)

  pos_prob <- positive_pct / sum(positive_pct)
  neg_prob <- negative_pct / sum(negative_pct)

  # Positive responses: higher latent score -> stronger agreement.
  n_strong <- round(length(pos_idx) * pos_prob[1])
  pos_order <- pos_idx[order(score[pos_idx], decreasing = TRUE)]
  result[pos_order[seq_len(n_strong)]] <- names(pos_prob)[1]
  if (n_strong < length(pos_order)) {
    result[pos_order[(n_strong + 1L):length(pos_order)]] <- names(pos_prob)[2]
  }

  # Non-positive responses: lower latent score -> stronger disagreement.
  # Allocate approximately according to the published ratios.
  neg_order <- neg_idx[order(score[neg_idx], decreasing = TRUE)]
  n_neutral <- round(length(neg_idx) * neg_prob[1])
  n_disagree <- round(length(neg_idx) * neg_prob[2])
  n_neutral <- min(n_neutral, length(neg_idx))
  n_disagree <- min(n_disagree, length(neg_idx) - n_neutral)

  if (n_neutral > 0L) {
    result[neg_order[seq_len(n_neutral)]] <- names(neg_prob)[1]
  }
  if (n_disagree > 0L) {
    ii <- (n_neutral + 1L):(n_neutral + n_disagree)
    result[neg_order[ii]] <- names(neg_prob)[2]
  }
  if (n_neutral + n_disagree < length(neg_order)) {
    ii <- (n_neutral + n_disagree + 1L):length(neg_order)
    result[neg_order[ii]] <- names(neg_prob)[3]
  }

  factor(
    result,
    levels = c(
      "Strongly Disagree",
      "Disagree",
      "Undecided",
      "Agree",
      "Strongly Agree"
    ),
    ordered = TRUE
  )
}

pct <- function(x) 100 * mean(x)

# -----------------------------------------------------------------------------
# 1. Simulate respondent characteristics
# -----------------------------------------------------------------------------

age_group <- factor(
  sample(age_levels, n, replace = TRUE, prob = age_prob),
  levels = age_levels,
  ordered = TRUE
)

age <- simulate_age(age_group)

# We choose prevalences around 47% volunteering and 95% weekly contact because
# these make the published conditional percentages in Figure 3 consistent with
# the published overall percentages in Figure 1.
#
# The age patterns below are plausible synthetic patterns, not CBS estimates.
volunteer_base <- c(
  "18 - 25" = 0.34,
  "25 - 35" = 0.40,
  "35 - 45" = 0.47,
  "45 - 55" = 0.52,
  "55 - 65" = 0.55,
  "65 - 75" = 0.54,
  "above 75" = 0.45
)

contact_base <- c(
  "18 - 25" = 0.965,
  "25 - 35" = 0.960,
  "35 - 45" = 0.955,
  "45 - 55" = 0.950,
  "55 - 65" = 0.945,
  "65 - 75" = 0.940,
  "above 75" = 0.925
)

p_volunteer <- volunteer_base[as.character(age_group)]
p_volunteer <- shift_probabilities_to_mean(p_volunteer, 0.472)

p_contact <- contact_base[as.character(age_group)]
p_contact <- shift_probabilities_to_mean(p_contact, 0.950)

volunteer <- weighted_binary_exact(p_volunteer, round(n * 0.472))
weekly_social_contact <- weighted_binary_exact(p_contact, round(n * 0.950))

synthetic <- data.frame(
  id = seq_len(n),
  age = age,
  age_group = age_group,
  volunteer = volunteer,
  weekly_social_contact = weekly_social_contact
)

cell_data <- make_cells(synthetic)
synthetic <- cell_data$data
cells <- cell_data$cells

# Shared latent "meaning/wellbeing" tendency. It has no substantive status;
# it is simply a convenient way to make the synthetic responses correlated.
latent_meaning <- rnorm(n)

# -----------------------------------------------------------------------------
# 2. Calibrate "feeling that one contributes"
# -----------------------------------------------------------------------------

initial_contribution <- unname(
  contribution_age_targets[as.character(cells$age_group)]
)

q_contribution <- calibrate_cell_probabilities(
  cells = cells,
  overall_target = fig1_targets["contributes"],
  age_targets = contribution_age_targets,
  volunteer_targets = contribution_volunteer_targets,
  contact_targets = contribution_contact_targets,
  initial = initial_contribution
)

contribution_score <- latent_meaning + rnorm(n, sd = 0.75)
synthetic$contributes <- assign_binary_by_cell(
  synthetic,
  cells,
  q_contribution,
  contribution_score
)

# -----------------------------------------------------------------------------
# 3. Calibrate "life is worthwhile"
# -----------------------------------------------------------------------------

initial_worthwhile <- rep(fig1_targets["worthwhile"], nrow(cells))

q_worthwhile <- calibrate_cell_probabilities(
  cells = cells,
  overall_target = fig1_targets["worthwhile"],
  volunteer_targets = worthwhile_volunteer_targets,
  contact_targets = worthwhile_contact_targets,
  initial = initial_worthwhile
)

worthwhile_score <- latent_meaning + rnorm(n, sd = 0.75)
synthetic$worthwhile <- assign_binary_by_cell(
  synthetic,
  cells,
  q_worthwhile,
  worthwhile_score
)

# -----------------------------------------------------------------------------
# 4. Create the five other Figure-1 dimensions
# -----------------------------------------------------------------------------

# Each variable gets the exact marginal count corresponding to the published
# percentage (up to unavoidable integer rounding), while retaining realistic
# positive correlations through latent_meaning.
social_contacts_score <- 0.70 * latent_meaning + rnorm(n)
synthetic$social_contacts_important <- make_binary_margin(
  fig1_targets["social_contacts_important"],
  social_contacts_score
)

personal_development_score <- 0.55 * latent_meaning + rnorm(n)
synthetic$personal_development_important <- make_binary_margin(
  fig1_targets["personal_development_important"],
  personal_development_score
)

autonomy_score <- 0.90 * latent_meaning + rnorm(n, sd = 0.8)
synthetic$autonomy <- make_binary_margin(
  fig1_targets["autonomy"],
  autonomy_score
)

useful_score <- 1.00 * latent_meaning +
  0.60 * synthetic$contributes +
  rnorm(n, sd = 0.8)
synthetic$useful <- make_binary_margin(
  fig1_targets["useful"],
  useful_score
)

hopeful_score <- 0.85 * latent_meaning + rnorm(n, sd = 0.9)
synthetic$hopeful <- make_binary_margin(
  fig1_targets["hopeful"],
  hopeful_score
)

# -----------------------------------------------------------------------------
# 5. Create five-category Likert responses
# -----------------------------------------------------------------------------

# Keep the binary indicators above because the CBS figures report the
# percentage giving a positive answer. The *_response variables are the raw
# ordered responses shown to students.

synthetic$worthwhile_response <- make_likert_from_binary(
  synthetic$worthwhile,
  worthwhile_score,
  worthwhile_likert_positive,
  worthwhile_likert_negative
)

synthetic$contribution_response <- make_likert_from_binary(
  synthetic$contributes,
  contribution_score,
  contribution_likert_positive,
  contribution_likert_negative
)

synthetic$social_contacts_important_response <- make_likert_from_binary(
  synthetic$social_contacts_important,
  social_contacts_score,
  other_likert_positive,
  other_likert_negative
)

synthetic$personal_development_important_response <- make_likert_from_binary(
  synthetic$personal_development_important,
  personal_development_score,
  other_likert_positive,
  other_likert_negative
)

synthetic$autonomy_response <- make_likert_from_binary(
  synthetic$autonomy,
  autonomy_score,
  other_likert_positive,
  other_likert_negative
)

synthetic$useful_response <- make_likert_from_binary(
  synthetic$useful,
  useful_score,
  other_likert_positive,
  other_likert_negative
)

synthetic$hopeful_response <- make_likert_from_binary(
  synthetic$hopeful,
  hopeful_score,
  other_likert_positive,
  other_likert_negative
)

# Put each raw response immediately before its dichotomized version.
synthetic <- synthetic[c(
  "id",
  "age",
  "age_group",
  "volunteer",
  "weekly_social_contact",
  "worthwhile_response",
  "worthwhile",
  "contribution_response",
  "contributes",
  "social_contacts_important_response",
  "social_contacts_important",
  "personal_development_important_response",
  "personal_development_important",
  "autonomy_response",
  "autonomy",
  "useful_response",
  "useful",
  "hopeful_response",
  "hopeful",
  "cell_id"
)]

# cell_id is only an internal simulation variable and is not useful to students.
synthetic$cell_id <- NULL

# -----------------------------------------------------------------------------
# 6. Save the synthetic dataset
# -----------------------------------------------------------------------------

write.csv(
  synthetic,
  file = "lecture1_scripts/cbs_zingeving_synthetic_2025.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# -----------------------------------------------------------------------------
# 7. Validation: compare synthetic percentages with CBS targets
# -----------------------------------------------------------------------------

validation <- data.frame(
  figure = character(),
  quantity = character(),
  cbs = numeric(),
  synthetic = numeric(),
  difference = numeric()
)

add_validation <- function(figure, quantity, target, estimate) {
  data.frame(
    figure = figure,
    quantity = quantity,
    cbs = 100 * target,
    synthetic = estimate,
    difference = estimate - 100 * target
  )
}

# Figure 1
fig1_names <- c(
  worthwhile = "Vindt het leven de moeite waard",
  contributes = "Heeft het gevoel iets bij te dragen",
  social_contacts_important = "Hecht (veel) belang aan sociale contacten",
  personal_development_important = "Hecht (veel) belang aan persoonlijke ontwikkeling",
  autonomy = "Kan de dingen doen die men wil en belangrijk vindt",
  useful = "Heeft het gevoel nuttig te zijn",
  hopeful = "Is (heel) hoopvol over de toekomst"
)

for (v in names(fig1_targets)) {
  validation <- rbind(
    validation,
    add_validation("Figure 1", fig1_names[[v]], fig1_targets[[v]], pct(synthetic[[v]]))
  )
}

# Figure 2
for (g in age_levels) {
  idx <- synthetic$age_group == g
  validation <- rbind(
    validation,
    add_validation(
      "Figure 2",
      g,
      contribution_age_targets[[g]],
      pct(synthetic$contributes[idx])
    )
  )
}

# Figure 3
for (value in c(FALSE, TRUE)) {
  idx <- synthetic$volunteer == value
  validation <- rbind(
    validation,
    add_validation(
      "Figure 3",
      paste("Worthwhile | volunteer =", value),
      worthwhile_volunteer_targets[[as.character(value)]],
      pct(synthetic$worthwhile[idx])
    ),
    add_validation(
      "Figure 3",
      paste("Contributes | volunteer =", value),
      contribution_volunteer_targets[[as.character(value)]],
      pct(synthetic$contributes[idx])
    )
  )

  idx <- synthetic$weekly_social_contact == value
  validation <- rbind(
    validation,
    add_validation(
      "Figure 3",
      paste("Worthwhile | weekly contact =", value),
      worthwhile_contact_targets[[as.character(value)]],
      pct(synthetic$worthwhile[idx])
    ),
    add_validation(
      "Figure 3",
      paste("Contributes | weekly contact =", value),
      contribution_contact_targets[[as.character(value)]],
      pct(synthetic$contributes[idx])
    )
  )
}

validation$cbs <- round(validation$cbs, 1)
validation$synthetic <- round(validation$synthetic, 1)
validation$difference <- round(validation$difference, 2)

cat("\nValidation against published CBS percentages:\n\n")
print(validation, row.names = FALSE)

cat("\nMaximum absolute discrepancy (percentage points): ",
    max(abs(validation$difference)), "\n", sep = "")

# -----------------------------------------------------------------------------
# 8. Plot 1: Zingeving, 2025
# -----------------------------------------------------------------------------

plot1_data <- data.frame(
  label = c(
    "Vindt het leven\nde moeite waard",
    "Heeft het gevoel iets\nbij te dragen",
    "Hecht (veel) belang aan\nsociale contacten",
    "Hecht (veel) belang aan\npersoonlijke ontwikkeling",
    "Kan de dingen doen die men wil\nen belangrijk vindt in het leven",
    "Heeft het gevoel\nnuttig te zijn",
    "Is (heel) hoopvol\nover de toekomst"
  ),
  group = c(
    "Hoofdaspecten",
    "Hoofdaspecten",
    rep("Deelaspecten", 5)
  ),
  percentage = c(
    pct(synthetic$worthwhile),
    pct(synthetic$contributes),
    pct(synthetic$social_contacts_important),
    pct(synthetic$personal_development_important),
    pct(synthetic$autonomy),
    pct(synthetic$useful),
    pct(synthetic$hopeful)
  )
)

plot1_data$label <- factor(plot1_data$label, levels = rev(plot1_data$label))
plot1_data$group <- factor(plot1_data$group, levels = c("Hoofdaspecten", "Deelaspecten"))

p1 <- ggplot(plot1_data, aes(x = percentage, y = label)) +
  geom_col(width = 0.68) +
  geom_text(
    aes(label = sprintf("%.1f", percentage)),
    hjust = -0.12,
    size = 3.7
  ) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0.06))
  ) +
  labs(
    title = "Zingeving, 2025",
    subtitle = "% van mensen van 18 jaar of ouder — synthetische data",
    x = "%",
    y = NULL,
    caption = "Synthetic teaching data calibrated to published CBS aggregates"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, face = "bold"),
    plot.title = element_text(face = "bold")
  )

print(p1)
ggsave("lecture1_scripts/cbs_plot1_zingeving_2025.png", p1, width = 9, height = 6.4, dpi = 180)

# -----------------------------------------------------------------------------
# 9. Plot 2: feeling that one contributes, by age
# -----------------------------------------------------------------------------

plot2_data <- data.frame(
  age_group = factor(age_levels, levels = age_levels, ordered = TRUE),
  percentage = vapply(
    age_levels,
    function(g) pct(synthetic$contributes[synthetic$age_group == g]),
    numeric(1)
  )
)

p2 <- ggplot(plot2_data, aes(x = age_group, y = percentage)) +
  geom_col(width = 0.68) +
  geom_text(
    aes(label = sprintf("%.1f", percentage)),
    vjust = -0.45,
    size = 3.7
  ) +
  scale_y_continuous(
    limits = c(0, 85),
    breaks = seq(0, 80, 20),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    title = "Het gevoel hebben iets bij te dragen, 2025",
    subtitle = "% — synthetische data",
    x = NULL,
    y = "%",
    caption = "Synthetic teaching data calibrated to published CBS aggregates"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(face = "bold")
  )

print(p2)
ggsave("lecture1_scripts/cbs_plot2_contribution_by_age_2025.png", p2, width = 9, height = 5.7, dpi = 180)

# -----------------------------------------------------------------------------
# 10. Plot 3: meaning and societal participation
# -----------------------------------------------------------------------------

make_participation_rows <- function(variable, label) {
  data.frame(
    participation = rep(label, 4),
    answer = rep(c("Ja", "Nee"), each = 2),
    outcome = rep(
      c("Het leven de moeite waard vinden", "Het gevoel hebben iets bij te dragen"),
      times = 2
    ),
    percentage = c(
      pct(synthetic$worthwhile[variable]),
      pct(synthetic$contributes[variable]),
      pct(synthetic$worthwhile[!variable]),
      pct(synthetic$contributes[!variable])
    )
  )
}

plot3_data <- rbind(
  make_participation_rows(
    synthetic$volunteer,
    "Afgelopen twaalf maanden\nvrijwilligerswerk gedaan"
  ),
  make_participation_rows(
    synthetic$weekly_social_contact,
    "Wekelijks contact met familie,\nvrienden of buren"
  )
)

plot3_data$participation <- factor(
  plot3_data$participation,
  levels = c(
    "Afgelopen twaalf maanden\nvrijwilligerswerk gedaan",
    "Wekelijks contact met familie,\nvrienden of buren"
  )
)
plot3_data$answer <- factor(plot3_data$answer, levels = c("Ja", "Nee"))

p3 <- ggplot(
  plot3_data,
  aes(x = participation, y = percentage, fill = answer)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  geom_text(
    aes(label = sprintf("%.1f", percentage)),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 3.4
  ) +
  facet_wrap(~ outcome, ncol = 1) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(
    title = "Zingeving, maatschappelijke deelname, 2025",
    subtitle = "% van mensen van 18 jaar of ouder — synthetische data",
    x = NULL,
    y = "%",
    fill = "Wel of niet",
    caption = "Synthetic teaching data calibrated to published CBS aggregates"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

print(p3)
ggsave("lecture1_scripts/cbs_plot3_participation_2025.png", p3, width = 9, height = 8, dpi = 180)

ggplot(synthetic, aes(x = useful, y = contributes)) +
  geom_point() +
  theme_bw(base_size = 20)

# -----------------------------------------------------------------------------
# 11. A few useful checks for teaching
# -----------------------------------------------------------------------------

cat("\nDataset written to: lecture1_scripts/cbs_zingeving_synthetic_2025.csv\n")
cat("Plots written to:\n")
cat("  lecture1_scripts/cbs_plot1_zingeving_2025.png\n")
cat("  lecture1_scripts/cbs_plot2_contribution_by_age_2025.png\n")
cat("  lecture1_scripts/cbs_plot3_participation_2025.png\n\n")

cat("First six rows:\n")
print(head(synthetic))

cat("\nCorrelation matrix of the seven binary meaning variables:\n")
meaning_vars <- c(
  "worthwhile",
  "contributes",
  "social_contacts_important",
  "personal_development_important",
  "autonomy",
  "useful",
  "hopeful"
)
print(round(cor(synthetic[meaning_vars]), 2))


set.seed(42)
n <- 250
rhos <-  c(-.5, 0, .15, .5)
dff <- data.frame()
for (i in 1:4) {
  rho <- rhos[i]
  sigma <- matrix(c(1, rho, rho, 1), 2, 2)
  xy <- MASS::mvrnorm(n, c(0, 0), sigma)
  df <- data.frame(xy)
  colnames(df) <- c("x", "y")
  df$r <- rho
  dff <- rbind(dff, df)
}

x <- rnorm(n)
y <- -.5 * x^2 + .35 * pmin(2, abs(x)) * rnorm(n) + 2
df <- data.frame(x, y)
colnames(df) <- c("x", "y")
df$r <- "quadratic"
dff <- rbind(dff, df)
#
# x <- 2*rnorm(n)
# radius <- max(x) + 1
# y <- (2*(runif(n) < .5) - 1) * sqrt(radius^2 - x^2) + rnorm(n)
# df <- data.frame(x, y)
# colnames(df) <- c("x", "y")
# df$r <- "circle"
# dff <- rbind(dff, df)

n <- 1000

# 1. Generate random angles uniformly around the circle (0 to 2*pi)
theta <- runif(n, min = 0, max = 2 * pi)

# 2. Set radius to exactly 1
radius <- 3 + rnorm(n)  * .35

# 3. Calculate x and y using trigonometry
x <- radius * cos(theta)
y <- radius * sin(theta)

# 4. Create the data frame
df <- data.frame(x = x, y = y, r = "circle")
dff <- rbind(dff, df)

# Verify by plotting (asp = 1 ensures the circle isn't distorted)
# plot(df$x, df$y, asp = 1, main = "Uniformly Distributed Circle (Radius = 1)")


g0 <- ggplot(dff, aes(x = x, y = y)) +
  geom_point(alpha = .7, shape = 21, fill = "grey") +
  facet_wrap(~r) +
  theme_bw(base_size = 20) +
  theme(strip.text = element_blank())

g1 <- ggplot(dff, aes(x = x, y = y)) +
  geom_point(alpha = .7, shape = 21, fill = "grey") +
  facet_wrap(~r) +
  theme_bw(base_size = 20)

ggsave(filename = "lecture1_images/cor_without_strip.jpg", g0)
ggsave(filename = "lecture1_images/cor_with_strip.jpg", g1)
