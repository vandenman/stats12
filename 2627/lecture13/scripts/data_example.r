library(tidyverse)
library(fs)
library(reshape2)
library(papaja)
library(patchwork)
fragment_kable <- function(obj, steps, digits = 2, ...) {
  stopifnot(length(steps) == ncol(obj))

  html <- knitr::kable(
    obj, format = "html", digits = digits,
    row.names = FALSE, ...
  )
  doc <- xml2::read_html(as.character(html))
  table <- xml2::xml_find_first(doc, "//table")

  for (j in which(!is.na(steps))) {
    cells <- xml2::xml_find_all(
      table,
      sprintf("./thead/tr/th[%d] | ./tbody/tr/td[%d]", j, j)
    )
    xml2::xml_set_attr(cells, "class", "fragment")
    xml2::xml_set_attr(
      cells, "data-fragment-index", as.character(steps[j])
    )
  }

  knitr::asis_output(as.character(table))
}
save_figs <- function(..., width = 25, height = 25, units = "cm", limitsize = TRUE, .base) {
  dont_save <- getOption("dont_save", FALSE)
  if (dont_save) return()

  if (...length() == 1L) {
    ggsave(...elt(1), file = .base,
           width = width, height = height, units = units, limitsize = limitsize)
  } else
    for (i in seq_len(...length()))
      ggsave(...elt(i), file = sprintf(.base, i),
             width = width, height = height, units = units, limitsize = limitsize)
}

out_dir_fig <- path("lecture13", "figures")
out_dir_tb  <- path("lecture13", "tables")
sapply(c(out_dir_fig, out_dir_tb), \(path) { if (!dir_exists(path)) dir_create(path)})

# please install these packages if you haven't done so
# library('ggjoy')
# library('stringr')
# library('latex2exp')
# library('tidyverse')
# library('BayesFactor') # devtools::install_github('richarddmorey/BayesFactor', subdir='pkg/BayesFactor')
theme_set(theme_apa())


# filter age >= 18; one participant reported 1987 as age, presumably his birth year
# so keep the row, but recode

datraw <- read.csv(path("lecture13", "data", "rtf74-osfstorage-archive", "harry_all.csv"), sep = ';') %>%
  mutate(
    age = ifelse(age == 1987, 2016 - 1987, age)
  ) %>%
  filter(age >= 18 & age <= 100)

# remove participants who were assigned multiple houses (34!)
nr_houses <- sapply(str_split(as.character(datraw$Sorting_house), ';'), length)
dat <- datraw[nr_houses == 1, ] %>%
  mutate(
    Sorting_house = as.factor(as.character(Sorting_house))
  )

recode <- function(house, sorted) as.factor(as.numeric(house == sorted))
center_score <- function(score, dat) {
  # mean score across all values for each participant
  mean_score <- rowMeans(dat[, -seq(280-57)])
  value_score <- rowMeans(select(dat, one_of(paste0('PVQ_', score))))

  # centered score (following Schwartz)
  value_score - mean_score
}


SELF_SCORE <- c('01', '23', '39', '16', '30', '56')
STIM_SCORE <- c('10', '28', '43')
HEDON_SCORE <- c('03', '36', '46')
ACHIEVEMENT_SCORE <- c('17', '32', '48')
POWER_SCORE <- c('06', '29', '41', '12', '20', '44')
SECUR_SCORE <- c('13', '26', '53', '02', '35', '50')
CONFOR_SCORE <- c('15', '31', '42', '04', '22', '51')
TRADIT_SCORE <- c('18', '33', '40', '07', '38', '54')
BENEV_SCORE <- c('11', '25', '47', '19', '27', '55')
UNIV_SCORE <- c('08', '21', '45', '05', '37', '52', '14', '34', '57')

dat <- dat %>%
  mutate(
    match = Sorting_house == Sorting_house_wish,
    Gryffindor = recode('Gryffindor', Sorting_house),
    Hufflepuff = recode('Hufflepuff', Sorting_house),
    Ravenclar  = recode('Ravenclaw', Sorting_house),
    Slytherin  = recode('Slytherin', Sorting_house),

    self = center_score(SELF_SCORE, .),
    stim = center_score(STIM_SCORE, .),
    hedon = center_score(HEDON_SCORE, .),
    achievement = center_score(ACHIEVEMENT_SCORE, .),
    power = center_score(POWER_SCORE, .),
    secur = center_score(SECUR_SCORE, .),
    confor = center_score(CONFOR_SCORE, .),
    tradit = center_score(TRADIT_SCORE, .),
    benev = center_score(BENEV_SCORE, .),
    univ = center_score(UNIV_SCORE, .)
  ) %>%
  rename(
    ach = achievement,
    Extraversion = IPIP_Extraversion,
    Conscientiousness = IPIP_Conscientiousness,
    Agreeableness = IPIP_Agreeableness,
    Intellect = IPIP_Intellect,
    EmStability = IPIP_EmStability,
    Machiavellianism = SD3_Machiavellianism,
    Psychopathy = SD3_Psychopathy,
    Narcissism = SD3_Narcissism
  ) %>%
  select(-starts_with('PVQ_'))

### Figure 1
sd3 <- c('Psychopathy', 'Machiavellianism', 'Narcissism')
big5 <- c('Extraversion', 'Agreeableness', 'Intellect', 'EmStability', 'Conscientiousness')

boxplot_dat <- select(dat, Sorting_house, sd3, big5) %>%
  melt %>%
  mutate(
    variable = as.character(variable),
    variable = str_replace(variable, 'Conscientiousness', 'Conscient.'),
    variable = str_replace(variable, 'Agreeableness', 'Agreeabl.'),
    variable = str_replace(variable, 'Machiavellianism', 'Machiav.')
  )

plot_full_df <- function(df, xlab = 'Personality Traits', title = 'Personality Traits across Houses') {
  ggplot(df, aes(x = variable, y = value)) +
    geom_boxplot(aes(fill = Sorting_house), alpha = .75) +
    ylab('Score') +
    xlab(xlab) +
    # ylim(c(0, 50)) +
    ggtitle(title) +
    theme(plot.title = element_text(size = 18, hjust = .5, margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title = element_text(size = 16),
          legend.title = element_blank(),
          axis.text = element_text(size = 10),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #axis.text.x = element_text(angle = 12),
          legend.spacing.x = unit(.1, 'cm'),
          legend.position = 'top') +
    scale_fill_manual(name = 'House', values = c('#7F0909', '#f0c75e', '#08245e', '#2a623d')) +
    ggtitle(title)
}

# pdf('Figures/Figure-1-Boxplot-Personality.pdf', width = 8, height = 5)

boxplot_dat_m <- boxplot_dat |>
  filter(variable == "Machiav.") |>
  mutate(variable = "Machiavellism")

plot_full_df(boxplot_dat, '') + fig0
y_scale <- scale_y_continuous(breaks = seq(10, 50, 10), limits = c(9, 51))
fig0 <- plot_full_df(boxplot_dat, '') + y_scale + theme_bw(base_size = 24)
fig1 <- fig0 +
  geom_rect(aes(xmin = 5.5, xmax = 6.5, ymin = 9, ymax = 47), fill = "transparent",
            color = "firebrick2", linewidth = 3)
# fig2 <- plot_full_df(boxplot_dat_m, xlab = NULL, title = NULL) + theme_bw(base_size = 24)
# fig2


fig2 <- ggplot(boxplot_dat_m, aes(x = Sorting_house, y = value, group = Sorting_house, fill = Sorting_house)) +
  y_scale +
  xlab(NULL) +
  ylab('Machiavellism') +
  scale_color_manual(name = 'House', values = c('#7F0909', '#f0c75e', '#08245e', '#2a623d')) +
  scale_fill_manual(name = 'House', values = c('#7F0909', '#f0c75e', '#08245e', '#2a623d')) +
  ggtitle("Add fake spread to x-coordinates") +
  geom_boxplot(alpha = .75) +
  theme_bw(base_size = 24)

fig2
fig3 <- ggplot(boxplot_dat_m, aes(x = Sorting_house, y = value, group = Sorting_house, color = Sorting_house)) +
  y_scale +
  ggbeeswarm::geom_beeswarm() +
  xlab(NULL) +
  ylab('Machiavellism') +
  scale_color_manual(name = 'House', values = c('#7F0909', '#f0c75e', '#08245e', '#2a623d')) +
  scale_fill_manual(name = 'House', values = c('#7F0909', '#f0c75e', '#08245e', '#2a623d')) +
  ggtitle("Add fake spread to x-coordinates") +
  theme_bw(base_size = 24)
fig3

save_figs(fig0, fig1, fig2, fig3, width = 40, .base = fs::path(out_dir_fig, "hp_mach_%d", ext = "svg"))

rows <- c(1, 2, 3, 4, 16, 5, 6, 7)

boxplot_dat_m |>
  select(-variable) |>
  rename(Machiavellism = "value", House = "Sorting_house") |>
  relocate(Machiavellism, House) |>
  mutate(Dummy_Ravenclaw = ifelse(House == "Ravenclaw", 1, 0)) |>
  mutate(Dummy_Huffelpuff = ifelse(House == "Hufflepuff", 1, 0)) |>
  mutate(Dummy_Slytherin = ifelse(House == "Slytherin", 1, 0)) |>
  slice(rows) |>
  fragment_kable(steps = c(NA, NA, 2, 3, 4)) |>
  writeLines(path(out_dir_tb, "anova-dummy-table.qmd"))

