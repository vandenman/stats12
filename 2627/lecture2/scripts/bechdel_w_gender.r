library(tidyverse)
library(httr2)
library(jsonlite)
library(fivethirtyeight)
library(dplyr)
library(purrr)

bechdel_small <- bechdel %>%
  transmute(
    title,
    year,
    imdb,
    clean_test,
    binary
  ) %>%
  filter(!is.na(imdb))

get_director_gender <- function(imdb_ids) {
  
  values <- paste0(
    '"', imdb_ids, '"',
    collapse = " "
  )
  
  query <- paste0('
    SELECT ?imdb ?director ?directorLabel ?gender ?genderLabel WHERE {
      VALUES ?imdb { ', values, ' }

      ?film wdt:P345 ?imdb ;
            wdt:P57 ?director .

      OPTIONAL {
        ?director wdt:P21 ?gender .
      }

      SERVICE wikibase:label {
        bd:serviceParam wikibase:language "en".
      }
    }
  ')
  
  resp <- httr2::request("https://query.wikidata.org/sparql") %>%
    httr2::req_url_query(
      query = query,
      format = "json"
    ) %>%
    httr2::req_user_agent(
      "R teaching dataset preparation"
    ) %>%
    httr2::req_perform()
  
  x <- httr2::resp_body_json(resp)$results$bindings
  
  purrr::map_dfr(x, \(z) {
    tibble::tibble(
      imdb = z$imdb$value,
      director = z$directorLabel$value,
      gender = if (!is.null(z$genderLabel)) {
        z$genderLabel$value
      } else {
        NA_character_
      }
    )
  })
}
get_director_gender <- function(imdb_ids) {
  
  values <- paste0(
    '"', imdb_ids, '"',
    collapse = " "
  )
  
  query <- paste0('
    SELECT ?imdb ?director ?directorLabel ?gender ?genderLabel WHERE {
      VALUES ?imdb { ', values, ' }

      ?film wdt:P345 ?imdb ;
            wdt:P57 ?director .

      OPTIONAL {
        ?director wdt:P21 ?gender .
      }

      SERVICE wikibase:label {
        bd:serviceParam wikibase:language "en".
      }
    }
  ')
  
  resp <- httr2::request("https://query.wikidata.org/sparql") %>%
    httr2::req_url_query(
      query = query,
      format = "json"
    ) %>%
    httr2::req_user_agent(
      "Bechdel teaching dataset preparation"
    ) %>%
    httr2::req_retry(
      max_tries = 6,
      backoff = \(i) 5 * 2^(i - 1)
    ) %>%
    httr2::req_perform()
  
  x <- httr2::resp_body_json(resp)$results$bindings
  
  purrr::map_dfr(x, \(z) {
    tibble::tibble(
      imdb = z$imdb$value,
      director = z$directorLabel$value,
      gender = if (!is.null(z$genderLabel)) {
        z$genderLabel$value
      } else {
        NA_character_
      }
    )
  })
}
ids <- unique(bechdel_small$imdb)

batches <- split(ids, ceiling(seq_along(ids) / 100))

i<<-0
director_info <- map_dfr(
  batches,
  \(x) {
    Sys.sleep(2)
    print(i)
    i <<- i + 1
    get_director_gender(x)
  }
)
director_info %>%
  count(gender, sort = TRUE)

director_info %>%
  summarise(
    rows = n(),
    films = n_distinct(imdb),
    directors = n_distinct(director)
  )
director_info %>%
  count(imdb) %>%
  count(n, name = "films")
director_gender <- director_info %>%
  mutate(
    gender_simple = case_when(
      gender == "male" ~ "Male",
      gender == "female" ~ "Female",
      is.na(gender) ~ NA_character_,
      TRUE ~ "Other"
    )
  ) %>%
  group_by(imdb) %>%
  summarise(
    director = paste(unique(director), collapse = "; "),
    director_gender = case_when(
      all(gender_simple == "Male", na.rm = TRUE) ~ "Male",
      all(gender_simple == "Female", na.rm = TRUE) ~ "Female",
      all(is.na(gender_simple)) ~ NA_character_,
      TRUE ~ "Mixed/other"
    ),
    .groups = "drop"
  )

bechdel_teaching <- bechdel %>%
  left_join(director_gender, by = "imdb")

write.csv(file = "lecture2/scripts/movies2.csv", x = bechdel_teaching)
write.csv("scr")

d0 <- read.csv("lecture2/scripts/movies.csv")
d0
as.data.frame(bechdel_teaching)

table(bechdel_teaching$binary, bechdel_teaching$director_gender)

library(gtsummary)
set.seed(1)
n = 74
dutch <- sample.int(2, size = n, replace = TRUE, prob = c(.3, .7))
tattoo <- sapply(c(.2, .5)[dutch], rbinom, n = 1, size = 1)
df_tattoo <- data.frame(Dutch = ordered(dutch, levels = c(1,2), labels = c("No", "Yes")),
                        Tattoo = ordered(tattoo, levels = c(0,1), labels = c("No", "Yes")))
tmp <- table(df_tattoo); cell11 = tmp[1,1]; cell12 = tmp[1,2]; cell21 = tmp[2,1]; cell22 = tmp[2,2]
library(gt)
tbl_cross(df_tattoo, row=Dutch, col=Tattoo, percent="none")

df2 <- bechdel_teaching
df2$director_gender[is.na(df2$director_gender)] <- "Mixed/other"
df2$Test <- df2$binary
df2$Gender <- df2$director_gender
tbl_cross(df2, row=Test, col=Gender, percent="none")

library(kableExtra)



df <- data.frame("Dutch" = c("No", "Yes", "<b>Total column</b>"),
                 "No" = c(cell11, cell21, cell11+cell21),
                 "Yes" = c(cell12, cell22, cell12+cell22),
                 "Total row" = c(
                   cell11+cell12, cell21+cell22, cell11+cell12+cell21+cell22),
                 check.names = FALSE)
df$`Total row`[1:2] <- cell_spec(df$`Total row`[1:2], color = "purple", bold = TRUE)
df$No[3] <- cell_spec(df$No[3], color = "orange", bold = TRUE)
df$Yes[3] <- cell_spec(df$Yes[3], color = "orange", bold = TRUE)
kable(df, escape = FALSE, align = rep('l', length(df[,1])), format = "html") %>%
  kable_styling("striped") %>%
  add_header_above(c(" " = 1, "Female" = 2, "Male" = 3), escape = FALSE)

