## ----setup, include = FALSE---------------------------------------------------
not_cran <- identical(Sys.getenv("NOT_CRAN"), "true")
online <- !is.null(curl::nslookup("r-project.org", error = FALSE))
eval_param <- not_cran & online

knitr::opts_chunk$set(
  eval = eval_param,
  message = FALSE,
  warning = FALSE,
  collapse = TRUE,
  comment = "#>"
)

library(fitzRoy)
library(dplyr)

## ----fetch_coaches_votes_aflm, eval=FALSE, include=TRUE-----------------------
# fetch_coaches_votes(season = 2021, comp = "AFLM") %>% head()

## ----fetch_coaches_votes_aflm_included, echo=FALSE, eval=eval_param-----------
# fitzRoy:::aflm_coaches_votes %>% head()

## ----fetch_coaches_votes_aflw, eval=FALSE, include=TRUE-----------------------
# fetch_coaches_votes(season = 2021, comp = "AFLW") %>% head()

## ----fetch_coaches_votes_aflw_included, echo=FALSE, eval=eval_param-----------
# fitzRoy:::aflw_coaches_votes %>% head()

## ----fetch_coaches_votes2_aflm, eval=FALSE, include=TRUE----------------------
# fetch_coaches_votes(season = 2021, round_number = 24, comp = "AFLM")

## ----fetch_coaches_votes2_aflm_included, echo=FALSE, eval=eval_param----------
# fitzRoy:::aflm_coaches_votes %>%
#   dplyr::filter(Round == 24) %>%
#   head()

## ----fetch_coaches_votes2_aflw, eval=FALSE, include=TRUE----------------------
# fetch_coaches_votes(season = 2021, round_number = 9, comp = "AFLW")

## ----fetch_coaches_votes2_aflw_included, echo=FALSE, eval=eval_param----------
# fitzRoy:::aflw_coaches_votes %>%
#   dplyr::filter(Round == 9) %>%
#   head()

## ----fetch_coaches_votes3_aflm, eval=FALSE, include=TRUE----------------------
# fetch_coaches_votes(season = 2021, comp = "AFLM", team = "Western Bulldogs")

## ----fetch_coaches_votes3_aflm_included, echo=FALSE, eval=eval_param----------
# fitzRoy:::aflm_coaches_votes %>%
#   dplyr::filter(Home.Team == "Western Bulldogs" | Away.Team == "Western Bulldogs") %>%
#   head()

## ----fetch_coaches_votes3_aflw, eval=FALSE, include=TRUE----------------------
# fetch_coaches_votes(season = 2021, comp = "AFLW", team = "Western Bulldogs")

## ----fetch_coaches_votes3_aflw_included, echo=FALSE, eval=eval_param----------
# fitzRoy:::aflw_coaches_votes %>%
#   dplyr::filter(Home.Team == "Western Bulldogs" | Away.Team == "Western Bulldogs") %>%
#   head()

## ----fetch_coaches_votes4_aflm, eval=FALSE, include=TRUE----------------------
# fetch_coaches_votes(season = 2021, round_number = 24, comp = "AFLM", team = "Western Bulldogs")

## ----fetch_coaches_votes4_aflm_included, echo=FALSE, eval=eval_param----------
# fitzRoy:::aflm_coaches_votes %>%
#   dplyr::filter(Home.Team == "Western Bulldogs" & Round == 24) %>%
#   head()

## ----fetch_coaches_votes4_aflw, eval=FALSE, include=TRUE----------------------
# fetch_coaches_votes(season = 2021, round_number = 9, comp = "AFLW", team = "Western Bulldogs")

## ----fetch_coaches_votes4_aflw_included, echo=FALSE, eval=eval_param----------
# fitzRoy:::aflw_coaches_votes %>%
#   dplyr::filter(Home.Team == "Western Bulldogs" & Round == 9) %>%
#   head()

## ----calculate_coaches_votes, eval=FALSE, include=TRUE------------------------
# df <- fetch_coaches_votes(season = 2021, round_number = 24, comp = "AFLM", team = "Western Bulldogs")
# calculate_coaches_vote_possibilities(df, "Coach View")

## ----calculate_coaches_votes_included, echo=FALSE, eval=eval_param------------
# fitzRoy:::aflm_coaches_votes %>%
#   dplyr::filter(Round == 24 & Home.Team == "Western Bulldogs") %>%
#   fitzRoy:::calculate_coaches_vote_possibilities("Coach View")

## ----calculate_coaches_votes2, eval=eval_param, include=TRUE------------------
# df <- data.frame(
#   Player.Name = c("Tom Liberatore", "Jack Macrae", "Marcus Bontempelli", "Cody Weightman", "Darcy Parish", "Aaron Naughton", "Jordan Ridley"),
#   Coaches.Votes = c(7, 6, 5, 5, 4, 2, 1)
# )
# calculate_coaches_vote_possibilities(df, "Player View")

