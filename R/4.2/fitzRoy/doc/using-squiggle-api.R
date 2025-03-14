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

## -----------------------------------------------------------------------------
# library(fitzRoy)
# library(dplyr)

## ----squiggle_games, include=TRUE,eval=FALSE----------------------------------
# fetch_squiggle_data(query = "games", year = 2020)

## ----squiggle_game_included, echo=FALSE, eval=eval_param----------------------
# fitzRoy:::squiggle_games %>% filter(year == 2020)

## ----squiggle_teams, include=TRUE, eval=FALSE---------------------------------
# fetch_squiggle_data("teams")

## ----squiggle_teams_included, echo=FALSE, eval=eval_param---------------------
# fitzRoy:::squiggle_teams

## ----squiggle_games2, include=TRUE,eval=FALSE---------------------------------
# fetch_squiggle_data(query = "games", year = 2020)

## ----squiggle_game2_included, echo=FALSE, eval=eval_param---------------------
# fitzRoy:::squiggle_games %>% filter(year == 2020)

## ----squiggle_sources1, include=TRUE, eval=FALSE------------------------------
# # You can get the sources
# fetch_squiggle_data("sources")

## ----squiggle_sources1_included, echo=FALSE, eval=eval_param------------------
# fitzRoy:::squiggle_sources

## ----squiggle_tips1,  include=TRUE, eval=FALSE--------------------------------
# # Get all tips
# fetch_squiggle_data("tips")

## ----squiggle_tips2, echo=FALSE, eval=eval_param------------------------------
# fitzRoy:::squiggle_tips

## ----squiggle_round1, include=TRUE, eval=FALSE--------------------------------
# # Get` just tips from round 1, 2018
# fetch_squiggle_data("tips", round = 1, year = 2018)

## ----squiggle_round2, echo=FALSE, eval=eval_param-----------------------------
# fitzRoy:::squiggle_tips %>% filter(year == 2018 & round == 1)

## ----squiggle_standings,  include=TRUE, eval=FALSE----------------------------
# fetch_squiggle_data("standings", year = 2020, round = 1)

## ----squiggle_standings2, echo=FALSE, eval=eval_param-------------------------
# fitzRoy:::squiggle_standings

## ----squiggle_ladder,  include=TRUE, eval=FALSE-------------------------------
# fetch_squiggle_data("ladder", year = 2019, round = 15, source = 1)

## ----squiggle_ladder_included, echo=FALSE, eval=eval_param--------------------
# fitzRoy:::squiggle_ladder

## ----squiggle_pav,  include=TRUE, eval=FALSE----------------------------------
# fetch_squiggle_data("pav",
#   firstname = "Dustin",
#   surname = "Martin",
#   year = 2017
# )

## ----squiggle_pav_included, echo=FALSE, eval=eval_param-----------------------
# fitzRoy:::squiggle_pav

