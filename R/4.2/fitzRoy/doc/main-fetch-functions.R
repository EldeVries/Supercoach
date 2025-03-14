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

## ----fetch_fixture, eval=FALSE, include=TRUE----------------------------------
# fetch_fixture(2021)

## ----fetch_fixture_included, echo=FALSE, eval=eval_param----------------------
# fitzRoy:::fixture_afl_aflm_2021

## ----fetch_fixture2, eval=FALSE, include=TRUE---------------------------------
# fetch_fixture(season = 2021, comp = "AFLM", source = "AFL")

## ----fetch_fixture3, eval=FALSE, include=TRUE---------------------------------
# fetch_fixture(season = 2021, round_number = 2) %>%
#   select(compSeason.name, round.name, home.team.name, away.team.name, venue.name)

## ----fetch_fixture3_included, echo=FALSE, eval=eval_param---------------------
# fitzRoy:::fixture_afl_aflm_2021 %>%
#   dplyr::filter(round.roundNumber == 2) %>%
#   dplyr::select(
#     compSeason.name, round.name,
#     home.team.name, away.team.name,
#     venue.name
#   )

## ----fetch_fixture_aflw, eval=FALSE, include=TRUE-----------------------------
# fetch_fixture(season = 2021, comp = "AFLW") %>%
#   select(
#     compSeason.name, round.name,
#     home.team.name, away.team.name,
#     venue.name
#   )

## ----fetch_fixture_aflw_included, echo=FALSE, eval=eval_param-----------------
# fitzRoy:::fixture_afl_aflw_2021 %>%
#   select(
#     compSeason.name, round.name,
#     home.team.name, away.team.name,
#     venue.name
#   )

## ----fetch_fixture_squiggle, eval=FALSE, include=TRUE-------------------------
# fetch_fixture(2021, round_number = 1, source = "squiggle")

## ----fetch_fixture_squiggle_included, echo=FALSE, eval=eval_param-------------
# fitzRoy:::fixture_squiggle_2021

## ----eval=FALSE, include=TRUE-------------------------------------------------
# # The following are the same
# fetch_fixture(2021, round_number = 5, source = "squiggle")
# fetch_fixture_squiggle(2021, round_number = 5)

## ----eval = FALSE-------------------------------------------------------------
# fetch_fixture(2022, source = "AFL", comp = "VFL")
# fetch_player_stats(2022, round = 1, source = "AFL", comp = "VFLW")
# fetch_fixture(2022, source = "AFL", comp = "WAFL")

