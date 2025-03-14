## ----setup, echo=FALSE--------------------------------------------------------
not_cran <- identical(Sys.getenv("NOT_CRAN"), "true")
online <- !is.null(curl::nslookup("r-project.org", error = FALSE))
eval_param <- not_cran & online


knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(dplyr)
library(fitzRoy)

## ----fixture, include=TRUE, eval=FALSE----------------------------------------
# fixture <- fetch_fixture(2021, comp = "AFLW")
# fixture %>%
#   select(
#     utcStartTime, round.name,
#     home.team.name, away.team.name, venue.name
#   )

## ----fixture_included, echo=FALSE, eval=eval_param----------------------------
# fixture <- fitzRoy:::fixture_afl_aflw_2021
# fixture %>%
#   select(
#     utcStartTime, round.name,
#     home.team.name, away.team.name, venue.name
#   )

## ----fixture2, include=TRUE, eval=FALSE---------------------------------------
# fetch_fixture(2021, round_number = 5, comp = "AFLM") %>%
#   select(
#     utcStartTime, round.name,
#     home.team.name, away.team.name, venue.name
#   )

## ----fixture2_included, echo=FALSE, eval=eval_param---------------------------
# fitzRoy:::fixture_afl_aflm_2021 %>%
#   filter(round.roundNumber == 5) %>%
#   select(
#     utcStartTime, round.name,
#     home.team.name, away.team.name, venue.name
#   )

## ----fixture_all, eval=FALSE--------------------------------------------------
# fixture_afl <- fetch_fixture(2020)
# fixture_aflw <- fetch_fixture(2020, round_number = 1, comp = "AFLW")
# fixture_squiggle <- fetch_fixture_squiggle(2020, round_number = 10)
# fixture_footywire <- fetch_fixture_squiggle(2018)

## ----lineup, include=TRUE, eval=FALSE-----------------------------------------
# fetch_lineup(2021, round_number = 1, comp = "AFLW") %>%
#   select(
#     round.name, status, teamName,
#     player.playerName.givenName,
#     player.playerName.surname, teamStatus
#   )

## ----lineup_included, echo=FALSE, eval=eval_param-----------------------------
# fitzRoy:::lineup_aflw_2021_1 %>%
#   select(
#     round.name, status, teamName,
#     player.playerName.givenName,
#     player.playerName.surname, teamStatus
#   )

## ----results, include=TRUE, eval=FALSE----------------------------------------
# results <- fetch_match_results_afltables(1897:2019)
# results

## ----results_included, echo=FALSE, eval=eval_param----------------------------
# results <- fitzRoy:::results_afltables_all %>%
#   filter(Date < "2020-01-01")
# 
# results

## ----results2, include=TRUE, eval=FALSE---------------------------------------
# results_new <- fetch_results_afltables(2021)
# bind_rows(results, results_new)

## ----results2_included, echo=FALSE, eval=eval_param---------------------------
# results_new <- fitzRoy:::results_afltables_all %>%
#   filter(Date >= "2020-01-01")
# 
# bind_rows(results, results_new)

## ----results-all, eval=FALSE--------------------------------------------------
# results_afl <- fetch_results(2020, round_number = 11)
# results_aflw <- fetch_results(2020, comp = "AFLW")
# results_squiggle <- fetch_results_squiggle(2019, round_number = 1)
# results_footywire <- fetch_results_footywire(1990)

## ----results_aflw, include=TRUE, eval=FALSE-----------------------------------
# fetch_results(2020, comp = "AFLW") %>%
#   select(
#     match.name, venue.name, round.name,
#     homeTeamScore.matchScore.totalScore,
#     awayTeamScore.matchScore.totalScore
#   )

## ----results__afl2_included, echo=FALSE, eval=eval_param----------------------
# fitzRoy:::results_afl_aflw_2020 %>%
#   select(
#     match.name, venue.name, round.name,
#     homeTeamScore.matchScore.totalScore,
#     awayTeamScore.matchScore.totalScore
#   )

## ----ladder, include=TRUE, eval=FALSE-----------------------------------------
# ladder <- fetch_ladder(2020, round_number = 7, comp = "AFLW") %>%
#   select(
#     season, round_name, position,
#     team.name, pointsFor, pointsAgainst, form
#   )
# ladder

## ----ladder_included, echo=FALSE, eval=eval_param-----------------------------
# ladder <- fitzRoy:::ladder_afl_aflw_2020 %>%
#   select(
#     season, round_name, position,
#     team.name, pointsFor, pointsAgainst, form
#   )
# ladder

## ----ladder2, include=TRUE, eval=FALSE----------------------------------------
# ladder <- fetch_ladder(2020, round_number = 7, comp = "AFLW")
# ncol(ladder)

## ----ladder2_included, echo=FALSE, eval=eval_param----------------------------
# ncol(fitzRoy:::ladder_afl_aflw_2020)

## ----ladder-all, eval=FALSE---------------------------------------------------
# ladder_afl <- fetch_ladder(2020, round_number = 11)
# ladder_aflw <- fetch_ladder(2020, comp = "AFLW")
# ladder_squiggle <- fetch_ladder_squiggle(2019, round_number = 1)
# ladder_afltables <- fetch_ladder_afltables(1990)

## ----stats, include=TRUE, eval=FALSE------------------------------------------
# fetch_player_stats(2020, comp = "AFLW")

## ----stats_included, echo=FALSE, eval=eval_param------------------------------
# fitzRoy:::stats_afl_aflw_2020

## ----stats2, include=TRUE, eval=FALSE-----------------------------------------
# fetch_player_stats(2019, source = "fryzigg")

## ----stats2_included, echo=FALSE, eval=eval_param-----------------------------
# fitzRoy:::stats_fryzigg_2019

## ----stats-all, eval=FALSE----------------------------------------------------
# stats_afl <- fetch_player_stats(2020, round_number = 11)
# stats_aflw <- fetch_player_stats(2020, source = "AFL", comp = "AFLW")
# stats_footywire <- fetch_player_stats(2019, round_number = 1, source = "footywire")
# stats_afltables <- fetch_player_stats_afltables(1990)

