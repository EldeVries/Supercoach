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

## -----------------------------------------------------------------------------
# not_cran
# online
# eval_param

## ----fetch_fixture_aflw2, eval=FALSE, include=TRUE----------------------------
# fetch_fixture(season = 2021, comp = "AFLW") %>%
#   select(
#     compSeason.name, round.name,
#     home.team.name, away.team.name,
#     venue.name
#   )

## ----fetch_fixture_aflw2_included,  echo=FALSE, eval=eval_param---------------
# fitzRoy:::fixture_afl_aflw_2021 %>%
#   select(
#     compSeason.name, round.name,
#     home.team.name, away.team.name,
#     venue.name
#   )

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

## ----fetch_results_aflw, eval=FALSE, include=TRUE-----------------------------
# fetch_results(2020, round_number = 1, comp = "AFLW") %>%
#   select(
#     match.date, match.name,
#     homeTeamScore.matchScore.totalScore, awayTeamScore.matchScore.totalScore
#   )

## ----fetch_results_aflw_included,  echo=FALSE, eval=eval_param----------------
# fitzRoy:::results_afl_aflw_2020 %>%
#   dplyr::filter(round.roundNumber == 1) %>%
#   select(
#     match.date, match.name,
#     homeTeamScore.matchScore.totalScore,
#     awayTeamScore.matchScore.totalScore
#   )

## ----fetch_ladder_aflw2, eval=FALSE, include=TRUE-----------------------------
# fetch_ladder(2020, round_number = 6, comp = "AFLW") %>%
#   select(
#     season, round_name,
#     position, team.name, played,
#     pointsFor, pointsAgainst
#   )

## ----fetch_ladder_aflw2_included,  echo=FALSE, eval=eval_param----------------
# fitzRoy:::ladder_afl_aflw_2020 %>%
#   dplyr::filter(round_number == 7) %>%
#   select(
#     season, round_name,
#     position, team.name, played,
#     pointsFor, pointsAgainst
#   )

## ----fetch_stats, eval=FALSE, include=TRUE------------------------------------
# fetch_player_stats(2020, round_number = 1, comp = "AFLW") %>%
#   select(player.player.player.givenName:clearances.totalClearances)

## ----fetch_stats_included, echo=FALSE, eval=eval_param------------------------
# fitzRoy:::stats_afl_aflw_2020 %>%
#   dplyr::filter(round.roundNumber == 1) %>%
#   dplyr::select(player.player.player.givenName:clearances.totalClearances)

## ----details_aflw, eval=FALSE, include=TRUE-----------------------------------
# details_aflw <- fetch_player_details(team = "Western Bulldogs", current = TRUE, comp = "AFLW", source = "AFL")
# 
# head(details_aflw)

## ----details_aflw_included, echo=FALSE, eval=eval_param-----------------------
# details_aflw <- fitzRoy:::details_aflw
# head(details_aflw)

## ----fetch_coaches_votes_aflw, eval=FALSE, include=TRUE-----------------------
# fetch_coaches_votes(season = 2021, round_number = 9, comp = "AFLW", team = "Western Bulldogs")

## ----fetch_coaches_votes_aflw_included, echo=FALSE, eval=eval_param-----------
# fitzRoy:::aflw_coaches_votes %>%
#   dplyr::filter(Home.Team == "Western Bulldogs" & Round == 9) %>%
#   head()

## ----cookie, eval=FALSE, include=TRUE-----------------------------------------
# cookie <- get_afl_cookie()
# print(cookie)

## ----cookie_included, echo=FALSE, eval=eval_param-----------------------------
# cookie <- fitzRoy:::cookie
# print(cookie)

## ----cookie_param, include=FALSE, eval=eval_param-----------------------------
# if (is.null(cookie)) {
#   eval_param <- FALSE
# }

## ----fetch_match_stats, eecho=FALSE, eval=eval_param--------------------------
# match_data <- fetch_results(2020, round_number = 1, comp = "AFLW")

## ----fetch_match_stats2, include=FALSE, eval = eval_param---------------------
# match_data <- fitzRoy:::results_afl_aflw_2020 %>%
#   dplyr::filter(round.roundNumber == 1)

## ----show_match_stats, eval=eval_param----------------------------------------
# glimpse(match_data)

## ----first_10, eval = eval_param----------------------------------------------
# first10 <- head(match_data, 10)
# first10_ids <- first10$Match.Id
# first10_ids

## ----detailed, eval=FALSE, include=TRUE---------------------------------------
# detailed <- get_aflw_detailed_data(first10_ids)
# glimpse(detailed)

## ----detailed_included, echo=FALSE, eval=eval_param---------------------------
# fitzRoy:::detailed_stats_aflw_2020 %>% glimpse()

