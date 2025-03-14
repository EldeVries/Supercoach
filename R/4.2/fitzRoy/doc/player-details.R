## ----setup, include = FALSE---------------------------------------------------
not_cran <- identical(Sys.getenv("NOT_CRAN"), "true")
online <- !is.null(curl::nslookup("r-project.org", error = FALSE))
eval_param <- not_cran & online
eval_param <- packageVersion("fitzRoy") == "1.1.0.9000"

knitr::opts_chunk$set(
  eval = eval_param,
  message = FALSE,
  warning = FALSE,
  collapse = TRUE,
  comment = "#>"
)
# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(dplyr)

## ----details_aflm, eval=FALSE, include=TRUE-----------------------------------
# fetch_player_details("Hawthorn")

## ----details_aflm_included, echo=FALSE, eval=eval_param-----------------------
# fitzRoy:::details_aflm

## ----details_aflw, eval=FALSE, include=TRUE-----------------------------------
# details_aflw <- fetch_player_details(team = "Western Bulldogs", current = TRUE, comp = "AFLW", source = "AFL")
# 
# head(details_aflw)

## ----details_aflw_included, echo=FALSE, eval=eval_param-----------------------
# details_aflw <- fitzRoy:::details_aflw
# head(details_aflw)

## ----eval=eval_param----------------------------------------------------------
# glimpse(details_aflw)

## ----details_afltables, eval=FALSE, include=TRUE------------------------------
# fetch_player_details("Hawthorn", source = "afltables")

## ----details_afltables_included, echo=FALSE, eval=eval_param------------------
# fitzRoy:::details_afltables

## ----details_footywire, eval=FALSE, include=TRUE------------------------------
# fetch_player_details("Richmond", source = "footywire", current = TRUE)

## ----details_footywire_included, echo=FALSE, eval=eval_param------------------
# fitzRoy:::details_footywire

