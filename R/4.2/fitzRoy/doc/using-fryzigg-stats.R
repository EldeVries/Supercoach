## ----include = FALSE----------------------------------------------------------
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


dat <- fitzRoy:::stats_fryzigg_2019

## ----results, eval=FALSE, include=TRUE----------------------------------------
# dat <- fitzRoy::fetch_player_stats_fryzigg(2019)

## ----results2, eval=eval_param------------------------------------------------
# dplyr::glimpse(dat)

## ----results3, eval=eval_param------------------------------------------------
# head(dat)

