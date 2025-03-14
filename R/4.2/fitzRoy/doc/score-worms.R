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

## ----plot_score_worm, eval=TRUE, include=TRUE---------------------------------
plot_score_worm('CD_M20240142004') 

## ----plot_score_worm_totals, eval=TRUE, include=TRUE--------------------------
plot_score_worm_totals('CD_M20240142004') 

## ----fetch_score_worm_data, eval=TRUE, include=TRUE---------------------------
fetch_score_worm_data('CD_M20240142004')

## ----fetch_score_worm_data_multi, eval=FALSE, include=TRUE--------------------
# fetch_score_worm_data(c('CD_M20240142101','CD_M20240142102','CD_M20240142103'))

