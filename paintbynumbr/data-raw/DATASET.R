## code to prepare `DATASET` dataset goes here

library(readr)
library(dplyr)

fars_accidents <- read_csv("accident.csv") %>% filter(across(.cols = is.character()))

usethis::use_data(fars_accidents, overwrite = TRUE)
#' Fatality Analysis Reporting System (FARS)
#'
#' A dataset containing records of fatal automtove accidents in the US
#'
#' @format A data frame with35766 rows and 81 variables:
#' \describe{
#'   \item{LONGITUD}{Longitude (deimal notation)}
#'   \item{LATITUDE}{Latitude (decimal notation)}
#'   ...
#' }
#' @source \url{http://https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars/}
"fars_accidents"
