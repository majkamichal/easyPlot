# Libraries: -------------------------------------------------------------------
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(colourpicker))
library(shinyBS)
library(shinythemes)
library(shinyWidgets)
library(scales)
library(Cairo)
library(shinyAce)


# Source files: ----------------------------------------------------------------
source("UI_recode_variables.R", local = TRUE)$value


# Source global variables: -----------------------------------------------------
syntax <- function(a = NULL) {
  res <- paste(a, collapse = "")
  substring(res, 3)
}


is.numeric_logical <- function(x) {
  all(x %in% c(0, 1))
}


is.datetime <- function(x) {
  inherits(x, "POSIXct")
}


is.date <- function(x) {
  inherits(x, "Date")
}


convert_numeric_to_date <- function(value, is_date) {
  if (is_date) as.Date(value, origin = "1970-01-01")
  else value
}


convert_numeric_to_datetime <- function(value, is_datetime) {
  if (is_datetime) as.POSIXct(value, origin = "1970-01-01 00:00.00 UTC")
  else value
}
