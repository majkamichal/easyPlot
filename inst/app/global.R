# Options: ---------------------------------------------------------------------
options(shiny.maxRequestSize = 100 * 1024^2)


# Libraries: -------------------------------------------------------------------
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(colourpicker))
suppressPackageStartupMessages(library(Hmisc))
library(shinyBS)
library(shinythemes)
library(shinyWidgets)
library(scales)
library(Cairo)
library(shinyAce)


# Define version for the UI header: --------------------------------------------
easy_plot_manual_version_ <- "0.9.8"

easy_plot_package_version_ <- options()$easyPlot.version
easy_plot_version_ <- ifelse(is.null(easy_plot_package_version_),
                             easy_plot_manual_version_,
                             easy_plot_package_version_)

easy_plot_header_name <- paste0("easyPlot ", easy_plot_version_)


# Source files: ----------------------------------------------------------------
source("UI_recode_variables.R", local = TRUE)$value


# Source global variables / functions: -----------------------------------------

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
