# Options: ---------------------------------------------------------------------
options(shiny.maxRequestSize = 100 * 1024^2)


# Libraries: -------------------------------------------------------------------
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(colourpicker))
suppressPackageStartupMessages(library(Hmisc))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(shinyBS))
suppressPackageStartupMessages(library(shinythemes))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(Cairo))
suppressPackageStartupMessages(library(shinyAce))


# Define version for the UI header: --------------------------------------------
easy_plot_manual_version_ <- "1.0.0"

easy_plot_package_version_ <- options()$easyPlot.version
easy_plot_version_ <- ifelse(is.null(easy_plot_package_version_),
                             easy_plot_manual_version_,
                             easy_plot_package_version_)

easy_plot_header_name_ <- paste0("easyPlot ", easy_plot_version_)


# Detect package/server mode: --------------------------------------------------
package_mode_bool_ <- FALSE
package_mode_bool_option <- options()$easyPlot.package.mode
if (!is.null(package_mode_bool_option) && package_mode_bool_option) {
  package_mode_bool_ <- TRUE
}

# Define whether "My data" checkbox is shown: ----------------------------------
show_my_data_bool_ <- FALSE
show_my_data_bool_option <- options()$easyPlot.show.my.data
if (!is.null(show_my_data_bool_option) && show_my_data_bool_option) {
  show_my_data_bool_ <- TRUE
}


# Source files: ----------------------------------------------------------------
source("moduleRecodeVariables.R", local = TRUE)$value
source("moduleDownloadPlot.R", local = TRUE)$value


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
