#' Start an Interactive User Interface `easyPlot`
#'
#' @param data (data.frame) an optional data frame with at least two columns passed as a character string
#'
#' @author Michal Majka
#'
#' @import shiny
#'
#' @export
#'
#' @examples
#' # Rund easyPlot gui
#' if (interactive()) {
#'   easyPlot()
#' }
#'
#' # Pass custom data to easyPlot
#' if (interactive()) {
#'   easyPlot(iris)
#' }
#'

easyPlot <- function(data = NULL) {

  appDir <- system.file("app", package = "easyPlot")

  if (appDir == "") {
      stop("easyPlot(): ", "Could not find the easyPlot directory. Try re-installing `easyPlot`.", call. = FALSE)
  }

  # Makes sure all dependencies are available
  dependencies <- c("ggplot2", "shinyjs", "shinyAce", "shinyBS", "scales", "Hmisc",
                    "shinythemes", "shinyWidgets", "Cairo", "DT", "colourpicker")

  ind_missing_package <- !dependencies %in% utils::installed.packages()[ ,"Package"]

  if (any(ind_missing_package)) {
      n_missing_packages <- sum(ind_missing_package)
      stop("Please install ",
           ifelse(n_missing_packages == 1, "the", "these"),
           " missing package",
           ifelse(n_missing_packages > 1, "s", ""),
           ":\n\n ",
           "install.packages(",
           ifelse(n_missing_packages > 1, "c(", ""),
           paste0("\"", dependencies[ind_missing_package], collapse = "\", "),
           ifelse(n_missing_packages > 1, "\"))", "\")"), "\n\n")
  }

  if (!is.null(data) && (!is.data.frame(data) | length(data) < 2)) {
      stop("easyPlot(): ", "Please provide a data frame with at least two columns", call. = FALSE)
  }

  options("easyPlot.shiny.data" = data)
  options("easyPlot.shiny.name" = deparse(substitute(data)))
  options("easyPlot.version" = as.character(packageVersion("easyPlot")))

  on.exit(options("easyPlot.version" = NULL))

  shiny::runApp(appDir,
                display.mode = "normal",
                launch.browser = TRUE)
}

