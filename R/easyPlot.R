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
#' # Run easyPlot gui
#' if (interactive()) {
#'   easyPlot::easyPlot()
#' }
#'
#' # Pass custom data to easyPlot
#' if (interactive()) {
#'   easyPlot::easyPlot(iris)
#' }

easyPlot <- function(data = NULL) {

  appDir <- system.file("easyPlot", package = "easyPlot")

  if (appDir == "") {
      stop("easyPlot(): ", "Could not find the easyPlot directory. Try re-installing `easyPlot`.", call. = FALSE)
  }

  # Makes sure all dependencies are available
  dependencies <- c("ggplot2", "shinyWidgets", "shinyAce", "shinyBS",
                    "shinyjs", "shinythemes", "colourpicker", "scales",
                    "Hmisc", "Cairo", "DT",  "data.table", "openxlsx")

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
  options("easyPlot.show.my.data" = !is.null(data))
  options("easyPlot.package.mode" = TRUE)
  options("easyPlot.version" = as.character(getNamespaceVersion("easyPlot")))

  on.exit(options("easyPlot.version" = NULL))
  on.exit(options("easyPlot.show.my.data" = NULL))
  on.exit(options("easyPlot.package.mode" = NULL))

  shiny::runApp(appDir,
                display.mode = "normal",
                launch.browser = TRUE)
}

