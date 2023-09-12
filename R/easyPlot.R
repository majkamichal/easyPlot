#' Start an interactive User Interface easyPlot
#'
#' @param data (character) an optional data frame with at least two columns passed as a character string
#'
#' @author Michal Majka
#'
#' @import shiny
#'
#' @export
#'
#' @examples
#' # easyPlot()
#'
#' # easyPlot(data = "iris")

easyPlot <- function(data = "FALSE") {

  appDir <- system.file("app", package = "easyPlot")

  if (appDir == "")
    stop("Could not find directory. Try re-installing 'easyPlot'.", call. = FALSE)

  # Makes sure all dependencies are available
  dependencies <- c("ggplot2", "shinyjs", "shinyAce", "shinyBS",
                    "shinythemes", "Cairo", "DT", "colourpicker")

  ind_missing_package <- !dependencies %in% utils::installed.packages()[ ,"Package"]

  if (any(ind_missing_package)) {
      n_missing_packages <- sum(ind_missing_package)
      stop("Please install ",
           ifelse(n_missing_packages == 1, "the", "these"),
           " missing package",
           ifelse(n_missing_packages > 1, "s", ""),
           ":\n ",
           "install.packages(",
           ifelse(n_missing_packages > 1, "c(", ""),
           paste0("\"", dependencies[ind_missing_package], collapse = "\", "),
           ifelse(n_missing_packages > 1, "\"))", "\")"))
  }

  if (!is.character(data))
    stop("easyPlot(): ", "Input has to be a character string with the name of a data frame", call. = FALSE)

  if (data != "FALSE") {

    if (!is.data.frame(get(data)) | length(get(data)) < 2)
      stop("easyPlot(): ", "Please provide the name of a data frame with at least two columns", call. = FALSE)
  }

  # # After closing the app these variables will be automatically removed
  # assign(".easyPlotEnv", new.env(), envir = .GlobalEnv)
  # assign("name", data, envir = .easyPlotEnv)

  pos <- 1
  .easyPlotEnv <- as.environment(pos)
  assign("name", data, envir = .easyPlotEnv)

  shiny::runApp(appDir,
                display.mode = "normal",
                launch.browser = TRUE)
}

