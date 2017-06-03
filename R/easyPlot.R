#' Launch easyPlot interactive User Interface
#'
#' @param data an optional data frame with at least two columns passed as a character string
#' @details
#' easyPlot()
#'
#' data(iris)
#'
#' easyPlot(data = "iris")
#' @export
#' @importFrom shiny runApp

easyPlot <- function(data = "FALSE") {

  appDir <- system.file("app", package = "easyPlot")

  if (appDir == "")
    stop("Could not find directory. Try re-installing 'easyPlot'.", call. = FALSE)

  if (!is.character(data))
    stop("Input has to be a character string with the name of a data frame")

  if (data != "FALSE") {

    if (!is.data.frame(get(data)) )
      stop("Please provide the name of a data frame with at least two columns")

    if (length(get(data)) < 2)
      stop("Please provide the name of a data frame with at least two columns")
  }

  # After closing the app these variables will be automatically removed
  assign(".easyPlotEnv", new.env(), envir = .GlobalEnv)
  assign("name", data, envir = .easyPlotEnv)

  shiny::runApp(appDir, display.mode = "normal")

}

