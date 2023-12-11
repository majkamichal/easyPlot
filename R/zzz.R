.onAttach <- function(libname, pkgname) {

  v <- getNamespaceVersion("easyPlot")
  rule <- paste0(rep("-", getOption("width")), collapse = "")

  packageStartupMessage(rule)
  packageStartupMessage(paste0("easyPlot ", v, " loaded"))
  packageStartupMessage("For more information please visit: ")
  packageStartupMessage("https://github.com/majkamichal/easyPlot")
  packageStartupMessage(rule)

}


.onDetach <- function(libpath) {

  rule <- paste0(rep("-", getOption("width")), collapse = "")
  packageStartupMessage(rule)
  packageStartupMessage("Thank you for using the easyPlot package!")
  packageStartupMessage("Don't forget to report bugs / request features under:")
  packageStartupMessage("https://github.com/majkamichal/easyPlot")

  packageStartupMessage(rule)
}
