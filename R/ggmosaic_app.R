#' Launch shiny app
#'
#' Shiny app "EDA with Mosaic Plots" for interactive exploratory model building
#'
#' @param ... arguments passed on.
#'
#' @export
ggmosaic_app <- function(...) {
  appDir <- system.file("shiny", package = "ggmosaic")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ggmosaic`.", call. = FALSE)
  }
  getFromNamespace("runApp", asNamespace("shiny"))(appDir, ...)
}
