#' Launch shiny app
#'
#' Shiny app "EDA with Mosaic Plots" for interactive exploratory model building
#'
#' @param example Selected shiny app to launch.
#' @param ... arguments passed on.
#'
#' @export
ggmosaic_app <- function(example = c("mosaics", "models"), ...) {
  # validate example
  example <- rlang::arg_match(example)

  appDir <- system.file("shiny", example, package = "ggmosaic")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ggmosaic`.", call. = FALSE)
  }
  getFromNamespace("runApp", asNamespace("shiny"))(appDir, ...)
}
