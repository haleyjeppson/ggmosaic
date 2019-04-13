#' Calculate frequencies.
#'
#' @param data input data frame
#' @param formula formula specifying display of plot
#' @param divider divider function
#' @param cascade cascading amount, per nested layer
#' @param scale_max Logical vector of length 1. If \code{TRUE} maximum values
#'   within each nested layer will be scaled to take up all available space.
#'   If \code{FALSE}, areas will be comparable between nested layers.
#' @param na.rm Logical vector of length 1 - should missing levels be
#'   silently removed?
#' @keywords internal
#' @importFrom utils getFromNamespace
#' @export
#' @examples
#' \dontrun{
#' library(productplots)
#' prodcalc(happy, ~ happy, "hbar", offset = 0.005)
#' prodcalc(happy, ~ happy, "hspine", offset = 0.01)
#' }
prodcalc <- function(data, formula, divider = mosaic(), cascade = 0, scale_max = TRUE, na.rm = FALSE, offset = offset) {

  vars <- parse_product_formula(stats::as.formula(formula))
#browser()
  if (length(vars$wt) == 1) {
    data$.wt <- data[[vars$wt]]
  } else {
    data$.wt <- 1
  }
  margin <- getFromNamespace("margin", "productplots")

  wt <- margin(data, vars$marg, vars$cond)
  wt2 <- margin(data, c(vars$marg, vars$cond)) # getting margins
  #browser()
  #wt$.n <- wt2$.wt

  if (na.rm) {
    wt <- wt[stats::complete.cases(wt), ]
  }


  if (is.function(divider)) divider <- divider(ncol(wt) - 1)
  if (is.character(divider)) divider <- lapply(divider, match.fun)

  max_wt <- if (scale_max) NULL else 1

  df <- divide(wt, divider = rev(divider), cascade = cascade, max_wt = max_wt, offset = offset)
#  browser()
  wt2 <- dplyr::rename(wt2, .n=".wt")
  dplyr::left_join(df, wt2, by = setdiff(names(wt2), ".n"))
}
