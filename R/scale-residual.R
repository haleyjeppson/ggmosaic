#' Diverging color scale for Pearson residuals
#'
#' Provides a blue-white-red color scale centered at 0 for visualizing
#' Pearson residuals from loglinear models. Designed for use with
#' \code{geom_mosaic()} when \code{expected} parameter is specified.
#'
#' @param ... Arguments passed to \code{\link[ggplot2]{scale_fill_gradient2}}
#' @param low Color for negative residuals (default: "steelblue")
#' @param mid Color for zero residuals (default: "white")
#' @param high Color for positive residuals (default: "firebrick")
#' @param midpoint Center point for color scale (default: 0)
#' @param limits Range of residuals to display. If NULL, uses range of data.
#' @param name Legend title
#' @export
#' @examples
#' data(titanic)
#'
#' # Independence model with residual shading
#' ggplot(data = titanic) +
#'   geom_mosaic(aes(x = product(Class, Sex)), expected = "independence") +
#'   scale_fill_residual()
#'
#' # Custom colors
#' ggplot(data = titanic) +
#'   geom_mosaic(aes(x = product(Class, Sex)), expected = "independence") +
#'   scale_fill_residual(low = "blue", high = "red")
#'
#' # Custom limits to highlight strong deviations
#' ggplot(data = titanic) +
#'   geom_mosaic(aes(x = product(Class, Sex, Survived)),
#'               expected = ~ Class + Sex) +
#'   scale_fill_residual(limits = c(-4, 4))
scale_fill_residual <- function(...,
                                low = "darkblue",
                                mid = "white",
                                high = "darkred",
                                midpoint = 0,
                                limits = NULL,
                                name = "Pearson\nResidual") {
  ggplot2::scale_fill_gradient2(
    low = low,
    mid = mid,
    high = high,
    midpoint = midpoint,
    limits = limits,
    name = name,
    ...
  )
}

#' @rdname scale_fill_residual
#' @export
scale_fill_residuals <- scale_fill_residual  # Alias for plural form
