#' Theme for mosaic plots
#'
#' Themes set the general aspect of the plot such as the colour of the
#' background, gridlines, the size and colour of fonts.
#' \code{theme_mosaic} provides access to the regular ggplot2 theme, but removes any
#' background, most of the gridlines, and ensures an aspect ratio of 1 for better
#' viewing of the mosaics.
#'
#' @param base_size base font size
#' @param base_family base font family
#'
#' @examples
#' library(ggmosaic)
#' data(happy)
#' ggplot(data = happy) +
#'   geom_mosaic(aes(weight=wtssall, x=product(health), fill=happy), na.rm=TRUE) +
#'   theme_mosaic()
#'
#' @name theme_mosaic
NULL
#' @export
#' @import ggplot2
theme_mosaic <- function (base_size = 11, base_family = "")
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      aspect.ratio = 1
    )
}
