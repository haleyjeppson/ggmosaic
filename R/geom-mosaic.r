#' Mosaic plots.
#'
#' @examples
#' data(Titanic)
#' titanic <- as.data.frame(Titanic)
#' # ggplot(data=titanic) + geom_mosaic()
#' df <- read.csv("inst/mosaic-rects.csv")
#' ggplot() + geom_mosaic(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), data=df)

geom_mosaic <- function(mapping = NULL, data = NULL, stat = "mosaic",
  position = "dodge", na.rm = FALSE,
  show.legend = NA, inherit.aes = FALSE, ...)
{
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMosaic,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @importFrom grid grobTree
GeomMosaic <- ggplot2::ggproto("GeomMosaic", ggplot2::Geom,
  setup_data = function(data, params) {
    cat("setup_data in GeomMosaic\n")
#    data$x <- 1
#    data$y <- 1
    data
  },
  required_aes = c("xmin", "xmax", "ymin", "ymax"),

  draw_group = function(data, panel_scales, coord) {
    cat("draw_group in GeomMosaic\n")

    browser()
#     ggplot2:::ggname("geom_lvplot", grobTree(
#       outliers_grob,
#       GeomRect$draw_panel(box, panel_scales, coord),
#       GeomSegment$draw_panel(medians, panel_scales, coord)
#     ))
  },

  draw_key = ggplot2::draw_key_rect
)
