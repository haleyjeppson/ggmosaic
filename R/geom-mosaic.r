#' Mosaic plots.
#'
#' @examples
#' data(Titanic)
#' titanic <- as.data.frame(Titanic)
#' # library(plyr)
#' # ggplot(data=titanic) + geom_mosaic(aes(weight=Freq))
#' # ggplot(data=titanic) + geom_mosaic(aes(weight=Freq, vars=list(Class, Survived))) # only works with modified check_aesthetics
#' ggplot(data=titanic) + geom_mosaic(aes(weight=Freq, vars=interaction(Class, Survived), group=1))
#' ggplot(data=titanic) + geom_mosaic(aes(weight=Freq, vars=interaction(Class, Survived), group=1, fill=Age))
#' # doing the right thing, but we need labelling to make it less confusing
#' ggplot(data=titanic) + geom_mosaic(aes(weight=Freq, vars=interaction(Class, Survived), conds = Age, group=1))
#'
#' df <- read.csv("inst/mosaic-rects.csv")
#' ggplot() + geom_mosaic(aes(x = xmin, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), data=df)

geom_mosaic <- function(mapping = NULL, data = NULL, stat = "mosaic",
  position = "identity", na.rm = FALSE,
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
  default_aes = ggplot2::aes(width = 0.75, linetype = "solid", fontsize=5,
                           shape = 19, colour = NA,
                           size = 1, fill = "grey30", alpha = .8, stroke = 0.5,
                           linewidth=.25),

  draw_group = function(data, panel_scales, coord) {
    cat("draw_group in GeomMosaic\n")

    browser()
#     ggplot2:::ggname("geom_lvplot", grobTree(
#       outliers_grob,
#       GeomRect$draw_panel(box, panel_scales, coord),
#       GeomSegment$draw_panel(medians, panel_scales, coord)
#     ))
    GeomRect$draw_panel(subset(data, level==max(data$level)), panel_scales, coord)
  },

check_aesthetics = function(x, n) {
  ns <- vapply(x, length, numeric(1))
  good <- ns == 1L | ns == n

  if (all(good)) {
    return()
  }
  browser()
  stop(
    "Aesthetics must be either length 1 or the same as the data (", n, "): ",
    paste(names(!good), collapse = ", "),
    call. = FALSE
  )
},

  draw_key = ggplot2::draw_key_rect
)
