#' Spine plots.
#'
#' @export
#'
#' @inheritParams ggplot2::layer
#' @param na.rm If \code{FALSE} (the default), removes missing values with a warning. If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{layer}. These are often aesthetics, used to set an aesthetic to a fixed value, like \code{color = 'red'} or \code{size = 3}. They may also be parameters to the paired geom/stat.
#' @examples
#' data(Titanic)
#' titanic <- as.data.frame(Titanic)
#' # ggplot(data=titanic) + geom_spine()
#' #df <- read.csv("inst/mosaic-rects.csv")
#' #ggplot() + geom_spine(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), data=df)

geom_spine <- function(mapping = NULL, data = NULL, stat = "spine",
                        position = "identity", na.rm = FALSE,
                        show.legend = NA, inherit.aes = FALSE, ...)
{
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSpine,
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
GeomSpine <- ggplot2::ggproto("GeomSpine", ggplot2::GeomRect,
                               default_aes = aes(colour = NA, fill = "grey35", size = 0.5, linetype = 1,
                                                 alpha = .4),

                               setup_data = function(data, params) {
                                 cat("setup_data in GeomSpine\n")
                                 #    data$x <- 1
                                 #    data$y <- 1
                                 browser()
                                 data
                               },
                               required_aes = c("xmin", "xmax", "ymin", "ymax"),

                               draw_group = function(data, panel_scales, coord) {
                                 cat("draw_group in GeomSpine\n")

                                 browser()
                                 #     ggplot2:::ggname("geom_lvplot", grobTree(
                                 #       outliers_grob,
                                 #       GeomRect$draw_panel(box, panel_scales, coord),
                                 #       GeomSegment$draw_panel(medians, panel_scales, coord)
                                 #     ))
                               },

                               draw_panel = function(self, data, panel_scales, coord) {
                                 browser()

                               #  panel_scales$x.major <- (data$xmax+data$xmin)/2
                                # panel_scales$x.major_source <- (data$xmax+data$xmin)/2

                               # panel_scales$x.range <- range(c(min(data$xmin), max(data$xmax)))

                                 browser()

                                 if (!coord$is_linear()) {
                                   aesthetics <- setdiff(
                                     names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
                                   )

                                   polys <- plyr::alply(data, 1, function(row) {
                                     poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
                                     aes <- as.data.frame(row[aesthetics],
                                                          stringsAsFactors = FALSE)[rep(1,5), ]

                                     GeomPolygon$draw_panel(cbind(poly, aes), panel_scales, coord)
                                   })

                                   ggplot2:::ggname("bar", do.call("grobTree", polys))
                                 } else {
                                   coords <- coord$transform(data, panel_scales)
                                   ggplot2:::ggname("geom_rect", rectGrob(
                                     coords$xmin, coords$ymax,
                                     width = coords$xmax - coords$xmin,
                                     height = coords$ymax - coords$ymin,
                                     default.units = "native",
                                     just = c("left", "top"),
                                     gp = gpar(
                                       col = coords$colour,
                                       fill = alpha(coords$fill, coords$alpha),
                                       lwd = coords$size * .pt,
                                       lty = coords$linetype,
                                       lineend = "butt"
                                     )
                                   ))
                                 }
                               },


                               draw_key = ggplot2::draw_key_rect
)

rect_to_poly <- function(xmin, xmax, ymin, ymax) {
  data.frame(
    y = c(ymax, ymax, ymin, ymin, ymax),
    x = c(xmin, xmax, xmax, xmin, xmin)
  )
}
