#' Mosaic plots.
#'
#' @examples
#' data(Titanic)
#' titanic <- as.data.frame(Titanic)
#' # ggplot(data=titanic) + geom_mosaic()
#' df <- read.csv("inst/mosaic-rects.csv")
#' ggplot() + geom_mosaic(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), data=df)

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

                               draw_group = function(data, panel_scales, coord) {
                                 cat("draw_group in GeomMosaic\n")

                                # browser()
                                 #     ggplot2:::ggname("geom_lvplot", grobTree(
                                 #       outliers_grob,
                                 #       GeomRect$draw_panel(box, panel_scales, coord),
                                 #       GeomSegment$draw_panel(medians, panel_scales, coord)
                                 #     ))
                               },

                               draw_panel = function(self, data, panel_scales, coord) {
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

                                   ggplot2::ggname("bar", do.call("grobTree", polys))
                                 } else {
                                   coords <- coord$transform(data, panel_scales)
                                   ggplot2::ggname("geom_rect", rectGrob(
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
