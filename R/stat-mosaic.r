

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


#' @rdname geom_mosaic
#' @export
stat_mosaic <- function(mapping = NULL, data = NULL, geom = "mosaic",
  position = "dodge", na.rm = TRUE, show.legend = NA,
  inherit.aes = TRUE, ...)
{
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatMosaic,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
StatMosaic <- ggplot2::ggproto("StatMosaic", ggplot2::Stat,
#  required_aes = c("x", "y"),
#  non_missing_aes = "weight",

  setup_params = function(data, params) {
    cat("setup_params from StatMosaic\n")
    params
  },

  compute_group = function(data, scales) {
    cat("compute_groups from StatMosaic\n")

#    df  is data frame with data that has xmin, xmax, ymin, ymax
    data
  }
)
