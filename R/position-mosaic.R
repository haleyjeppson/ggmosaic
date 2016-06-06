#' Don't adjust position
#'
#' @family position adjustments
#' @export
position_mosaic <- function() {
  PositionMosaic
}

#' @export
PositionMosaic <- ggplot2::ggproto(
  "PositionMosaic", ggplot2::Position,
  compute_layer = function(data, params, scales) {
                              browser()
                              data
                            },
  setup_data = function(self, data, params) {
    browser()
    data = remove_missing(data, FALSE,
                          c("x", "y", "ymin", "ymax", "xmin", "xmax"), name = "position_stack")

    if (is.null(data$ymax) && is.null(data$y)) {
      message("Missing y and ymax in position = 'stack'. ",
              "Maybe you want position = 'identity'?")
      return(data)
    }

    if (!is.null(data$ymin) && !all(data$ymin == 0))
      warning("Stacking not well defined when ymin != 0", call. = FALSE)

    data
  }
)


