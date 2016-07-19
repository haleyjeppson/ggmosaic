#' @importFrom plotly geom2trace
#' @export
geom2trace.GeomMosaic <- function(data, params, p) {
  #browser()
  data$y <- data$ymax - data$ymin
  # TODO: use xmin/xmax once plotly.js allows explicit bar widths
  # https://github.com/plotly/plotly.js/issues/80
  list(
    x = data$x,
    y = data$y,
    text = data$hovertext,
    key = data$key,
    type = "bar",
    marker = list(
      autocolorscale = FALSE,
      color = toRGB(
        plotly:::aes2plotly(data, params, "fill"),
        plotly:::aes2plotly(data, params, "alpha")
      ),
      line = list(
        width = plotly:::aes2plotly(data, params, "size"),
        color = plotly:::aes2plotly(data, params, "colour")
      )
    )
  )
}
