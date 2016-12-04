#' @importFrom utils getFromNamespace
#' @importFrom plotly to_basic
#' @export
to_basic.GeomMosaic <- function (data, prestats_data, layout, params, p, ...) {
  seps <- get.separators()
  level <- NULL # visible binding
  data <- subset(data, level == max(level))
  data$hovertext <- gsub("\n", "<br>", data$label)
  data$hovertext <- gsub(seps[1], ": ", data$hovertext)
  data$hovertext <- paste0(data$hovertext, "<br>Frequency: ", data[[".wt"]])
  getFromNamespace("to_basic.GeomRect", asNamespace("plotly"))(data)
}
