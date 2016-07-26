#' @export
to_basic.GeomMosaic <- function (data, prestats_data, layout, params, p, ...) {
  data <- subset(data, level == max(level))
  data$hovertext <- gsub("\n", "<br>", data$label)
  data$hovertext <- gsub("-", ": ", data$hovertext)
  data$hovertext <- paste0(data$hovertext, "<br>Weight: ", data[[".wt"]])
  to_basic.GeomRect(data)
}
