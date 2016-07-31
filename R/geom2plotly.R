#' @export
to_basic.GeomMosaic <- function (data, prestats_data, layout, params, p, ...) {
  seps <- get.separators()
  data <- subset(data, level == max(level))
  data$hovertext <- gsub("\n", "<br>", data$label)
  data$hovertext <- gsub(seps[1], ": ", data$hovertext)
  data$hovertext <- paste0(data$hovertext, "<br>Frequency: ", data[[".wt"]])
  getFromNamespace("to_basic.GeomRect", "plotly")(data)
}

prefix_class <- function(x, y) {
  structure(x, class = unique(c(y, class(x))))
}
