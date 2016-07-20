#' @export
to_basic.GeomMosaic <- function (data, prestats_data, layout, params, p, ...)
{
  data <- subset(data, level==max(level))
  browser()
  x.vars<- data[ , grepl("x[0-9]", colnames(data))]
  x.values <- data.frame(sapply(x.vars, function(x) gsub("-", ": ", x)))
  x.values[] <- lapply(x.values, as.character)
  labs <- tidyr::unite_(x.values, col="labs", from=colnames(x.values)[], sep=" <br>")
  fill.vars <- c(gsub(".*<", "<", data$hovertext))
  all <- data.frame(labs, fill.vars)
  data$hovertext <- unlist(c(tidyr::unite_(all, col="hovertxt", from=colnames(all)[], sep="")))

  data$group <- seq_len(nrow(data))
  others <- data[!names(data) %in% c("xmin", "ymin", "xmax",
                                     "ymax")]
  data <- with(data, {
    rbind(cbind(x = xmin, y = ymin, others), cbind(x = xmin,
                                                   y = ymax, others), cbind(x = xmax, y = ymax, others),
          cbind(x = xmax, y = ymin, others))
  })

  prefix_class(data, "GeomPolygon")
}


 # to_basic.GeomRect <- getFromNamespace("to_basic.GeomRect", asNamespace("plotly"))


  prefix_class <- function(x, y) {
    structure(x, class = unique(c(y, class(x))))
  }

