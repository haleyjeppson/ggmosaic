
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

in_data <- function(data, variable) {
  length(intersect(names(data), variable)) > 0
}

#parse_product_formula <- getFromNamespace("parse_product_formula", "productplots")

