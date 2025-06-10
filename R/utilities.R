
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

in_data <- function(data, variable) {
  length(intersect(names(data), variable)) > 0
}

parse_product_formula <- getFromNamespace("parse_product_formula", "productplots")

#' Wrapper for a list
#'
#' @param ... Unquoted variables going into the product plot.
#' @export
#' @examples
#' data(titanic)
#' ggplot(data = titanic) +
#'   geom_mosaic(aes(x = product(Survived, Class), fill = Survived))
product <- function(...) {
  rlang::exprs(...)
}

is.formula <- function (x) inherits(x, "formula")

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

product_names <- function() {
  function(x) {
    #cat(" in product_breaks\n")
    #browser()
    unique(x)
  }
}

product_breaks <- function() {
  function(x) {
    #cat(" in product_breaks\n")
    #browser()
    unique(x)
  }
}

product_labels <- function() {
  function(x) {
    #cat(" in product_labels\n")
    #browser()

    unique(x)
  }
}

is.waive <- function(x) inherits(x, "waiver")




## copied from ggplot2
with_seed_null <- function(seed, code) {
  if (is.null(seed)) {
    code
  } else {
    withr::with_seed(seed, code)
  }
}



