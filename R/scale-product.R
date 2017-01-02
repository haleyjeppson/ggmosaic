is.formula <- function (x) inherits(x, "formula")

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

product_breaks <- function() {
    function(x) {
   #   cat(" in product_breaks\n")
      #browser()
      unique(x)
    }
}

product_labels <- function() {
  function(x) {
 #   cat(" in product_labels\n")
    #browser()
    unique(x)
  }
}
is.waive <- getFromNamespace("is.waive", "ggplot2")

#' Helper function that ggplot2 needs for determining scales on x and y
#'
#' @param x variable under consideration
#' @return character string "product"
#' @export
scale_type.product <- function(x) {
 # cat("checking for type product\n")
  #browser()
  "product"
}

#' Helper function that ggplot2 needs for determining scales on x and y
#'
#' @param x variable under consideration
#' @return character string "productlist"
#' @export
scale_type.productlist <- function(x) {
# cat("checking for type productlist\n")
  #browser()
  "productlist"
}

#' @export
length.productlist <- function(x) {
#  cat("length.productlist\n")
  y <- x
  class(y) <- "list"
  length(y[[1]])
}

#' @export
as.data.frame.productlist <- function(x, row.names = NULL, optional = FALSE, ...) {
#  cat("as.data.frame.productlist \n")
  y <- x
  class(y) <- "list"
  res <- as.data.frame(y)
  names(res) <- names(x)
  res
}

#' Product scales for mosaic plots
#'
#' product scales are especially introduced for use with mosaic plots: they are a hybrid of
#' continuous and discrete scales.
#' @inheritParams ggplot2::continuous_scale
#' @name scale_productlist
NULL

# #' Product scales for mosaic plots
# #'
# #' product scales are especially introduced for use with mosaic plots: they are a hybrid of
# #' continuous and discrete scales.
# #' @inheritParams ggplot2::continuous_scale
# #' @name scale_product
# NULL

#' @rdname scale_productlist
#' @importFrom ggplot2 waiver
#' @param sec.axis specify a secondary axis
#' @export
scale_x_productlist <- function(name = waiver(), breaks = product_breaks(),
                            minor_breaks = NULL, labels = product_labels(),
                            limits = NULL, expand = waiver(), oob = scales:::censor,
                            na.value = NA_real_, trans = "identity",
                            position = "bottom", sec.axis = waiver()) {
  #browser()
  sc <- ggplot2::continuous_scale(
    c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper"),
    "position_c", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = "none", position = position, super = ScaleContinuousProduct
  )

  if (!is.waive(sec.axis)) {
    if (is.formula(sec.axis)) sec.axis <- ggplot2::sec_axis(sec.axis)
    is.sec_axis = getFromNamespace("is.sec_axis", "ggplot2")
    if (is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")
    sc$secondary.axis <- sec.axis
  }
  sc
}

# #' @rdname scale_product
# #' @importFrom ggplot2 waiver
# #' @param sec.axis specify a secondary axis
# #' @export
# scale_x_product <- function(name = waiver(), breaks = product_breaks(),
#                             minor_breaks = NULL, labels = product_labels(),
#                             limits = NULL, expand = waiver(), oob = scales:::censor,
#                             na.value = NA_real_, trans = "identity",
#                             position = "bottom", sec.axis = waiver()) {
#   #browser()
#   sc <- ggplot2::continuous_scale(
#     c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper"),
#     "position_c", identity, name = name, breaks = breaks,
#     minor_breaks = minor_breaks, labels = labels, limits = limits,
#     expand = expand, oob = oob, na.value = na.value, trans = trans,
#     guide = "none", position = position, super = ScaleContinuousProduct
#   )
#
#   if (!is.waive(sec.axis)) {
#     if (is.formula(sec.axis)) sec.axis <- ggplot2::sec_axis(sec.axis)
#     is.sec_axis = getFromNamespace("is.sec_axis", "ggplot2")
#     if (is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")
#     sc$secondary.axis <- sec.axis
#   }
#   sc
# }

# #' @rdname scale_product
# #' @importFrom ggplot2 waiver
# #' @export
# scale_y_product <- function(name = waiver(), breaks = waiver(),
#                                minor_breaks = waiver(), labels = waiver(),
#                                limits = NULL, expand = waiver(), oob = scales:::censor,
#                                na.value = NA_real_, trans = "identity",
#                             position = "left", sec.axis = waiver()) {
#   sc <- ggplot2::continuous_scale(
#     c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper"),
#     "position_c", identity, name = name, breaks = breaks,
#     minor_breaks = minor_breaks, labels = labels, limits = limits,
#     expand = expand, oob = oob, na.value = na.value, trans = trans,
#     guide = "none", position = position, super = ScaleContinuousProduct
#   )
#
#   if (!is.waive(sec.axis)) {
#     if (is.formula(sec.axis)) sec.axis <- ggplot2::sec_axis(sec.axis)
#     is.sec_axis = getFromNamespace("is.sec_axis", "ggplot2")
#     if (is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")
#     sc$secondary.axis <- sec.axis
#   }
#   sc
# }


#' @rdname scale_productlist
#' @export
ScaleContinuousProduct <- ggproto(
  "ScaleContinuousProduct", ScaleContinuousPosition,
  train =function(self, x) {
    #cat("train in ScaleContinuousProduct\n")
    #cat("class of variable: ")
    #cat(class(x))
    if (is.list(x)) {
      x <- x[[1]]
      if ("Scale" %in% class(x)) {
        # re-assign the scale values now that we have the information - but only if necessary
        if (is.function(self$breaks)) self$breaks <- x$breaks
        if (is.function(self$labels)) self$labels <- x$labels
        #browser()
        #cat("\n")
        return()
      }
    }
    if (is.discrete(x)) {
      self$range$train(x=c(0,1))
      #cat("\n")
      return()
    }
    self$range$train(x)
    #cat("\n")
  },
  map = function(self, x, limits = self$get_limits()) {
    #cat("map in ScaleContinuousProduct\n")
    #browser()
    if (is.discrete(x)) return(x)
    if (is.list(x)) return(0) # need a number
    scaled <- as.numeric(self$oob(x, limits))
    ifelse(!is.na(scaled), scaled, self$na.value)
  },
  dimension = function(self, expand = c(0, 0)) {
    #cat("dimension in ScaleContinuousProduct\n")
    c(-0.05,1.05)
  }
)

