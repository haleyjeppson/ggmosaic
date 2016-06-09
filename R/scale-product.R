product_breaks <- function() {
    function(x) {
      cat(" in product_breaks\n")
    #  browser()
      unique(x)
    }
}

product_labels <- function() {
  function(x) {
    cat(" in product_labels\n")
  #  browser()
    unique(x)
  }
}


#' @export
scale_type.product <- function(x) {
  cat("checking for type product\n")
  #browser()
  "product"
}

#' Product scales for mosaic plots
#'
#' product scales are especially introduced for use with mosaic plots: they are a hybrid of
#' continuous and discrete scales.
#' @inheritParams ggplot2::scale_continuous
#' @name scale_product
NULL

#' @rdname scale_product
#' @importFrom ggplot2 waiver
#' @export
scale_x_product <- function(name = waiver(), breaks = product_breaks(),
                               minor_breaks = NULL, labels = product_labels(),
                               limits = NULL, expand = waiver(), oob = scales:::censor,
                               na.value = NA_real_, trans = "identity") {
#  browser()
  sc <- ggplot2::continuous_scale(
    c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper"),
    "position_c", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = "none"
  )

  # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- ScaleContinuousProduct
  class(sc) <- class(ScaleContinuousProduct)

  sc
}

#' @rdname scale_product
#' @importFrom ggplot2 waiver
#' @export
scale_y_product <- function(name = waiver(), breaks = waiver(),
                               minor_breaks = waiver(), labels = waiver(),
                               limits = NULL, expand = waiver(), oob = scales:::censor,
                               na.value = NA_real_, trans = "identity") {
  sc <- ggplot2::continuous_scale(
    c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper"),
    "position_c", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = "none"
  )

  # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
  # object should in the first place be created with the correct parent.
  sc$super <- ScaleContinuousProduct
  class(sc) <- class(ScaleContinuousProduct)

  sc
}


#' @export
ScaleContinuousProduct <- ggproto(
  "ScaleContinuousProduct", ScaleContinuous,
  # Position aesthetics don't map, because the coordinate system takes
  # care of it. But they do need to be made in to doubles, so stat methods
  # can tell the difference between continuous and discrete data.
  train =function(self, x) {
    cat("train in ScaleContinuousProduct\n")
    if (is.list(x)) {
      x <- x[[1]]
      if ("Scale" %in% class(x)) {
        # re-assign the scale values now that we have the information
        self$breaks <- x$breaks
        self$labels <- x$labels
        # there are some duplicates and NAs that should be removed
     #   browser()
        return()
      }
    }
    if (is.discrete(x)) {
      self$range$train(x=c(0,1))
      return()
    }
    self$range$train(x)
  },
  map = function(self, x, limits = self$get_limits()) {
    cat("map in ScaleContinuousProduct\n")
  #  browser()
    if (is.discrete(x)) return(x)
    if (is.list(x)) return(0) # need a number
    scaled <- as.numeric(self$oob(x, limits))
    ifelse(!is.na(scaled), scaled, self$na.value)
  },
  dimension = function(self, expand = c(0, 0)) {
    cat("dimension in ScaleContinuousProduct\n")
    c(-0.05,1.05)
  }
)

