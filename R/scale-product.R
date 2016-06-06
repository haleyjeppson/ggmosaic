#' @rdname scale_product
#' @export
#'
#' @param x an `R` object
scale_type.product <- function(x) {
  cat("checking for type product\n")
  browser()
  "product"
}

#' Position scale for product variables
#'
#' Use \code{scale_*_product} with \code{product} variables.
#' Even though the breaks might suggest a discrete variable
#' it is probably easier to think of this scale as continuous.
#'
#' @name scale_product
#' @inheritParams ggplot2::continuous_scale
#' @seealso \code{\link{scale_continuous}} for continuous position scales.
#' @examples
#' last_month <- Sys.Date() - 0:29
#' df <- data.frame(
#'   date = last_month,
#'   price = runif(30)
#' )
#' base <- ggplot(df, aes(date, price)) +
#'   geom_line()
#'
NULL



#' @rdname scale_product
#' @export
scale_x_product <- function(name = waiver(),
                         breaks = waiver(),
                         labels = waiver(),
                         minor_breaks = waiver(),
                         limits = NULL, expand = waiver()) {
browser()
  scale_product(c("x", "xmin", "xmax", "xend"), "product",
                 name = name,
                 breaks = breaks,
                 labels = labels,
                 minor_breaks = minor_breaks,
                 limits = limits, expand = expand
  )
}

#' @rdname scale_product
#' @export
scale_y_product <- function(name = waiver(),
                            breaks = waiver(),
                            labels = waiver(),
                            minor_breaks = waiver(),
                            limits = NULL, expand = waiver()) {

  scale_product(c("y", "ymin", "ymax", "yend"), "product",
                name = name,
                breaks = breaks,
                labels = labels,
                minor_breaks = minor_breaks,
                limits = limits, expand = expand
  )
}


#' @rdname scale_product
#' @export
ScaleDiscreteProduct <- ggproto(
  "ScaleDiscreteProduct", ScaleDiscrete,
  map = function(self, x, limits = self$get_limits()) {
    cat("in map of ScaleDiscreteProduct\n")
    self$oob(x, limits)
  }
)


# base class for scale_{xy}_product
scale_product <- function(aesthetics, expand = waiver(), breaks = pretty_breaks(),
                       minor_breaks = waiver(), ...) {

  if (is.character(breaks)) {
    breaks_str <- breaks
    breaks <- breaks_str
  }

  if (is.character(minor_breaks)) {
    mbreaks_str <- minor_breaks
    minor_breaks <- mbreaks_str
  }
browser()
  continuous_scale(aesthetics, "product", identity, breaks = breaks,
                   minor_breaks = minor_breaks, guide = "none", expand = expand,
                   trans = "identity", ...)
}


# The discrete position scale maintains two separate ranges - one for
# continuous data and one for discrete data.  This complicates training and
# mapping, but makes it possible to place objects at non-integer positions,
# as is necessary for jittering etc.

#' #' @rdname ggplot2-ggproto
#' #' @format NULL
#' #' @usage NULL
#' #' @export
#' ScaleDiscretePosition <- ggproto(
#'   "ScaleDiscretePosition", ScaleDiscrete,
#'
#'                                  train = function(self, x) {
#'                                    if (is.discrete(x)) {
#'                                      self$range$train(x, drop = self$drop)
#'                                    } else {
#'                                      self$range_c$train(x)
#'                                    }
#'                                  },
#'
#'                                  get_limits = function(self) {
#'                                    if (self$is_empty()) return(c(0, 1))
#'                                    self$limits %||% self$range$range %||% integer()
#'                                  },
#'
#'                                  is_empty = function(self) {
#'                                    is.null(self$range$range) && is.null(self$limits) && is.null(self$range_c$range)
#'                                  },
#'
#'                                  reset = function(self) {
#'                                    # Can't reset discrete scale because no way to recover values
#'                                    self$range_c$reset()
#'                                  },
#'
#'                                  map = function(self, x, limits = self$get_limits()) {
#'                                    if (is.discrete(x)) {
#'                                      seq_along(limits)[match(as.character(x), limits)]
#'                                    } else {
#'                                      x
#'                                    }
#'                                  },
#'
#'                                  dimension = function(self, expand = c(0, 0)) {
#'                                    c_range <- self$range_c$range
#'                                    d_range <- self$range$range
#'
#'                                    if (self$is_empty()) {
#'                                      c(0, 1)
#'                                    } else if (is.null(d_range)) { # only continuous
#'                                      expand_range(c_range, expand[1], 0 , 1)
#'                                    } else if (is.null(c_range)) { # only discrete
#'                                      expand_range(c(1, length(d_range)), 0, expand[2], 1)
#'                                    } else { # both
#'                                      range(
#'                                        expand_range(c_range, expand[1], 0 , 1),
#'                                        expand_range(c(1, length(d_range)), 0, expand[2], 1)
#'                                      )
#'                                    }
#'                                  },
#'
#'                                  clone = function(self) {
#'                                    new <- ggproto(NULL, self)
#'                                    new$range <- discrete_range()
#'                                    new$range_c <- continuous_range()
#'                                    new
#'                                  }
#' )
#'
