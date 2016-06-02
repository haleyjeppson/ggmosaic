#' @export
scale_type.product <- function(x) {
  cat("checking for type product\n")
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
ScaleContinuousProduct <- ggproto("ScaleContinuousProduct", ScaleContinuous,
                                   map = function(self, x, limits = self$get_limits()) {
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
