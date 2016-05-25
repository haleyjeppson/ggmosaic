
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


#' @rdname geom_mosaic
#' @export
stat_mosaic <- function(mapping = NULL, data = NULL, geom = "rect",
                        position = "identity", na.rm = TRUE, show.legend = NA,
                        inherit.aes = TRUE, ...)
{
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatMosaic,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
# scale_type.formula <- function(x) "formula"

#' @export
StatMosaic <- ggplot2::ggproto("StatMosaic", ggplot2::Stat,
                                required_aes = c("x"),
                               #  non_missing_aes = "weight",

                               setup_params = function(self, data, params, divider = mosaic(), cascade = 0,
                                                       scale_max = TRUE, na.rm = FALSE, levels = -1L,
                                                       ...) {
                                 cat("setup_params from StatMosaic\n")
                                browser()

                                 ## do i need to extract labels as the parameteres here? so that the scale is correct
                                 ##


                                # require("productplots")
                                #    params$formula = ~data$x
                                #    params$vars <- parse_product_formula(params$formula)
                                #    params$p <- length(c(params$vars$cond, params$vars$marg))
                                #
                                #    if (is.function(params$divider)) params$divider <- params$divider(params$p)
                                #    params$div_names <- params$divider
                                #    if (is.character(params$divider)) params$divider <- plyr::llply(params$divider, match.fun)
                                #    browser()
                                   params
                                 },


                               compute_panel= function(self, data, scales, #params,
                                                        divider = mosaic(), cascade = 0,
                                                        scale_max = TRUE, na.rm = FALSE, levels = -1L,
                                                        ...) {
                                 cat("compute_panel from StatMosaic\n")
                                  browser()

                                  require("productplots")
                                  x <- data$x
                                  formula <-  ~x

                                  #vars <- parse_product_formula(formula)
                                  #p <- length(c(vars$cond, vars$marg))
                                  #browser()

                                  #if (is.function(divider)) divider <- divider(p)
                                  #div_names <- divider
                                  #if (is.character(divider)) divider <- plyr::llply(divider, match.fun)

                                  res <- prodcalc(data, formula, "hspine", cascade, scale_max, na.rm = na.rm)
                                  if (!(length(levels) == 1 && is.na(levels))) {
                                    levels[levels < 0] <-  max(res$level) + 1 + levels[levels < 0]
                                    res <- res[res$level %in% levels, ]
                                  }
                         #         browser()

                                  data.frame(xmin=res$l, xmax=res$r, ymin=res$b, ymax=res$t, .wt=res$.wt, level=res$level) #, x=res$x)

                                 #    df  is data frame with data that has xmin, xmax, ymin, ymax
                               }
)
