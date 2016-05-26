
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


#' @rdname geom_spine
#' @export
stat_spine <- function(mapping = NULL, data = NULL, geom = "rect",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...)
{
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatSpine,
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
StatSpine <- ggplot2::ggproto("StatSpine", ggplot2::Stat,
                                required_aes = c("z"),
                               #  non_missing_aes = "weight",

                               setup_params = function(self, data, params, divider = mosaic(), cascade = 0,
                                                       scale_max = TRUE, na.rm = FALSE, levels = -1L,
                                                       ...) {
                                 cat("setup_params from StatSpine\n")


                                 params
                                 },


                               compute_panel= function(self, data, scales, #params,
                                                        divider = mosaic(), cascade = 0,
                                                        scale_max = TRUE, na.rm = FALSE, levels = -1L,
                                                        ...) {
                                 cat("compute_panel from StatSpine\n")
                                 browser()

                                 require("productplots")
                                 z <- data$z
                                 formula <-  ~z

                                 vars <- parse_product_formula(formula)
                                 p <- length(c(vars$cond, vars$marg))
                                 browser()

                                 if (is.function(divider)) divider <- divider(p)
                                 div_names <- divider
                                 if (is.character(divider)) divider <- plyr::llply(divider, match.fun)

                                 res <- prodcalc(data, formula, divider, cascade, scale_max, na.rm = na.rm)
                                 if (!(length(levels) == 1 && is.na(levels))) {
                                   levels[levels < 0] <-  max(res$level) + 1 + levels[levels < 0]
                                   res <- res[res$level %in% levels, ]
                                 }
                         #         browser()

                                 data.frame(xmin=res$l, xmax=res$r, ymin=res$b, ymax=res$t, .wt=res$.wt, level=res$level, z=res$z)

                                 #    df  is data frame with data that has xmin, xmax, ymin, ymax
                               }
)
