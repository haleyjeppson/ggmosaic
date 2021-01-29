#' @rdname geom_mosaic
#' @inheritParams ggplot2::stat_identity
#' @section Computed variables:
#' \describe{
#' \item{x}{location of center of the rectangle}
#' \item{y}{location of center of the rectangle}
#' }
#' @export
stat_mosaic_text <- function(mapping = NULL, data = NULL, geom = "Text",
                        position = "identity", na.rm = FALSE,  divider = mosaic(),
                        show.legend = NA, inherit.aes = TRUE, offset = 0.01, ...)
{
  if (!is.null(mapping$y)) {
    stop("stat_mosaic() must not be used with a y aesthetic.", call. = FALSE)
  } else mapping$y <- structure(1L, class = "productlist")

  aes_x <- mapping$x
  if (!is.null(aes_x)) {
    aes_x <- rlang::eval_tidy(mapping$x)
    var_x <- paste0("x__", as.character(aes_x))
  }

  aes_fill <- mapping$fill
  var_fill <- ""
  if (!is.null(aes_fill)) {
    aes_fill <- rlang::quo_text(mapping$fill)
    var_fill <- paste0("x__fill__", aes_fill)
    if (aes_fill %in% as.character(aes_x)) {
      idx <- which(aes_x == aes_fill)
      var_x[idx] <- var_fill
    } else {
      mapping[[var_fill]] <- mapping$fill
    }
  }

  aes_alpha <- mapping$alpha
  var_alpha <- ""
  if (!is.null(aes_alpha)) {
    aes_alpha <- rlang::quo_text(mapping$alpha)
    var_alpha <- paste0("x__alpha__", aes_alpha)
    if (aes_alpha %in% as.character(aes_x)) {
      idx <- which(aes_x == aes_alpha)
      var_x[idx] <- var_alpha
    } else {
      mapping[[var_alpha]] <- mapping$alpha
    }
  }


  #  aes_x <- mapping$x
  if (!is.null(aes_x)) {
    mapping$x <- structure(1L, class = "productlist")

    for (i in seq_along(var_x)) {
      mapping[[var_x[i]]] <- aes_x[[i]]
    }
  }


  aes_conds <- mapping$conds
  if (!is.null(aes_conds)) {
    aes_conds <- rlang::eval_tidy(mapping$conds)
    mapping$conds <- structure(1L, class = "productlist")
    var_conds <- paste0("conds", seq_along(aes_conds), "__", as.character(aes_conds))
    for (i in seq_along(var_conds)) {
      mapping[[var_conds[i]]] <- aes_conds[[i]]
    }
  }
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatMosaicText,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    params = list(
      na.rm = na.rm,
      divider = divider,
      offset = offset,
      ...
    )
  )
}

#' Geom proto
#'
#' @format NULL
#' @usage NULL
#' @importFrom tidyr unite_
#' @export
StatMosaicText <- ggplot2::ggproto(
  "StatMosaicText", ggplot2::Stat,
  #required_aes = c("x"),
  non_missing_aes = "weight",

  setup_params = function(data, params) {
    #cat("setup_params from StatMosaic\n")
    #browser()
    # if (!is.null(data$y)) {
    #   stop("stat_mosaic() must not be used with a y aesthetic.", call. = FALSE)
    # }
    params
  },

  setup_data = function(data, params) {
    #cat("setup_data from StatMosaic\n")
    #browser()

    data
  },

  compute_panel = function(self, data, scales, na.rm=FALSE, divider, offset) {

    first_stage <- StatMosaic$compute_panel(data, scales, na.rm=FALSE, divider, offset)

     # if (all(is.na(first_stage$colour)))
       # first_stage$colour <- scales::alpha(first_stage$fill, first_stage$alpha) # regard alpha in colour determination

     # browser()
     sub <- subset(first_stage, level==max(first_stage$level))
       text <- subset(sub, .n > 0) # do not label the obs with weight 0
     text <- tidyr::nest(text, data = -label)

     text <-
       dplyr::mutate(
         text,
         coords = purrr::map(data, .f = function(d) {
           data.frame(
             x = (d$xmin + d$xmax)/2,
             y = (d$ymin + d$ymax)/2,
             #size = 2.88,
             angle = 0,
             hjust = 0.5,
             vjust = 0.5,
             alpha = NA,
             family = "",
             fontface = 1,
             lineheight = 1.2,
             dplyr::select(d, -any_of(c("x", "y", "alpha")))
           )
         })
       )

     text <- tidyr::unnest(text, coords)

     # sub$fill <- NA
     # sub$colour <- NA
     # sub$size <- sub$size/10

     text

  }
)

