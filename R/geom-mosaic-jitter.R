#' Jittered dots in Mosaic plots.
#'
#' @export
#'
#' @description
#' A mosaic plat with jittered dots
#'
#' @inheritParams ggplot2::layer
#' @param divider Divider function. The default divider function is mosaic() which will use spines in alternating directions. The four options for partitioning:
#' \itemize{
#' \item \code{vspine} Vertical spine partition: width constant, height varies.
#' \item \code{hspine}  Horizontal spine partition: height constant, width varies.
#' \item \code{vbar} Vertical bar partition: height constant, width varies.
#' \item \code{hbar}  Horizontal bar partition: width constant, height varies.
#' }
#' @param offset Set the space between the first spine
#' @param drop_level Generate points for the max - 1 level
#' @param na.rm If \code{FALSE} (the default), removes missing values with a warning. If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{layer}. These are often aesthetics, used to set an aesthetic to a fixed value, like \code{color = 'red'} or \code{size = 3}. They may also be parameters to the paired geom/stat.
#' @examples
#' data(titanic)
#'
#' ggplot(data = titanic) +
#'   geom_mosaic(aes(x = product(Class), fill = Survived), alpha = 0.3) +
#'   geom_mosaic_jitter(aes(x = product(Class), color = Survived))
#'
#' ggplot(data = titanic) +
#'   geom_mosaic(aes(x = product(Class)), alpha = 0.1) +
#'   geom_mosaic_jitter(aes(x = product(Class), color = Survived), drop_level = TRUE)
#'
#' ggplot(data = titanic) +
#'   geom_mosaic(alpha = 0.3, aes(x = product(Class, Sex),  fill = Survived),
#'               divider = c("vspine", "hspine", "hspine")) +
#'   geom_mosaic_jitter(aes(x = product(Class, Sex), color = Survived),
#'               divider = c("vspine", "hspine", "hspine"))
#'
#'  ggplot(data = titanic) +
#'   geom_mosaic(alpha = 0.3, aes(x = product(Class), conds = product(Sex),  fill = Survived),
#'               divider = c("vspine", "hspine", "hspine")) +
#'   geom_mosaic_jitter(aes(x = product(Class), conds = product(Sex), fill = Survived),
#'               divider = c("vspine", "hspine", "hspine"))
geom_mosaic_jitter <- function(mapping = NULL, data = NULL, stat = "mosaic_jitter",
                               position = "identity", na.rm = FALSE,  divider = mosaic(),
                               offset = 0.01, drop_level = FALSE,
                               show.legend = NA, inherit.aes = FALSE, ...)
{
  if (!is.null(mapping$y)) {
    stop("stat_mosaic() must not be used with a y aesthetic.", call. = FALSE)
  } else mapping$y <- structure(1L, class = "productlist")

    #browser()

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

  aes_colour <- mapping$colour
  var_colour <- ""
  if (!is.null(aes_colour)) {
    aes_colour <- rlang::quo_text(mapping$colour)
    var_colour <- paste0("x__colour__", aes_colour)
    if (aes_colour %in% as.character(aes_x)) {
      idx <- which(aes_x == aes_colour)
      var_x[idx] <- var_colour
    } else {
      mapping[[var_colour]] <- mapping$colour
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
    stat = stat,
    geom = GeomMosaicJitter,
    position = position,
    show.legend = show.legend,
    check.aes = FALSE,
    inherit.aes = FALSE, # only FALSE to turn the warning off
    params = list(
      na.rm = na.rm,
      divider = divider,
      offset = offset,
      drop_level = drop_level,
      ...
    )
  )
}

#' Geom proto
#'
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom grid grobTree
#' @importFrom tidyr nest unnest
#' @importFrom dplyr mutate select
GeomMosaicJitter <- ggplot2::ggproto(
  "GeomMosaicJitter", ggplot2::Geom,
  setup_data = function(data, params) {
    #cat("setup_data in GeomMosaic\n")
    #browser()
    data
  },
  # required_aes = c("xmin", "xmax", "ymin", "ymax"),
  # default_aes = ggplot2::aes(width = 0.1, linetype = "solid", fontsize=5,
  #                            shape = 19, colour = NA,
  #                            size = 1, fill = "grey30", alpha = 1, stroke = 0.1,
  #                            linewidth=.1, weight = 1, x = NULL, y = NULL, conds = NULL),
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = aes(
    shape = 19, colour = "grey30", size = 1, fill = NA,
    alpha = NA, stroke = 0.5, linewidth=.1, weight = 1
  ),

  draw_panel = function(data, panel_scales, coord) {
    #cat("draw_panel in GeomMosaic\n")
    # browser()
    # if (all(is.na(data$colour)))
    #   data$colour <- scales::alpha(data$fill, data$alpha) # regard alpha in colour determination

    # sub <- data #subset(data, level==max(data$level))
    # points <- sub
    # points <- tidyr::nest(points, data = -label)
    #
    # points <-
    #   dplyr::mutate(
    #     points,
    #     coords = purrr::map(data, .f = function(d) {
    #       data.frame(
    #         x = runif(d$.n, min = d$xmin, max = d$xmax),
    #         y = runif(d$.n, min = d$ymin, max = d$ymax),
    #         dplyr::select(d, -x, -y)
    #       )
    #     })
    #   )

    # points <- tidyr::unnest(points, coords)

    # sub$fill <- NA
    # sub$size <- sub$size/10

      ggplot2:::ggname("geom_mosaic_jitter", grobTree(
      #GeomRect$draw_panel(sub, panel_scales, coord),
      GeomPoint$draw_panel(data, panel_scales, coord)
    ))
  },

  check_aesthetics = function(x, n) {
    #browser()
    ns <- vapply(x, length, numeric(1))
    good <- ns == 1L | ns == n


    if (all(good)) {
      return()
    }

    stop(
      "Aesthetics must be either length 1 or the same as the data (", n, "): ",
      paste(names(!good), collapse = ", "),
      call. = FALSE
    )
  },

  draw_key = ggplot2::draw_key_point
)



