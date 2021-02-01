#' Labeling for Mosaic plots.
#'
#' @export
#'
#' @description
#' A mosaic plot with text or labels
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
#' @param na.rm If \code{FALSE} (the default), removes missing values with a warning. If \code{TRUE} silently removes missing values.
#' @param as.label Show as a ggplot label (box with round corners)
#' @param repel Use ggrepel wo labels don't overlap
#' @param repel_params List of ggrepel parameters (e.g. list(point.padding = 0))
#' @param check_overlap If `TRUE`, text that overlaps previous text in the
#'   same layer will not be plotted. `check_overlap` happens at draw time and in
#'   the order of the data. Therefore data should be arranged by the label
#'   column before calling `geom_label()` or `geom_text()`.
#' @param ... other arguments passed on to \code{layer}. These are often aesthetics, used to set an aesthetic to a fixed value, like \code{color = 'red'} or \code{size = 3}. They may also be parameters to the paired geom/stat.
#' @examples
#' data(titanic)
#'
#' ggplot(data = titanic) +
#'   geom_mosaic(aes(x = product(Class), fill = Survived)) +
#'   geom_mosaic_text(aes(x = product(Class), fill = Survived))
#'
#' ggplot(data = titanic) +
#'   geom_mosaic(aes(x = product(Class, Sex),  fill = Survived),
#'               divider = c("vspine", "hspine", "hspine")) +
#'   geom_mosaic_text(aes(x = product(Class, Sex), fill = Survived),
#'               divider = c("vspine", "hspine", "hspine"), size = 2)
#'
#' ggplot(data = happy) +
#'   geom_mosaic(aes(x = product(health), fill = happy), na.rm = TRUE, show.legend = FALSE) +
#'   geom_mosaic_text(aes(x = product(happy, health)), na.rm = TRUE)
#'
#' # avoid overlapping text
#' ggplot(data = happy) +
#'   geom_mosaic(aes(x = product(health), fill = happy), na.rm = TRUE, show.legend = FALSE) +
#'   geom_mosaic_text(aes(x = product(happy, health)), na.rm = TRUE, check_overlap = TRUE)
#'
#' # or use ggrepel
#' ggplot(data = happy) +
#'   geom_mosaic(aes(x = product(health), fill = happy), na.rm = TRUE, show.legend = FALSE) +
#'   geom_mosaic_text(aes(x = product(happy, health)), na.rm = TRUE, repel = TRUE)
#'
#' # and as a label
#' ggplot(data = happy) +
#'   geom_mosaic(aes(x = product(health), fill = happy), na.rm = TRUE, show.legend = FALSE) +
#'   geom_mosaic_text(aes(x = product(happy, health)), na.rm = TRUE, repel = TRUE, as.label=TRUE)
#'
geom_mosaic_text <- function(mapping = NULL, data = NULL, stat = "mosaic",
                             position = "identity", na.rm = FALSE,  divider = mosaic(), offset = 0.01,
                             show.legend = NA, inherit.aes = FALSE, as.label = FALSE, repel = FALSE,
                             repel_params = NULL, check_overlap = FALSE,
                             ...)
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
    geom = GeomMosaicText,
    position = position,
    show.legend = show.legend,
    check.aes = FALSE,
    inherit.aes = FALSE, # only FALSE to turn the warning off
    params = list(
      na.rm = na.rm,
      divider = divider,
      offset = offset,
      as.label = as.label,
      repel = repel,
      repel_params = repel_params,
      check_overlap = check_overlap,
      # repel_params = repel_params,
      # point.padding = 0,
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
#' @importFrom ggrepel GeomTextRepel
#' @importFrom ggrepel GeomLabelRepel
GeomMosaicText <- ggplot2::ggproto(
  "GeomMosaicText", ggplot2::Geom,
  setup_data = function(data, params) {
    #cat("setup_data in GeomMosaic\n")
    #browser()
    data
  },
  required_aes = c("xmin", "xmax", "ymin", "ymax"),
  default_aes = ggplot2::aes(width = 0.1, linetype = "solid", size=2.7,
                             shape = 19, colour = "black",
                             fill = "white", alpha = 1, stroke = 0.1,
                             linewidth=.1, weight = 1, x = NULL, y = NULL, conds = NULL,
                             point.size = NA,
                             segment.linetype = 1, segment.colour = NULL, segment.size = 0.5, segment.alpha = NULL,
                             segment.curvature = 0, segment.angle = 90, segment.ncp = 1,
                             segment.shape = 0.5, segment.square = TRUE, segment.squareShape = 1,
                             segment.inflect = FALSE, segment.debug = FALSE, bg.colour = NA, bg.r = 0.1

  ),
  draw_panel = function(data, panel_scales, coord, as.label, repel, repel_params, check_overlap = FALSE) {
    #cat("draw_panel in GeomMosaic\n")
    if (all(is.na(data$colour)))
      data$colour <- scales::alpha(data$fill, data$alpha) # regard alpha in colour determination

    sub <- subset(data, level==max(data$level))
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

    sub$fill <- NA
    sub$colour <- NA
    sub$size <- sub$size/10

    if(!repel) {
      if(!as.label) {
        GeomChosen <- GeomText
        ggplot2:::ggname("geom_mosaic_text", grobTree(
          GeomRect$draw_panel(sub, panel_scales, coord),
          GeomChosen$draw_panel(text, panel_scales, coord, check_overlap = check_overlap)
        ))
      } else if(as.label) {
        GeomChosen <- GeomLabel
        ggplot2:::ggname("geom_mosaic_text", grobTree(
          GeomRect$draw_panel(sub, panel_scales, coord),
          rlang::exec(GeomChosen$draw_panel, text, panel_scales, coord)
        ))
      }
    } else {
      if(!as.label) {
        GeomChosen <- GeomTextRepel
      } else if(as.label) {
        GeomChosen <- GeomLabelRepel
      }
      ggplot2:::ggname("geom_mosaic_text", grobTree(
        GeomRect$draw_panel(sub, panel_scales, coord),
        # GeomChosen$draw_panel(text, panel_scales, coord, !!!repel_params)
        rlang::exec(GeomChosen$draw_panel, text, panel_scales, coord, !!!repel_params)
      ))
    }


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

  draw_key = ggplot2::draw_key_rect
)



