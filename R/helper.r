scale_y_product <- function (df) {
  l <- b <- r <- NULL
  data <- df$data
  vars <- parse_product_formula(df$formula)
  col <- c(vars$marg, vars$cond)[grep("v", df$divider)]
  if (length(col) == 0) {
    breaks <- seq(0, 1, length = 5)
    scale_y_continuous("", breaks = breaks, labels = round(breaks,
                                                           2))
  }
  else {
    labels <- subset(data, (level = max(level)) & (l == 0))
    labels$pos <- with(labels, (b + t)/2)
 #   browser()
    labels$label <- sapply(1:nrow(labels), function(x) paste(unlist(labels[x,
                                                                          col]), collapse = ":"))
    ylabel <- paste(col, collapse = ":")
    scale_y_continuous(ylabel, breaks = labels$pos, labels = labels$label)
  }
}

scale_x_product <- function (df) {
 # browser()
  l <- b <- r <- NULL
  data <- df$data
  vars <- parse_product_formula(df$formula)
  col <- c(vars$marg, vars$cond)[grep("h", df$divider)]
  if (length(col) == 0) {
    breaks <- seq(0, 1, length = 5)
    scale_x_continuous("", breaks = breaks, labels = round(breaks,
                                                           2))
  }
  else {
    labels <- subset(data, (level = max(level)) & (b ==
                                                     0))
    labels$pos <- with(labels, (l + r)/2)
#    browser()
    labels$label <- sapply(1:nrow(labels), FUN = function(x) paste(unlist(labels[x,
                                                                          col]), collapse = ":"))
    xlabel <- paste(col, collapse = ":")
    scale_x_continuous(xlabel, breaks = labels$pos, labels = labels$label)
  }
}

lhs <- function (x) {
  stopifnot(is.call(x) || is.name(x))
  if (length(x) == 3)
    x[[2]]
}

rhs <- function (x) {
  stopifnot(is.call(x) || is.name(x))
  if (length(x) == 2) {
    x[[2]]
  }
  else if (length(x) == 3) {
    x[[3]]
  }
}

op <- function (x) {
  stopifnot(is.call(x) || is.name(x))
  if (length(x) == 3 || length(x) == 2)
    x[[1]]
}

variables <- function(string, operator) {
  if(is.language(string)) string <- as.character(string)[-1]
  if(is.symbol(string)) string <- as.character(string)
  if(!is.character(string)) {
    # browser()
    cat("which type of string ended up here?")
  }

  res <- unlist(strsplit(split=operator, fixed=TRUE, string))
  trimws(res)
}

parse_product_formula <- function(f) {
  #  browser()
  wt <- if ((is.call(f)) && length(f) == 3)
    all.vars(lhs(f)) # unchanged - weight variable is not part of the product statement
  else character()
  mc <- rhs(f)
  if (identical(op(mc), as.name("|"))) {
    cond <- variables(rhs(mc), '+')
    marg <- variables(lhs(mc), '+')
  }
  else {
    cond <- character()
    marg <- variables(mc, '+')
  }
  marg <- marg[marg != "."]
  list(wt = wt, marg = marg, cond = cond)
}
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

#' @rdname geom_mosaic_jitter
#' @inheritParams ggplot2::stat_identity
#' @section Computed variables:
#' \describe{
#' \item{xmin}{location of bottom left corner}
#' \item{xmax}{location of bottom right corner}
#' \item{ymin}{location of top left corner}
#' \item{ymax}{location of top right corner}
#' }
#' @export
stat_mosaic_jitter <- function(mapping = NULL, data = NULL, geom = "mosaic_jitter",
                               position = "identity", na.rm = FALSE,  divider = mosaic(),
                               show.legend = NA, inherit.aes = TRUE, offset = 0.01,
                               drop_level = FALSE, ...)
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
    stat = StatMosaicJitter,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
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
#' @importFrom tidyr unite_
#' @export
StatMosaicJitter <- ggplot2::ggproto(
  "StatMosaicJitter", ggplot2::Stat,
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

  compute_panel = function(self, data, scales, na.rm=FALSE, drop_level=FALSE, divider, offset) {
    #cat("compute_panel from StatMosaic\n")
    #browser()

    #    vars <- names(data)[grep("x[0-9]+__", names(data))]
    vars <- names(data)[grep("x__", names(data))]
    conds <- names(data)[grep("conds[0-9]+__", names(data))]


    if (length(vars) == 0) formula <- "1"
    else formula <-  paste(vars, collapse="+")



    formula <- paste("weight~", formula)

    if (length(conds) > 0) formula <- paste(formula, paste(conds, collapse="+"), sep="|")

    df <- data
    if (!in_data(df, "weight")) {
      df$weight <- 1
    }


    res <- prodcalc(df, formula=as.formula(formula),
                    divider = divider, cascade=0, scale_max = TRUE,
                    na.rm = na.rm, offset = offset)

    # browser()

    # consider 2nd weight for points
    if (in_data(df, "weight2")) {
      formula2 <- str_replace(formula, "weight", "weight2")
      res2 <- prodcalc(df, formula = as.formula(formula2), divider = divider,
                       cascade = 0, scale_max = TRUE, na.rm = na.rm, offset = offset)
      res$.n2 <- res2$.n
    }

    # browser()
    # need to set x variable - I'd rather set the scales here.
    prs <- parse_product_formula(as.formula(formula))
    p <- length(c(prs$marg, prs$cond))
    if (is.function(divider)) divider <- divider(p)

    # the level at which things are labelled could be made a parameter.
    # At the moment the deepest level is being labelled.
    dflist <- list(data=subset(res, level==max(res$level)), formula=as.formula(formula), divider=divider)
    scx <- productplots::scale_x_product(dflist)
    scy <- productplots::scale_y_product(dflist)


    # res is data frame that has xmin, xmax, ymin, ymax
    res <- dplyr::rename(res, xmin=l, xmax=r, ymin=b, ymax=t)
    # res <- subset(res, level==max(res$level))

    # export the variables with the data - terrible hack
    # res$x <- list(scale=scx)
    # if (!is.null(scales$y)) {
    #   # only set the y scale if it is a product scale, otherwise leave it alone
    #   if ("ScaleContinuousProduct" %in% class(scales$y))
    #     res$y <- list(scale=scy)
    # }
    # XXXX add label for res
    cols <- c(prs$marg, prs$cond)

    if (length(cols) > 1) {
      df <- res[,cols]
      df <- tidyr::unite_(df, "label", cols, sep="\n")

      res$label <- df$label
    } else res$label <- as.character(res[,cols])


    res$x <- list(scale=scx)
    if (!is.null(scales$y)) {
      # only set the y scale if it is a product scale, otherwise leave it alone
      if ("ScaleContinuousProduct" %in% class(scales$y))
        res$y <- list(scale=scy)
    }

    # merge res with data:
    # is there a fill/alpha/color variable?
    fill_idx <- grep("x__fill", names(data))
    if (length(fill_idx) > 0) {
      fill_res_idx <- grep("x__fill", names(res))
      res$fill <- res[[fill_res_idx]]
    }
    alpha_idx <- grep("x__alpha", names(data))
    if (length(alpha_idx) > 0) {
      alpha_res_idx <- grep("x__alpha", names(res))
      res$alpha <- res[[alpha_res_idx]]
    }
    colour_idx <- grep("x__colour", names(data))
    if (length(colour_idx) > 0) {
      colour_res_idx <- grep("x__colour", names(res)) # find what comes after __colour
      res$colour <- res[[colour_res_idx]]
    }

    res$group <- 1 # unique(data$group) # ignore group variable
    res$PANEL <- unique(data$PANEL)
    # browser()

    # generate points
    # consider 2nd weight for point
    if (in_data(res, ".n2")) {
      res$.n <- res$.n2
    }

    sub <- subset(res, level==max(res$level))
    if(drop_level) {
      ll <- subset(res, level==max(res$level)-1)
      sub <- dplyr::left_join(select(sub, -(xmin:ymax)), select(ll, contains("x__"), xmin:ymax, -contains("col")))
    }



    points <- subset(sub, sub$.n>=1)
    points <- tidyr::nest(points, data = -label)

    points <-
      dplyr::mutate(
        points,
        coords = purrr::map(data, .f = function(d) {
          data.frame(
            x = runif(d$.n, min = d$xmin, max = d$xmax),
            y = runif(d$.n, min = d$ymin, max = d$ymax),
            dplyr::select(d, -x, -y)
          )
        })
      )

    points <- tidyr::unnest(points, coords)
    # browser()

    points
  }
)

