
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

in_data <- function(data, variable) {
  length(intersect(names(data), variable)) > 0
}



#' Wrapper for a list
#'
#' @param x name of the variable going into the product plot.
#' @param ...  arbitrarily many additional variables.
#' @export
#' @examples
#' data(Titanic)
#' titanic <- as.data.frame(Titanic)
#' titanic$Survived <- factor(titanic$Survived, levels=c("Yes", "No"))
#' ggplot(data=titanic) +
#'   geom_mosaic(aes(weight=Freq, x=product(Survived, Class), fill=Survived))

product <- function(x, ...) {
#  browser()
  # interaction doesn't deal with missing values correctly
  vars <- list(x, ...)
  varnames <- as.character(match.call()[-1])
  vars <- lapply(1:length(varnames), function(k) {
    if (!is.factor(vars[[k]])) vars[[k]] <- factor(vars[[k]])
#    levels(vars[[k]]) <- paste(varnames[k], levels(vars[[k]]), sep=":")
    vars[[k]]
  })
  names(vars) <- varnames

  class(vars) <- "productlist"
  vars
}



#' @rdname geom_mosaic
#' @inheritParams ggplot2::stat_identity
#' @section Computed variables:
#' \describe{
#' \item{xmin}{location of bottom left corner}
#' \item{xmax}{location of bottom right corner}
#' \item{ymin}{location of top left corner}
#' \item{ymax}{location of top right corner}
#' }
#' @export
stat_mosaic <- function(mapping = NULL, data = NULL, geom = "mosaic",
                        position = "identity", na.rm = TRUE,  divider = mosaic(),
                        show.legend = NA, inherit.aes = TRUE, offset = 0.01, ...)
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
StatMosaic <- ggplot2::ggproto(
  "StatMosaic", ggplot2::Stat,
#  required_aes = c("x"),
  non_missing_aes = "weight",

  setup_params = function(data, params) {
  #  cat("setup_params from StatMosaic\n")
  #   browser()

    params
  },

  compute_panel = function(self, data, scales, na.rm=FALSE, divider, offset) {
#   cat("compute_panel from StatMosaic\n")
#   browser()

   vars <- names(data)[grep("x", names(data))]
   conds <- names(data)[grep("conds", names(data))]

   if (in_data(data, "fill")) {
     # is fill colour one of the existing variables?
     # in that case, we want to replace the variable by "fill".
     # Otherwise, we expand vars by one variable.
     fillfound <- FALSE
     fillvar <- sapply(vars, FUN = function(x) {
       identical(data[,x], data$fill)
       })
     if (any(fillvar)) {
       vars[which(fillvar)] <- "fill"
       fillfound <- TRUE
     }

     # if we have conditions, we need to check if one of them is
     # is the fill variable.
     if (length(conds) > 0) {
        condsvar <- sapply(conds, FUN = function(x) {
           identical(data[,x], data$fill)
         })
         if (any(condsvar)) {
           conds[which(condsvar)] <- "fill"
           fillfound <- TRUE
         }
       }
     if (!fillfound) vars <- c("fill", vars)
   }

   # same things as above, only with alpha
   if (in_data(data, "alpha")) {
     # is alpha one of the existing variables?
     # in that case, we want to replace the variable by "alpha".
     # Otherwise, we expand vars by one variable.
     alphafound <- FALSE
     alphavar <- sapply(vars, FUN = function(x) {
       identical(data[,x], data$alpha)
     })
     if (any(alphavar)) {
       vars[which(alphavar)] <- "alpha"
       alphafound <- TRUE
     }

     # if we have conditions, we need to check if one of them is
     # is the alpha variable.
     if (length(conds) > 0) {
       condsvar <- sapply(conds, FUN = function(x) {
         identical(data[,x], data$alpha)
       })
       if (any(condsvar)) {
         conds[which(condsvar)] <- "alpha"
         alphafound <- TRUE
       }
     }
     if (!alphafound) vars <- c("alpha", vars)
   }


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

    # need to set x variable - I'd rather set the scales here.
    prs <- productplots::parse_product_formula(as.formula(formula))
    p <- length(c(prs$marg, prs$cond))
    if (is.function(divider)) divider <- divider(p)

    # the level at which things are labelled could be made a parameter.
    # At the moment the deepest level is being labelled.
    dflist <- list(data=subset(res, level==max(res$level)), formula=as.formula(formula), divider=divider)
    scx <- productplots::scale_x_product(dflist)
    scy <- productplots::scale_y_product(dflist)

    #   res is data frame that has xmin, xmax, ymin, ymax
    res <- dplyr::rename(res, xmin=l, xmax=r, ymin=b, ymax=t)
    res <- subset(res, level==max(res$level))

    # export the variables with the data - terrible hack
    # res$x <- list(scale=scx)
    # if (!is.null(scales$y)) {
    #   # only set the y scale if it is a product scale, otherwise leave it alone
    #   if ("ScaleContinuousProduct" %in% class(scales$y))
    #     res$y <- list(scale=scy)
    # }
# XXXX add label for res
    cols <- c(prs$marg, prs$cond)
    # browser()
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
    res$group <- 1 # unique(data$group) # ignore group variable
    res$PANEL <- unique(data$PANEL)
    res
  }
)

