ggm <- new.env()
ggm$separators = c(sep1=":", sep2="-", sep3=".")


#' Separator characters
#'
#' separators are a list of three special characters. These characters need to be different for each level.
#' @param seps Vector of length three. The first separator tags the value with its order, the second separator distinguishes between the key (variable name) and the value of the variable in the record.
#' The third separator links different values. XXX Can't use "|" for the first separator - not sure why. Might be a bug. XXX
#' @export
set.separators <- function(seps) {
  stopifnot(length(seps) == 3, length(unique(seps)) == 3)
#  env <- loadNamespace("ggmosaic")
#  unlockBinding("separators", env)
#  assign("separators", seps , env)
  ggm$separators <- seps
}

#' @rdname set.separators
#' @export
get.separators <- function() {
  ggm$separators
}


#' Product
#'
#' This function creates a product variable based on all combinations of the variables.
#' @export
#'
#' @param x variable
#' @param ... other arguments passed on
#'
#'
product <- function(x, ...) {

  # interaction doesn't deal with missing values correctly
  vars <- list(x, ...)
  varNames <- as.character(match.call()[-1])
  separators <- get.separators()
  if(length(vars) < length(varNames)) varNames <- varNames[1:length(vars)]
#browser()
  vars <- sapply(1:length(vars), function(y) {
    z <- factor(vars[[y]])
    name <- varNames[y]
    ynum <- as.numeric(z)
    ynum[is.na(ynum)] <- max(ynum, na.rm=TRUE)+1
    ychar <- as.character(z)
    paste(ynum, paste(name, ychar, sep=separators[1]), sep=separators[2])
  })

  if (ncol(vars) == 1)
    prod <- vars
  else {
    prod <- vars[,1]
    for (i in 2:ncol(vars)) {
      prod <- paste(prod, vars[,i], sep=separators[3])
    }
  }
  prod <- factor(prod)
  class(prod) <- "product"
  prod
}


#' Is an Object of Type Product?
#'
#' Checks whether its argument is a product.
#'
#' @export
#'
#' @param x an `R` object
#'
is.product <- function(x) {
  "product" %in% class(x)
}


#' @method as.data.frame product
#' @export
as.data.frame.product <- function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x),
                                                                                                  width.cutoff = 500L), collapse = " "))
{
  force(nm)
  nrows <- length(x)
  if (!(is.null(row.names) || (is.character(row.names) && length(row.names) ==
                               nrows))) {
    warning(gettextf("'row.names' is not a character vector of length %d -- omitting it. Will be an error!",
                     nrows), domain = NA)
    row.names <- NULL
  }
  if (is.null(row.names)) {
    if (nrows == 0L)
      row.names <- character()
    else if (length(row.names <- names(x)) != nrows || anyDuplicated(row.names))
      row.names <- .set_row_names(nrows)
  }
  if (!is.null(names(x)))
    names(x) <- NULL
  levels <- attr(x, "levels")
  value <- list(levels[x])

  if (!optional)
    names(value) <- nm
  df <- structure(value, row.names = row.names, class = "data.frame")
# browser()

  df
}


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

in_data <- function(data, variable) {
  length(intersect(names(data), variable)) > 0
}

#' @importFrom purrr map_df
#' @importFrom dplyr mutate select
#' @importFrom stats reorder
expand_variable <- function(data, variable) {
  # better leave this an internal helper function
  if (!in_data(data, variable)) return()

#  browser()
  separators <- get.separators()

  x__ <- NULL # provide visible binding
  data$x__ <- data[,variable]
  k <- sum(gregexpr(separators[3], data$x__[1], fixed = TRUE)[[1]]!=-1)+1
  df <- tidyr::separate(select(data, x__), x__, into=paste0("V",1:k), sep = paste0("\\", separators[3]))

  df <- purrr::map(df, .f = function(x) {
    dframe <- data.frame(x=x)
    dframe <- tidyr::separate(dframe, x, into=c("levels", "x"), sep=paste0("\\", separators[2]))
    dframe <- mutate(dframe, x = reorder(x, as.numeric(levels), mean, na.rm=TRUE))
    dframe$x
  })
  df <- data.frame(df)

#  split_this <- as.character(data[,variable])
#  df <-   plyr::ldply(strsplit(split_this, split=separators[3], fixed=TRUE), function(x) x)
  # if(ncol(df) == 1)
  # df <- plyr::llply(df, function(x) {
  #   split_this <- as.character(x)
  #   parts <- plyr::ldply(strsplit(split_this, split=separators[2], fixed=TRUE), function(x) x)
  #   #x <- factor(parts[,2])
  #   if (ncol(parts) == 2) {
  #     parts[,2] <- sapply(strsplit(parts[,2],separators[1]),'[',2)
  #     xorder <- suppressWarnings({as.numeric(parts[,1])})
  #     if (any(is.na(xorder))) xorder[is.na(xorder)] <- max(xorder, na.rm=T) + 1
  #     x <- stats::reorder(factor(parts[,2]), xorder)
  #     return(x)
  #   }
  #   parts
  # })
  # else
  # df <- plyr::llply(df, function(x) {
  #   split_this <- as.character(x)
  #   parts <- plyr::ldply(strsplit(split_this, split=separators[2], fixed=TRUE), function(x) x)
  #   #x <- factor(parts[,2])
  #   if (ncol(parts) == 2) {
  #   xorder <- suppressWarnings({as.numeric(parts[,1])})
  #   if (any(is.na(xorder))) xorder[is.na(xorder)] <- max(xorder, na.rm=T) + 1
  #   x <- stats::reorder(factor(parts[,2]), xorder)
  #   return(x)
  #   }
  #   parts
  # })
  # df <- data.frame(df)

  names(df) <- paste(variable, 1:ncol(df), sep="")
  df
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
  # browser()

    vars <- expand_variable(data, "x")
    if (!is.null(vars)){
    names(vars) <- apply(vars, 2, function(x) unique(sapply(strsplit(as.character(x), ggm$separators[1]), "[", 1)))
    }
    conds <- expand_variable(data, "conds")
    if (!is.null(conds)){
    names(conds) <- apply(conds, 2, function(x) unique(sapply(strsplit(as.character(x), ggm$separators[1]), "[", 1)))
    }

    if (is.null(vars)) formula <- "1"
    else formula <-  paste(names(vars), collapse="+")

    if (in_data(data, "fill")) {
      if(!is.null(conds)){
      if (!all(apply(data[complete.cases(data[c('conds', 'x')]),], 1, function(y) as.logical(grepl(y['fill'], y['conds']))))) {
        if (!all(apply(data[complete.cases(data[c('fill', 'x')]),], 1, function(y) as.logical(grepl(y['fill'], y['x']))))) {
      formula <- paste("fill+",formula) }
     else { #---- need to replace varible in formula with fill?
       vars1 <- data.frame(fill=data$fill, vars)
       logicals <- data.frame(t(apply(vars1[complete.cases(vars1[,1]),], 1, function(y) grepl(y['fill'], y[]))))
       logicals <- logicals[,-1]
       logs <- apply(data.frame(logicals), 2, function(y) all(y[]))
       logs <- data.frame(t(logs))
       names(logs) <- names(vars)
       names(logs)[logs==TRUE] <- "fill"

      formula <- paste(names(logs), collapse="+")
     }
      }
      }
        else {
          if (!all(apply(data[complete.cases(data[c('fill', 'x')]),], 1, function(y) as.logical(grepl(y['fill'], y['x']))))) {
            formula <- paste("fill+",formula) }
          else { #---- need to replace varible in formula with fill?
            vars1 <- data.frame(fill=data$fill, vars)
            logicals <- data.frame(t(apply(vars1[complete.cases(vars1[,1]),], 1, function(y) grepl(y['fill'], y[]))))
            logicals <- logicals[,-1]
            logs <- apply(data.frame(logicals), 2, function(y) all(y[]))
            logs <- data.frame(t(logs))
            names(logs) <- names(vars)
            names(logs)[logs==TRUE] <- "fill"

            formula <- paste(names(logs), collapse="+")


          }
          }}


    formula <- paste("weight~", formula)

    if (! is.null(conds)) {
      if (!all(apply(data[complete.cases(data[c('fill', 'conds')]),], 1, function(y) as.logical(grepl(y['fill'], y['conds']))))) {
        formula <- paste(formula, paste(names(conds), collapse="+"), sep="|")
      }
      else {
        conds1 <- data.frame(fill=data$fill, conds)
        logicals <- data.frame(t(apply(conds1[complete.cases(conds1[,1]),], 1, function(y) grepl(y['fill'], y[]))))
        logicals <- logicals[,-1]
        logs <- apply(data.frame(logicals), 2, function(y) all(y[]))
        logs <- data.frame(t(logs))
        names(logs) <- names(conds)
        names(logs)[logs==TRUE] <- "fill"

        formula<- paste(formula, paste(names(logs), collapse="+"), sep="|")

      }
    }

    df <- data
    if (! is.null(vars)) df <- data.frame(df, vars)
    if (! is.null(conds)) df <- data.frame(df, conds)
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
    res$x <- list(scale=scx)
    if (!is.null(scales$y)) {
      # only set the y scale if it is a product scale, otherwise leave it alone
      if ("ScaleContinuousProduct" %in% class(scales$y))
        res$y <- list(scale=scy)
    }
# XXXX add label for res
    cols <- c(prs$marg, prs$cond)
#    browser()
    if (length(cols) > 1) {
    df <- res[,cols]
    df <- tidyr::unite_(df, "label", cols, sep="\n")

    res$label <- df$label
    } else res$label <- as.character(res[,cols])

#    res$label <- plyr::ldply(
#      1:nrow(res),
#      function(x) paste(unlist(res[x, rev(cols)]), collapse="\n"))$V1
    # merge res with data:
    res$group <- 1 # unique(data$group) # ignore group variable
    res$PANEL <- unique(data$PANEL)
    res
  }
)

# #' might need to overwrite check_aesthetics in geom - at the moment this function gets ignored
# #' @export

check_aesthetics <- function (x, n)
{
  #  do a recursive check on the length of aesthetics - this will allow a specification of
  #  variables as a list (to allow for arbitrary many variables such as for mosaic plots or
  #  parallel coordinate plots)
  islist <- vapply(x, is.list, logical(1))
  lapply(x[islist==TRUE], check_aesthetics, n =n)
  x <- x[-which(islist)]
  #  end of recursive check

  ns <- vapply(x, length, numeric(1))
  good <- ns == 1L | ns == n
  if (all(good)) {
    return()
  }
  stop("Aesthetics must be either length 1 or the same as the data (",
       n, "): ", paste(names(!good), collapse = ", "), call. = FALSE)
}
