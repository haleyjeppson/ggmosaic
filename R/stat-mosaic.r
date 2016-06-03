#' @export
product <- function(x, ...) {
  vars <- list(x, ...)
  vars <- t(plyr::laply(vars, function(y) {
    x <- factor(y)
    paste(as.numeric(y), as.character(y), sep="-")   # keep order of variables the same
  }, .drop = FALSE))
#  browser()
  if (ncol(vars) == 1) prod <- vars
  else {
    prod <- plyr::laply(1:length(x), function(i) {
      paste(vars[i,], sep=".", collapse=".")
    })
  }
  prod <- factor(prod) # interaction doesn't deal with missing values correctly
  class(prod) <- "product"
  prod
}

#' @export
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

#browser()

  df
}


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

in_data <- function(data, variable) {
  length(intersect(names(data), variable)) > 0
}

#' better leave this an internal helper function
expand_variable <- function(data, variable) {
  if (!in_data(data, variable)) return()

  split_this <- as.character(data[,variable])
  df <-   plyr::ldply(strsplit(split_this, split=".", fixed=TRUE), function(x) x)
  df <- plyr::llply(df, function(x) {
    split_this <- as.character(x)
    parts <- plyr::ldply(strsplit(split_this, split="-", fixed=TRUE), function(x) x)
    #x <- factor(parts[,2])
    if (ncol(parts) == 2) {
    xorder <- suppressWarnings({as.numeric(parts[,1])})
    if (any(is.na(xorder))) xorder[is.na(xorder)] <- max(xorder, na.rm=T) + 1
    x <- reorder(factor(parts[,2]), xorder)
    return(x)
    }
    parts
  })
  df <- data.frame(df)

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
                        position = "position", na.rm = TRUE,  divider = mosaic(),
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
#'
#' @export
StatMosaic <- ggplot2::ggproto(
  "StatMosaic", ggplot2::Stat,
  #  required_aes = c("vars"),
  #  non_missing_aes = "weight",

  setup_params = function(data, params) {
    cat("setup_params from StatMosaic\n")
    # browser()

    params
  },

  compute_group = function(data, scales, na.rm=FALSE, divider, offset) {
    cat("compute_groups from StatMosaic\n")
# browser()

    vars <- expand_variable(data, "vars")
    #    data <- dplyr::select(data, -vars)
    #    data <- data.frame(data, vars)

    conds <- expand_variable(data, "conds")


    formula <-  paste(names(vars), collapse="+")
    if (in_data(data, "fill")) formula <- paste("fill+",formula)
    formula <- paste("weight~", formula)

    if (! is.null(conds)) {
      formula <- paste(formula, paste(names(conds), collapse="+"), sep="|")
    }
    df <- data.frame(data, vars)
    if (! is.null(conds)) df <- data.frame(df, conds)
    if (!in_data(df, "weight")) {
      df$weight <- 1
    }


    res <- prodcalc(df, formula=as.formula(formula),
                                  divider = divider, cascade=0, scale_max = TRUE,
                                  na.rm = na.rm, offset = offset)


    #   res is data frame that has xmin, xmax, ymin, ymax
    res <- dplyr::rename(res, xmin=l, xmax=r, ymin=b, ymax=t)
    # only consider the deepest level of the mosaic
    #    res <- subset(res, level == max(res$level))

    # merge res with data:
    res$group <- unique(data$group)
    res$PANEL <- unique(data$PANEL)
    res
  }
)

#' might need to overwrite check_aesthetics in geom - at the moment this function gets ignored
#' @export
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
