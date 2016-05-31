#' @export
product <- function(x, ...) {
  prod <- interaction(x, ...)
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
  class(df[,1]) <- "product"
  df
}


# new.nms <- !missing(col.names)
# if (cut.names) {
#   maxL <- if (is.logical(cut.names))
#     256L
#   else as.integer(cut.names)
#   if (any(long <- nchar(col.names, "bytes", keepNA = FALSE) >
#           maxL))
#     col.names[long] <- paste(substr(col.names[long],
#                                     1L, maxL - 6L), "...")
#   else cut.names <- FALSE
# }
# m <- match(names(formals(data.frame))[-1L], col.names, 0L)
# if (any.m <- any(m))
#   col.names[m] <- paste0("..adfl.", col.names[m])
# if (new.nms || any.m || cut.names)
#   names(x) <- col.names
# if (is.null(check.n <- list(...)$check.names))
#   check.n <- !optional
# alis <- c(list(check.names = check.n, fix.empty.names = fix.empty.names,
#                stringsAsFactors = stringsAsFactors), if (!is.null(row.names)) list(row.names = row.names))
# x <- do.call(data.frame, c(x, alis))

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

in_data <- function(data, variable) {
  length(intersect(names(data), variable)) > 0
}

#' better leave this an interval helper function
expand_variable <- function(data, variable) {
  if (!in_data(data, variable)) return()

  split_this <- as.character(data[,variable])
  df <-   plyr::ldply(strsplit(split_this, split=".", fixed=TRUE), function(x) x)
  names(df) <- paste(variable, 1:ncol(df), sep="")
  df
}


#' @rdname geom_mosaic
#' @export
stat_mosaic <- function(mapping = NULL, data = NULL, geom = "mosaic",
  position = "identity", na.rm = TRUE,  divider = productplots::mosaic(),
  show.legend = NA, inherit.aes = TRUE, ...)
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
      ...
    )
  )
}


#' @export
StatMosaic <- ggplot2::ggproto("StatMosaic", ggplot2::Stat,
#  required_aes = c("vars"),
#  non_missing_aes = "weight",

  setup_params = function(data, params) {
    cat("setup_params from StatMosaic\n")
    # browser()

    params
  },

  compute_group = function(data, scales, na.rm=FALSE, divider) {
    cat("compute_groups from StatMosaic\n")


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


    res <- productplots::prodcalc(df, formula=as.formula(formula),
                    divider = divider, cascade=0, scale_max = TRUE,
                    na.rm = na.rm)

#  browser()

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
