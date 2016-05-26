#' @export
scale_type.quoted <- function(x) {
  "quoted"
}


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
StatMosaic <- ggplot2::ggproto("StatMosaic", ggplot2::Stat,
#  required_aes = c("vars"),
#  non_missing_aes = "weight",

  setup_params = function(data, params) {
    cat("setup_params from StatMosaic\n")
    # browser()

    params
  },

  compute_group = function(data, scales, na.rm=FALSE) {
    cat("compute_groups from StatMosaic\n")
  #  browser()


    vars <- expand_variable(data, "vars")
#    data <- dplyr::select(data, -vars)
    data <- data.frame(data, vars)

    conds <- expand_variable(data, "conds")
    if (! is.null(conds)) {
      data <- data.frame(data, conds)
    }
    if (!in_data(data, "weight")) {
      data$weight <- 1
    }

    formula <- paste("weight~", paste(names(vars), collapse="+"))
    if (! is.null(conds)) {
      formula <- paste(formula, paste(names(conds), collapse="+"), sep="|")
    }

    res <- productplots::prodcalc(data, formula=as.formula(formula),
                    divider = productplots::mosaic(), cascade=0, scale_max = TRUE,
                    na.rm = na.rm)

 browser()
#    df  is data frame with data that has xmin, xmax, ymin, ymax
    res <- dplyr::rename(res, xmin=l, xmax=r, ymin=b, ymax=t)
    # get variables from data into res
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
