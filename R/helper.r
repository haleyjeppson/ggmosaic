scale_y_product <- function (df) {
  # browser()
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
  # browser()
  stopifnot(is.call(x) || is.name(x))
  if (length(x) == 3)
    x[[2]]
}

rhs <- function (x) {
  # browser()
  stopifnot(is.call(x) || is.name(x))
  if (length(x) == 2) {
    x[[2]]
  }
  else if (length(x) == 3) {
    x[[3]]
  }
}

op <- function (x) {
  # browser()
  stopifnot(is.call(x) || is.name(x))
  if (length(x) == 3 || length(x) == 2)
    x[[1]]
}

variables <- function(string, operator) {
  # browser()
  if(is.language(string)) {
    if(length(as.character(string)) == 1){
      string <- as.character(string)
      }
      else string <- as.character(string)[-1]
  }
  if(is.symbol(string)) string <- as.character(string)
  if(!is.character(string)) {
    # browser()
    cat("which type of string ended up here?")
  }

  res <- unlist(strsplit(split=operator, fixed=TRUE, string))
  trimws(res)
}

parse_product_formula <- function(f) {
  # browser()
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


