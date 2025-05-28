partd <- function(x) {
  d <- attr(x, "d")
  if (!is.null(d)) d else 1
}

# Convenience function to create bounds
bound <- function(t = 1, r = 1, b = 0, l = 0) {
  data.frame(t = t, r = r, b = b, l = l)
}


divide <- function(data, bounds = bound(), divider = list(productplots::hbar), level = 1, cascade = 0, max_wt = NULL, offset = offset) {
  d <- partd(divider[[1]])
  if (ncol(data) == d + 1) {
    return(divide_once(data, bounds, divider[[1]], level, max_wt, offset))
  }
  # In divide we work with the opposite order of variables to margin -
  # so we flip and then flip back
  margin <- getFromNamespace("margin", "productplots")

  parent_data <- margin(data, rev(seq_len(d)))
  parent_data <- parent_data[, c(rev(seq_len(d)), d + 1)]

  parent <- divide_once(parent_data, bounds, divider[[1]], level, max_wt, offset)
  parentc <- parent
  parentc$l <- parent$l + cascade
  parentc$b <- parent$b + cascade
  parentc$r <- parent$r + cascade
  parentc$t <- parent$t + cascade

  if (is.null(max_wt)) {
    max_wt <- max(margin(data, d + 1, seq_len(d))$.wt, na.rm = TRUE)
  }

  # Rather awkward to deal with the case that some columns have potentially
  #   missing values as split() will just drop those levels:
  #   https://bugs.r-project.org/show_bug.cgi?id=18899
  # Therefore, for each column, if there are missing values, find the first
  #   string like NA, <NA>, <<NA>> that is not found in the data, and replace
  #   NA with that string before splitting. The names of the output will be
  #   all messed up, but they are ignored, so we don't need to restore them.
  na_sentinels <- rep("NA", d)
  for (jj in seq_len(d)) {
    if (!anyNA(data[[jj]])) next
    while (any(data[[jj]] == na_sentinels[jj])) {
      na_sentinels[jj] <- paste0("<", na_sentinels[jj], ">")
    }
    data[[jj]][is.na(data[[jj]])] <- na_sentinels[jj]
  }
  pieces <- split(data, data[seq_len(d)])



  children <- purrr::map_df(seq_along(pieces), function(i) {
    piece <- pieces[[i]]
    partition <- divide(piece[, -seq_len(d)], parentc[i, ], divider[-1],
                        level = level + 1, cascade = cascade, max_wt = max_wt, offset = offset)

    labels <- piece[rep(1, nrow(partition)), 1:d, drop = FALSE]
    cbind(labels, partition)
  })

  # children <- plyr::ldply(seq_along(pieces), function(i) {
  #   piece <- pieces[[i]]
  #   partition <- divide(piece[, -seq_len(d)], parentc[i, ], divider[-1],
  #                       level = level + 1, cascade = cascade, max_wt = max_wt, offset = offset)
  #
  #   labels <- piece[rep(1, nrow(partition)), 1:d, drop = FALSE]
  #   cbind(labels, partition)
  # })
  dplyr::bind_rows(parent, children)
}

# @param data data frame giving partitioning variables and weights.  Final
#   column should be called .wt and contain weights
divide_once <- function(data, bounds, divider, level = 1, max_wt = NULL, offset) {
  d <- partd(divider)
  # Convert into vector/matrix/array for input to divider function
  if (d > 1) {
    data[-ncol(data)] <- lapply(data[-ncol(data)], addNA, ifany = TRUE)
    wt <- tapply(data$.wt, data[-ncol(data)], identity)
    # This ensures that the order of the data matches the order tapply uses
    data <- as.data.frame.table(wt, responseName = ".wt")
  } else {
    wt <- data$.wt
  }

  wt <- wt / sum(wt, na.rm = TRUE)
  if (is.null(max_wt)) max_wt <- max(wt, na.rm = TRUE)

  partition <- divider(wt, bounds, offset, max = max_wt)
  cbind(data, partition, level = level)
}
