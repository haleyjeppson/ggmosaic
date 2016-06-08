#' Internal helper function
#'
#' Convenience function to create bounds
#' function copied directly from the productplots package
#' @author Hadley Wickham
bound <- function(t = 1, r = 1, b = 0, l = 0) {
  data.frame(t = t, r = r, b = b, l = l)
}

#' Internal helper function
#'
#' function copied directly from the productplots package
#' @author Hadley Wickham
margin <- function(table, marginals = c(), conditionals = c()) {
  if (is.numeric(marginals))    marginals    <- names(table)[marginals]
  if (is.numeric(conditionals)) conditionals <- names(table)[conditionals]

  marginals <- rev(marginals)
  conditionals <- rev(conditionals)

  marg <- weighted.table(table[c(conditionals, marginals)], table$.wt)

  if (length(conditionals) > 0) {
    # Work around bug in ninteraction
    cond <- marg[conditionals]
    cond[] <- lapply(cond, addNA, ifany = TRUE)
    marg$.wt <- stats::ave(marg$.wt, plyr::id(cond), FUN = function(x) x / sum(x, na.rm=TRUE))
  }

  marg$.wt[is.na(marg$.wt)] <- 0
  marg
}

#' Internal helper function
#'
#' function copied directly from the productplots package
#' @author Hadley Wickham
weighted.table <- function(vars, wt = NULL) {
  # If no weight column, give constant weight
  if (is.null(wt)) {
    wt <- rep(1, nrow(vars))
    wt <- wt/sum(wt, na.rm= TRUE)
  }

  # Ensure missing values are counted
  vars[] <- lapply(vars, addNA, ifany = TRUE)

  # Need to reverse order of variables because as.data.frame works in the
  # opposite way to what I want
  sums <- tapply(wt, rev(vars), sum, na.rm = TRUE)

  df <- as.data.frame.table(sums, responseName = ".wt")
  # Missing values represent missing combinations in the original dataset,
  # i.e. they have zero weight
  df$.wt[is.na(df$.wt)] <- 0
  df[, c(rev(seq_len(ncol(df) - 1)), ncol(df)) ]
}
