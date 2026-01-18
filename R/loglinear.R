#' Translate shortcut strings to formulas
#'
#' @param shortcut Character: "independence", "saturated", or "conditional"
#' @param vars Character vector of margin variable names
#' @param conds Character vector of conditioning variable names (optional)
#' @return Formula object
#' @keywords internal
shortcut_to_formula <- function(shortcut, vars, conds = NULL) {
  shortcut <- match.arg(tolower(shortcut),
                        c("independence", "saturated", "conditional"))

  all_vars <- c(vars, conds)

  if (shortcut == "independence") {
    # Main effects only: ~ A + B + C
    formula_str <- paste("~", paste(all_vars, collapse = " + "))

  } else if (shortcut == "saturated") {
    # All interactions: ~ A * B * C
    formula_str <- paste("~", paste(all_vars, collapse = " * "))

  } else if (shortcut == "conditional") {
    # Conditional independence
    # Margins are independent given conditions
    # ~ marg1 + marg2 + cond + marg1:cond + marg2:cond
    if (length(conds) == 0) {
      stop("'conditional' shortcut requires conditioning variables (use conds aesthetic)",
           call. = FALSE)
    }
    main_effects <- paste(all_vars, collapse = " + ")
    # Create all interactions between margin vars and condition vars
    interactions <- paste(
      apply(expand.grid(vars, conds), 1, paste, collapse = ":"),
      collapse = " + "
    )
    formula_str <- paste("~", main_effects, "+", interactions)
  }

  as.formula(formula_str)
}


#' Build model formula from user specification
#'
#' @param expected Formula, character shortcut, or NULL
#' @param vars Character vector of margin variable names (with prefixes like x__Class)
#' @param conds Character vector of conditioning variable names (optional, with prefixes)
#' @return Formula object or NULL
#' @importFrom stats  as.formula glm poisson predict reformulate setNames terms
#' @keywords internal
build_model_formula <- function(expected, vars, conds = NULL) {
  # If expected is NULL, no model
  if (is.null(expected)) {
    return(NULL)
  }

  # If expected is already a formula, map it to use prefixed variable names
  if (is.formula(expected)) {
    # Extract the formula terms (variable names from user's formula)
    formula_terms <- attr(terms(expected), "term.labels")

    # Create a mapping from original names to prefixed names
    # vars and conds contain prefixed names like "x__Class", "x__Sex"
    # We need to strip prefixes to match against user's formula terms
    all_prefixed_vars <- c(vars, conds)

    # Extract original variable names by removing prefixes
    # Prefixes can be: x__, x__fill__, x__alpha__, conds[0-9]+__
    original_names <- gsub("^(x__|x__fill__|x__alpha__|conds[0-9]+__)", "", all_prefixed_vars)

    # Create mapping: original_name -> prefixed_name
    name_map <- setNames(all_prefixed_vars, original_names)

    # Map each term in the user's formula
    # Terms can be simple (e.g., "Class") or interactions (e.g., "Class:Sex")
    mapped_terms <- sapply(formula_terms, function(term) {
      # Split by : for interactions
      if (grepl(":", term)) {
        parts <- strsplit(term, ":")[[1]]
        mapped_parts <- sapply(parts, function(p) {
          p_trimmed <- trimws(p)
          if (p_trimmed %in% names(name_map)) {
            name_map[[p_trimmed]]
          } else {
            stop("Variable '", p_trimmed, "' in formula not found in data", call. = FALSE)
          }
        })
        paste(mapped_parts, collapse = ":")
      } else {
        # Simple term
        term_trimmed <- trimws(term)
        if (term_trimmed %in% names(name_map)) {
          name_map[[term_trimmed]]
        } else {
          stop("Variable '", term_trimmed, "' in formula not found in data", call. = FALSE)
        }
      }
    })

    # Rebuild formula with mapped names
    new_formula_str <- paste("~", paste(mapped_terms, collapse = " + "))
    return(as.formula(new_formula_str))
  }

  # If expected is a character, treat as shortcut
  if (is.character(expected) && length(expected) == 1) {
    return(shortcut_to_formula(expected, vars, conds))
  }

  # Otherwise, invalid input
  stop("'expected' must be a formula, character shortcut ('independence', ",
       "'saturated', 'conditional'), or NULL", call. = FALSE)
}


#' Fit Poisson GLM and calculate Pearson residuals
#'
#' @param data Data frame with .n column (observed counts)
#' @param vars Character vector of all variable names (margins + conds)
#' @param model_formula Formula for the GLM
#' @return Data frame with added .expected and .residual columns
#' @keywords internal
#' @importFrom dplyr select all_of distinct left_join
fit_loglinear_model <- function(data, vars, model_formula) {
  # Check for reserved column names
  if (any(c(".expected", ".residual") %in% names(data))) {
    warning("Data contains reserved column names (.expected, .residual). ",
            "These will be overwritten.", call. = FALSE)
  }

  # Create aggregated dataset for modeling
  # Select only the variables needed for the model plus observed counts
  mod_data <- data |>
    dplyr::select(dplyr::all_of(c(vars, ".n"))) |>
    dplyr::distinct()

  # Fit Poisson GLM
  tryCatch({
    # Build GLM formula with .n as response
    glm_formula <- reformulate(
      attr(terms(model_formula), "term.labels"),
      response = ".n"
    )

    # Fit the model
    model <- glm(glm_formula, data = mod_data, family = poisson())

    # Calculate expected values (fitted values from the model)
    # Use newdata to ensure we get predictions for all rows, even if some were dropped during fitting
    mod_data$.expected <- predict(model, newdata = mod_data, type = "response")

    # Calculate Pearson residuals with protection against zero expected values
    # Use machine epsilon as minimum to avoid division by zero
    expected_safe <- pmax(mod_data$.expected, .Machine$double.eps)
    mod_data$.residual <- (mod_data$.n - expected_safe) / sqrt(expected_safe)

    # Join the expected values and residuals back to original data
    # Important: Only add .expected and .residual columns, don't modify existing vars
    # Select only the new columns plus join keys from mod_data
    join_data <- mod_data |>
      dplyr::select(dplyr::all_of(c(vars, ".expected", ".residual")))

    # Perform the join
    # The 'by' parameter ensures variable columns are used as keys and not duplicated
    result <- dplyr::left_join(
      data,
      join_data,
      by = vars
    )

    result

  }, error = function(e) {
    warning("Loglinear model fitting failed: ", e$message,
            "\nProceeding without residual shading.", call. = FALSE)
    # On error, set expected = observed and residuals = 0
    data$.expected <- data$.n
    data$.residual <- 0
    data
  })
}
