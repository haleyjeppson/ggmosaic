# Implementation Plan: Loglinear Model Residual Shading for ggmosaic

## Overview

Add loglinear model fitting and Pearson residual-based shading to `geom_mosaic()`, similar to vcd's `mosaic(expected = ...)` functionality. This will enable statistical visualization of deviations from independence or other specified models.

## User Requirements (Confirmed)

- **Residual type**: Pearson residuals `(observed - expected) / sqrt(expected)`
- **Model specification**: Support both formula interface AND shortcuts ("independence", "saturated", "conditional")
- **Parameter approach**: New parameter `expected` (independent of fill aesthetic)
- **Color scale**: Diverging blue-white-red centered at 0

## Architecture Overview

### Data Flow Integration Point

The feature integrates into ggmosaic's existing pipeline:

```
geom_mosaic() → stat_mosaic() → StatMosaic$compute_panel() → prodcalc()
                                                                    ↓
                                                    [FIT MODEL & CALCULATE RESIDUALS HERE]
                                                                    ↓
                                                GeomMosaic$draw_panel()
```

**Key insight from exploration**:
- `prodcalc()` (R/calculate.R:49) returns data with `.n` column containing observed counts
- This is the perfect injection point for model fitting
- Existing Shiny app (inst/shiny/models/server.R:440-446) already demonstrates the exact approach

### Design Decisions

1. **Model fitting location**: Inside `prodcalc()` - lowest level with access to observed counts
2. **Shortcut translation**: Build formula dynamically from detected variables
3. **Fill behavior**: Auto-map residuals to fill only if user hasn't specified fill; always include `.residual` column for manual mapping
4. **Backward compatibility**: `expected = NULL` (default) preserves current behavior exactly

## Implementation Plan

### Phase 1: Core Model Fitting Infrastructure

**Create: `R/loglinear.R`** (new file)

Three helper functions:

```r
#' Translate shortcut strings to formulas
#' @param shortcut "independence", "saturated", or "conditional"
#' @param vars Character vector of margin variable names
#' @param conds Character vector of conditioning variable names
#' @return Formula object
shortcut_to_formula <- function(shortcut, vars, conds = NULL)

#' Build model formula from user specification
#' @param expected Formula, character shortcut, or NULL
#' @param vars Margin variable names
#' @param conds Conditioning variable names
#' @return Formula object or NULL
build_model_formula <- function(expected, vars, conds = NULL)

#' Fit Poisson GLM and calculate Pearson residuals
#' @param data Data frame with .n column (observed counts)
#' @param vars All variable names (margins + conds)
#' @param model_formula Formula for GLM
#' @return Data frame with added .expected and .residual columns
fit_loglinear_model <- function(data, vars, model_formula)
```

**Key implementation details**:
- Use `glm(formula, data, family = poisson())`
- Calculate expected: `predict(model, type = "response")`
- Calculate Pearson residuals: `(observed - expected) / sqrt(pmax(expected, .Machine$double.eps))`
- Wrap in `tryCatch()` with informative warnings on failure
- Handle zero expected values with machine epsilon protection

**Shortcut formulas**:
- `"independence"`: `~ A + B + C` (main effects only)
- `"saturated"`: `~ A * B * C` (all interactions)
- `"conditional"`: `~ A + B + C + A:C + B:C` (margins independent given conditions)

### Phase 2: Parameter Threading

**Modify: `R/calculate.R`**

Update `prodcalc()` signature and implementation:

```r
# Line 20 - Add parameter
prodcalc <- function(data, formula, divider = mosaic(), cascade = 0,
                     scale_max = TRUE, na.rm = FALSE, offset = offset,
                     expected = NULL) {  # NEW

  # ... existing code through line 49 ...

  wt2 <- dplyr::rename(wt2, .n=".wt")
  result <- dplyr::left_join(df, wt2, by = setdiff(names(wt2), ".n"))

  # NEW: Fit model if requested
  if (!is.null(expected)) {
    all_vars <- c(vars$marg, vars$cond)
    model_formula <- build_model_formula(expected, vars$marg, vars$cond)
    result <- fit_loglinear_model(result, all_vars, model_formula)
  }

  result
}
```

**Modify: `R/stat-mosaic.R`**

Update function signatures to accept and pass `expected`:

```r
# Line 11 - Add to stat_mosaic()
stat_mosaic <- function(..., expected = NULL, ...)

# Line 118 - Add to compute_panel()
compute_panel = function(self, data, scales, ..., expected = NULL)

# Line 142 - Pass to prodcalc()
res <- prodcalc(df, formula=as.formula(formula), ..., expected = expected)

# After line 201 - Handle residual mapping
if (!is.null(expected) && ".residual" %in% names(res)) {
  # Auto-map to fill only if user hasn't specified fill
  fill_idx <- grep("x__fill", names(data))
  if (length(fill_idx) == 0) {
    res$fill <- res$.residual
  }
}
```

**Modify: `R/geom-mosaic.R`**

```r
# Line 108 - Add to geom_mosaic() signature
geom_mosaic <- function(..., expected = NULL, ...)

# Line 182 - Add to params
params = list(..., expected = expected, ...)
```

### Phase 3: User Interface

**Create: `R/scale-residual.R`** (new file)

Convenience scale for residual visualization:

```r
#' Diverging color scale for Pearson residuals
#' @param low Color for negative residuals (default: "steelblue")
#' @param mid Color for zero residuals (default: "white")
#' @param high Color for positive residuals (default: "firebrick")
#' @export
scale_fill_residual <- function(...,
                                low = "steelblue",
                                mid = "white",
                                high = "firebrick",
                                midpoint = 0,
                                name = "Pearson\nResidual") {
  ggplot2::scale_fill_gradient2(low = low, mid = mid, high = high,
                                midpoint = midpoint, name = name, ...)
}
```

### Phase 4: Documentation

**Update roxygen2 documentation**:

In `R/geom-mosaic.R`:
```r
#' @param expected Optional loglinear model specification for residual shading.
#'   Can be a formula (e.g., \code{~ A + B}), a character shortcut
#'   ("independence", "saturated", "conditional"), or NULL (default).
#'   When specified, Pearson residuals are calculated and mapped to fill.
#' @examples
#' # Independence model with automatic residual shading
#' ggplot(data = titanic) +
#'   geom_mosaic(aes(x = product(Class, Sex)), expected = "independence") +
#'   scale_fill_residual()
```

**Update NAMESPACE**:
```r
export(scale_fill_residual)
export(build_model_formula)
```

## Critical Files Summary

| File | Action | Purpose |
|------|--------|---------|
| `R/loglinear.R` | CREATE | Model fitting logic and helper functions |
| `R/calculate.R` | MODIFY | Add `expected` parameter to `prodcalc()`, integrate model fitting |
| `R/stat-mosaic.R` | MODIFY | Thread `expected` through stat layer, handle residual-to-fill mapping |
| `R/geom-mosaic.R` | MODIFY | Add `expected` parameter to user-facing function |
| `R/scale-residual.R` | CREATE | Convenience scale for diverging color scheme |

## Testing Strategy

### Unit Tests (create `tests/testthat/test-loglinear.R`)

- Formula building: verify shortcuts produce correct formulas
- Model fitting: test with various contingency tables
- Error handling: zero cells, convergence failures
- Backward compatibility: `expected = NULL` produces identical output

### Integration Tests

- Compare residual values with vcd package results
- Test all three shortcuts (independence, saturated, conditional)
- Verify fill aesthetic precedence rules

### Visual Validation

Create examples in `issues/` folder comparing ggmosaic and vcd outputs

## Edge Cases and Error Handling

1. **Zero expected frequencies**: Use `pmax(expected, .Machine$double.eps)` to prevent division by zero
2. **Model convergence failures**: Wrap in `tryCatch()`, warn user, return observed values only
3. **Variable name conflicts**: Warn if `.expected` or `.residual` columns exist in user data
4. **Fill aesthetic conflicts**: Document behavior - explicit fill takes precedence, but `.residual` remains available
5. **Sparse tables**: GLM may not converge; provide informative error messages

## Implementation Order

1. Create `R/loglinear.R` with all helper functions
2. Modify `R/calculate.R` to integrate model fitting
3. Modify `R/stat-mosaic.R` for parameter threading
4. Modify `R/geom-mosaic.R` for user interface
5. Create `R/scale-residual.R` for convenience scale
6. Update documentation and NAMESPACE
7. Create tests and examples

## Example Usage

After implementation, users will be able to:

```r
library(ggmosaic)

# Independence model (default)
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual()

# Custom formula
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Survived)),
              expected = ~ Class + Sex) +
  scale_fill_residual()

# Conditional independence
ggplot(data = happy) +
  geom_mosaic(aes(x = product(health, marital), conds = sex),
              expected = "conditional") +
  scale_fill_residual()

# Manual residual mapping (override auto-fill)
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex), fill = .residual),
              expected = "independence") +
  scale_fill_gradient2()
```

## Dependencies

No new package dependencies required:
- `stats::glm()` for model fitting (base R)
- `dplyr` for data manipulation (already imported)
- `ggplot2` for scale functions (already imported)

## Deliverables for `issues/` Folder

Upon user approval, create in `issues/` folder:
- Example script comparing ggmosaic residual shading with vcd
- Visual comparison plots
- Documentation of implementation decisions
