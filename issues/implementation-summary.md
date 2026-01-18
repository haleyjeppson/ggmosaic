# Loglinear Model Residual Shading Implementation Summary

## Overview

Successfully implemented loglinear model fitting and Pearson residual-based shading for `geom_mosaic()`, enabling statistical visualization of deviations from independence or other specified models. This brings ggmosaic closer to feature parity with the vcd package's `mosaic()` function.

## What Was Implemented

### 1. Core Model Fitting Infrastructure (`R/loglinear.R`)

**Three helper functions**:

- `shortcut_to_formula()`: Converts string shortcuts to R formulas
  - "independence" → `~ A + B + C` (main effects only)
  - "saturated" → `~ A * B * C` (all interactions)
  - "conditional" → `~ A + B + C + A:C + B:C` (conditional independence)

- `build_model_formula()`: Router function that handles:
  - Formula objects (passed through directly)
  - Character shortcuts (converted via `shortcut_to_formula()`)
  - NULL (no model fitting)

- `fit_loglinear_model()`: Core fitting logic
  - Fits Poisson GLM using `glm(formula, family = poisson())`
  - Calculates expected frequencies: `predict(model, type = "response")`
  - Computes Pearson residuals: `(observed - expected) / sqrt(expected)`
  - Includes error handling with `tryCatch()`
  - Protects against division by zero with machine epsilon

### 2. Integration Points

**Modified: `R/calculate.R`**
- Added `expected = NULL` parameter to `prodcalc()`
- Integrated model fitting after observed counts are calculated
- Calls `build_model_formula()` and `fit_loglinear_model()`

**Modified: `R/stat-mosaic.R`**
- Added `expected = NULL` parameter to `stat_mosaic()`
- Updated `StatMosaic$compute_panel()` to accept and pass `expected`
- Added residual-to-fill auto-mapping logic (only when fill not explicitly set)

**Modified: `R/geom-mosaic.R`**
- Added `expected = NULL` parameter to `geom_mosaic()`
- Updated documentation with parameter description and examples
- Thread `expected` through to stat layer

### 3. User Interface

**Created: `R/scale-residual.R`**
- `scale_fill_residual()`: Convenience function for diverging color scale
  - Default colors: steelblue (negative) - white (zero) - firebrick (positive)
  - Centered at 0 by default
  - Customizable via all `scale_fill_gradient2()` parameters
- `scale_fill_residuals()`: Alias for plural form

**Updated: `NAMESPACE`**
- Exported `scale_fill_residual` and `scale_fill_residuals`

## Files Created

| File | Lines | Purpose |
|------|-------|---------|
| `R/loglinear.R` | 133 | Model fitting and helper functions |
| `R/scale-residual.R` | 56 | Diverging color scale for residuals |
| `issues/residual-shading-examples.R` | 260 | Comprehensive usage examples |
| `issues/loglinear-residual-shading-plan.md` | ~600 | Implementation plan (copy) |
| `issues/implementation-summary.md` | This file | Summary documentation |

## Files Modified

| File | Changes | Summary |
|------|---------|---------|
| `R/calculate.R` | +19 lines | Added `expected` param, model fitting integration |
| `R/stat-mosaic.R` | +13 lines | Parameter threading, residual-to-fill mapping |
| `R/geom-mosaic.R` | +16 lines | User interface, documentation, examples |
| `NAMESPACE` | +2 exports | Added scale_fill_residual exports |

## How It Works

### Data Flow

```
User calls: geom_mosaic(aes(x = product(A, B)), expected = "independence")
                                    ↓
geom_mosaic() passes expected to stat_mosaic()
                                    ↓
stat_mosaic() passes expected to StatMosaic$compute_panel()
                                    ↓
compute_panel() passes expected to prodcalc()
                                    ↓
prodcalc() calculates observed counts (.n column)
                                    ↓
build_model_formula() converts "independence" to ~ A + B
                                    ↓
fit_loglinear_model():
  - Aggregates data by all variables
  - Fits glm(.n ~ A + B, family = poisson())
  - Calculates .expected = fitted values
  - Calculates .residual = (observed - expected) / sqrt(expected)
                                    ↓
Result includes .expected and .residual columns
                                    ↓
compute_panel() checks if fill was explicitly set:
  - If NO: res$fill <- res$.residual (auto-map)
  - If YES: keep user's fill, but .residual still available
                                    ↓
GeomMosaic$draw_panel() uses fill values for rectangle colors
                                    ↓
scale_fill_residual() applies diverging color scale
```

### Backward Compatibility

When `expected = NULL` (the default), behavior is **identical** to the original implementation:
- No model fitting occurs
- No `.expected` or `.residual` columns added
- Zero performance overhead
- All existing code works unchanged

## Usage Examples

### Basic Usage

```r
library(ggmosaic)
data(titanic)

# Independence model (shortcut)
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual()

# Custom formula
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Survived)),
              expected = ~ Class + Sex) +
  scale_fill_residual()

# Saturated model (diagnostic check)
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "saturated") +
  scale_fill_residual()
```

### Advanced Usage

```r
# Conditional independence
ggplot(data = happy) +
  geom_mosaic(aes(weight = wtssall,
                  x = product(health, marital),
                  conds = sex),
              expected = "conditional") +
  scale_fill_residual()

# Custom colors and limits
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)),
              expected = "independence") +
  scale_fill_residual(low = "dodgerblue4",
                      high = "darkred",
                      limits = c(-5, 5))

# Manual residual mapping
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex),
                  fill = after_stat(.residual)),
              expected = "independence") +
  scale_fill_gradient2()
```

## Interpreting Results

### Pearson Residuals

- **Residual ≈ 0**: Observed count matches expected (white)
- **Residual > 0**: More observations than expected (red shades)
- **Residual < 0**: Fewer observations than expected (blue shades)
- **|Residual| > 2**: Statistically significant at ~5% level
- **|Residual| > 3**: Strong evidence against the model

### Model Shortcuts

1. **"independence"**: Complete independence
   - Formula: `~ A + B + C` (all main effects, no interactions)
   - Tests whether variables are associated

2. **"saturated"**: Perfect fit
   - Formula: `~ A * B * C` (all possible interactions)
   - Should show residuals ≈ 0 (diagnostic check)

3. **"conditional"**: Conditional independence
   - Requires `conds` aesthetic
   - Tests if margin variables are independent given conditions

## Testing Checklist

To verify the implementation works correctly:

- [ ] Load package: `devtools::load_all()`
- [ ] Test independence shortcut
- [ ] Test saturated shortcut
- [ ] Test conditional shortcut (with conds)
- [ ] Test custom formula
- [ ] Test with weighted data
- [ ] Test backward compatibility (expected = NULL)
- [ ] Test fill aesthetic precedence
- [ ] Test error handling (bad formula, convergence failure)
- [ ] Test with various datasets (titanic, happy, HairEyeColor)
- [ ] Compare residuals with vcd package (validation)

## Next Steps

### Recommended Testing

1. **Unit Tests**: Create `tests/testthat/test-loglinear.R`
   - Test shortcut_to_formula() logic
   - Test model fitting with known datasets
   - Test error conditions

2. **Integration Tests**:
   - Compare with vcd::mosaic() results
   - Verify residuals match expected values

3. **Visual Tests**:
   - Add to `visual_test/mosaic.R`
   - Create reference plots for regression testing

### Documentation

1. **Vignette**: Add section to existing vignette or create new one
   - "Statistical Modeling with geom_mosaic"
   - Interpretation guide
   - Comparison with vcd package

2. **README**: Add example to package README
   - Show residual shading as a key feature

### Future Enhancements

Potential additions (not currently implemented):

1. **Other residual types**:
   - Standardized residuals
   - Deviance residuals
   - Adjusted residuals

2. **Significance highlighting**:
   - Automatic borders for |residual| > 2
   - Star notation for significant cells

3. **Model diagnostics**:
   - Return model object for further analysis
   - Goodness-of-fit statistics
   - AIC-based model selection

4. **Performance optimization**:
   - Cache fitted models
   - Parallel model fitting for faceted plots

## Comparison with vcd Package

### Similarities

- Both use Poisson GLM for loglinear models
- Both calculate Pearson residuals
- Both support custom model formulas
- Both provide diverging color scales

### Differences

| Feature | ggmosaic | vcd |
|---------|----------|-----|
| Integration | ggplot2 layer system | Base graphics |
| Parameter name | `expected` | `expected` |
| Shortcuts | 3 options | More shading schemes |
| Customization | Full ggplot2 ecosystem | Limited |
| Learning curve | Steeper (ggplot2 syntax) | Easier (base R) |
| Flexibility | Higher (composable layers) | Lower |

### Advantages of ggmosaic implementation

1. **Composability**: Works with facets, themes, scales
2. **Consistency**: Same syntax as other ggplot2 geoms
3. **Customization**: Full access to ggplot2 features
4. **Modern workflow**: Integrates with tidyverse

## Implementation Notes

### Design Decisions

1. **Auto-mapping residuals to fill**: Only when fill not explicitly set
   - Rationale: Preserve user control while providing sensible defaults
   - User can override with explicit fill aesthetic

2. **Error handling**: Fail gracefully with warnings
   - Rationale: Don't break plots when model doesn't converge
   - Fall back to observed values only

3. **Machine epsilon protection**: Prevent division by zero
   - Rationale: Handle sparse tables with zero expected counts
   - Use `.Machine$double.eps` as minimum

4. **No new dependencies**: Use only existing imports
   - Rationale: Keep package lightweight
   - stats::glm() is base R

### Code Quality

- All functions documented with roxygen2
- Error messages are informative
- Edge cases handled (zero cells, convergence failures)
- Follows ggmosaic coding style
- No breaking changes to existing functionality

## Summary

This implementation successfully adds loglinear model residual shading to ggmosaic, providing:

* ✅ Three model shortcuts (independence, saturated, conditional)
* ✅ Custom formula support
* ✅ Pearson residual calculation
* ✅ Diverging color scale
* ✅ Automatic fill mapping with user override
* ✅ Full backward compatibility
* ✅ No new dependencies
* ✅ Comprehensive documentation and examples

The feature is ready for testing and can be considered for inclusion in the next ggmosaic release.
