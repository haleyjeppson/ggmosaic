# Add Loglinear Model Residual Shading to ggmosaic

## Overview

This PR extends `geom_mosaic()` to support **loglinear model fitting and residual-based shading**, bringing extended mosaic plot functionality (similar to `vcd::mosaic()` and `vcdExtra::mosaic.glm()`) to the ggplot2 framework.

## Motivation

Mosaic plots are powerful for visualizing categorical data, but basic plots only show observed frequencies, using "marimekko" shading. To understand whether patterns are statistically meaningful, we need to compare observed frequencies with expected values under a model of independence or association.

Extended mosaic plots fit a loglinear model to contingency tables and shade tiles according to **Pearson residuals**:

$$r = \frac{\text{observed} - \text{expected}}{\sqrt{\text{expected}}}$$

- **Blue tiles**: Observed > Expected (positive association)
- **Red tiles**: Observed < Expected (negative association)
- **Intensity**: Magnitude of residual (|r| > 2 ≈ significant at α = 0.05)

(**Note**: `vcdExtra::mosaic.glm()` allows `residuals_type = c("pearson", "deviance", "rstandard")`; these could be added later here.)

This approach enables:
- Visual hypothesis testing for categorical associations
- Model comparison and diagnostics
- Publication-ready statistical visualizations
- Integration with ggplot2's layer system


## Main Features

### 1. Loglinear Model Specification (`expected` parameter)

Three convenient shortcuts:
```r
# Complete independence
expected = "independence"  # ~ A + B + C

# Saturated model
expected = "saturated"     # ~ A * B * C

# Conditional independence
expected = "conditional"   # ~ A + B + C + A:C + B:C (for conditioned models)
```

Plus custom formula support:
```r
# Test if C is independent of A×B
expected = ~ A + B
```

**Note**: `vcdExtra` defines a variety of other shortcuts for constructing and manipulating loglinear models:
`vcdExtra::joint()`, `vcdExtra::conditional()`, `vcdExtra::mutual()`, `vcdExtra::saturated()` which could be useful here.

### 2. Residual Shading Scale

New `scale_fill_residual()` provides diverging color scale. The defaults use
a gradient from `low = "darkblue", mid = "white", high = "darkred"`,


```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)),
              expected = "independence") +
  scale_fill_residual()
```

### 3. Cell Value Display

I also extended `geom_mosaic_text()` to show observed/expected/residual values:

```r
geom_mosaic_text(aes(x = product(Class, Sex)),
                 expected = "independence",
                 display_values = "residual",  # or "observed", "expected", "label"
                 format_digits = 2)
```

### 4. Enhanced Text Aesthetics

All text aesthetics now controllable in `geom_mosaic_text()`:
- `size`, `colour`, `fontface`, `family`, `angle`, `hjust`, `vjust`, `lineheight`

## Complete Example

```r
library(ggmosaic)

# Extended mosaic plot with residual shading
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   expected = "independence",
                   display_values = "residual",
                   format_digits = 2,
                   size = 4,
                   fontface = "bold") +
  labs(title = "Titanic: Class × Sex Association",
       subtitle = "Pearson residuals from independence model") +
  theme_mosaic()
```

## Implementation Details

### New Files

**R/loglinear.R** - Core model fitting infrastructure
- `shortcut_to_formula()`: Translate shortcuts to formulas
- `build_model_formula()`: Map user variables to internal prefixed names
- `fit_loglinear_model()`: Fit Poisson GLM and calculate Pearson residuals

**R/scale-residual.R** - Diverging color scale
- `scale_fill_residual()`: Convenience scale for residual visualization
- `scale_fill_residuals()`: Alias for consistency

**vignettes/loglinear-models.Rmd** - Comprehensive tutorial
- Introduction to extended mosaic plots
- Model specification examples
- Interpretation guidelines
- Complete workflows

### Modified Files

**R/calculate.R**
- Added `expected` parameter to `prodcalc()`
- Integration point for model fitting after observed counts calculation

**R/stat-mosaic.R**
- Thread `expected` parameter through stat layer
- Auto-map residuals to fill when appropriate
- Pass model specification to `prodcalc()`

**R/geom-mosaic.R**
- Added `expected` parameter to user interface
- Updated documentation with examples
- Preserved backward compatibility (default: `expected = NULL`)

**R/geom-mosaic-text.R**
- Added `display_values` parameter: "label", "observed", "expected", "residual"
- Added `format_digits` parameter for numeric formatting
- Added `expected` parameter for model specification
- Enhanced text aesthetics (fontface, family, angle, hjust, vjust, lineheight)
- Fixed hardcoded text aesthetic values to use data values

**NAMESPACE**
- Exported new functions: `scale_fill_residual()`, `scale_fill_residuals()`

## Documentation

Comprehensive documentation in [`issues/`](https://github.com/friendly/ggmosaic/tree/master/issues) folder:

### Implementation Documentation
- [**implementation-summary.md**](https://github.com/friendly/ggmosaic/blob/master/issues/implementation-summary.md) - Complete technical documentation (600+ lines)
- [**loglinear-residual-shading-plan.md**](https://github.com/friendly/ggmosaic/blob/master/issues/loglinear-residual-shading-plan.md) - Original implementation plan
- [**text-aesthetics-fix.md**](https://github.com/friendly/ggmosaic/blob/master/issues/text-aesthetics-fix.md) - Text aesthetics enhancement details

### User Documentation
- [**loglinear-models-quick-reference.md**](https://github.com/friendly/ggmosaic/blob/master/issues/loglinear-models-quick-reference.md) - Quick reference guide with common patterns
- [**vignette-summary.md**](https://github.com/friendly/ggmosaic/blob/master/issues/vignette-summary.md) - Vignette overview
- [**display-values-feature.md**](https://github.com/friendly/ggmosaic/blob/master/issues/display-values-feature.md) - Display values feature documentation

### Examples and Comparisons
- [**residual-shading-examples.R**](https://github.com/friendly/ggmosaic/blob/master/issues/residual-shading-examples.R) - 10 comprehensive examples
- [**quick-test.R**](https://github.com/friendly/ggmosaic/blob/master/issues/quick-test.R) - 5 basic verification tests
- [**vcd-comparison.R**](https://github.com/friendly/ggmosaic/blob/master/issues/vcd-comparison.R) - Side-by-side comparison with vcd package
- [**test-display-values.R**](https://github.com/friendly/ggmosaic/blob/master/issues/test-display-values.R) - 8 examples of display_values feature
- [**test-text-size.R**](https://github.com/friendly/ggmosaic/blob/master/issues/test-text-size.R) - Text aesthetic examples

### Future Development
- [**vignette-outline-extended-topics.md**](https://github.com/friendly/ggmosaic/blob/master/issues/vignette-outline-extended-topics.md) - Roadmap for advanced topics

## Backward Compatibility

✅ **Fully backward compatible**

All new features default to NULL or original behavior:
- `expected = NULL` (default) - No model fitting, original behavior preserved
- `display_values = "label"` (default) - Shows factor labels as before
- All text aesthetics have defaults matching original implementation

Existing code will work unchanged.

## Testing

### Manual Testing
All examples in the following test files pass:
- `issues/quick-test.R` - Basic functionality
- `issues/residual-shading-examples.R` - Comprehensive scenarios
- `issues/test-display-values.R` - Display values feature
- `issues/test-text-size.R` - Text aesthetics
- `issues/vcd-comparison.R` - Validation against vcd package

### Vignette
Comprehensive vignette builds successfully:
```r
rmarkdown::render("vignettes/loglinear-models.Rmd")
```

### Edge Cases Handled
- Zero expected values (machine epsilon protection)
- Model convergence failures (wrapped in `tryCatch()` with warnings)
- Variable name mapping (user names → internal prefixed names)
- Missing expected/residual columns (informative warnings)

## Commit History

Starting from commit `ba2e6f6` (initial plan):

1. **ba2e6f6** - Initial plan for loglinear residual shading
2. Core loglinear model infrastructure implementation
3. Display values feature for `geom_mosaic_text()`
4. Text aesthetics enhancement and fixes
5. Comprehensive documentation and examples
6. Vignette development
7. Final polish and testing

## Examples in Action

### Basic Independence Test
```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)),
              expected = "independence") +
  scale_fill_residual()
```

### Three-Way Table with Residuals
```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Survived)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex, Survived)),
                   expected = "independence",
                   display_values = "residual",
                   format_digits = 1,
                   size = 2.5)
```

### Custom Model
```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Survived)),
              expected = ~ Class + Sex) +
  scale_fill_residual() +
  labs(title = "Testing Survival Independence Given Class × Sex")
```

### Display Observed Counts
```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex), fill = Survived)) +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   display_values = "observed",
                   format_digits = 0,
                   size = 4)
```

## Benefits

### For Users
- Seamless integration of statistical testing with visualization
- Intuitive color coding (blue = more than expected, red = less)
- Publication-ready plots with customizable aesthetics
- ggplot2 native (layers, themes, scales all work)

### For Researchers
- Visual hypothesis testing for categorical data
- Model comparison capabilities
- Residual diagnostics
- Similar functionality to `vcd::mosaic()` with ggplot2 flexibility

### For Package Ecosystem
- Bridges gap between exploratory visualization and statistical inference
- Complements existing categorical data analysis packages (vcd, vcdExtra)
- Leverages ggplot2's extensive ecosystem

## Dependencies

No new package dependencies required:
- `stats::glm()` for model fitting (base R)
- `dplyr` for data manipulation (already imported)
- `ggplot2` for scales and layers (already imported)

## Future Work (Not in This PR)

Potential enhancements outlined in [vignette-outline-extended-topics.md](https://github.com/friendly/ggmosaic/blob/master/issues/vignette-outline-extended-topics.md):
- Goodness-of-fit statistics integration
- Model comparison tools
- Additional residual types (standardized, adjusted)
- Integration with `broom` for tidy output
- Survey weight support
- Interactive plots with plotly

## Related Issues

This PR addresses common user requests for:
- Statistical significance visualization in mosaic plots
- Residual shading similar to vcd package
- Displaying counts/values in cells
- Enhanced text formatting control

## Checklist

- [x] Code follows package style conventions
- [x] All functions documented with roxygen2
- [x] Examples provided and tested
- [x] Backward compatibility maintained
- [x] Vignette created and builds successfully
- [x] No new dependencies added
- [x] Edge cases handled with appropriate warnings
- [x] Manual testing completed
- [x] Comprehensive documentation in issues/ folder

## Acknowledgments

This work was inspired by:
- Meyer, Zeileis, & Hornik (2006) - vcd package strucplot framework
- Friendly (1994) - Mosaic displays for multi-way contingency tables
- Hartigan & Kleiner (1981) - Original mosaic plot methodology

## Questions for Reviewers

1. **Naming**: Is `expected` the best parameter name, or would `model` be clearer?
2. **Defaults**: Should residual shading auto-apply when `expected` is specified, or require explicit `scale_fill_residual()`?
3. **Documentation**: Any additional examples needed in function documentation?
4. **Vignette**: Should this be split into multiple vignettes (basic + advanced)?

## Try It Out

To test this PR locally:
```r
# Install from this branch
devtools::install_github("friendly/ggmosaic")

# Load and try examples
library(ggmosaic)
data(titanic)

# Basic residual shading
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)),
              expected = "independence") +
  scale_fill_residual()

# With residual values
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   expected = "independence",
                   display_values = "residual",
                   format_digits = 2)

# View comprehensive vignette
vignette("loglinear-models", package = "ggmosaic")
```

---

**Author**: Michael Friendly ([@friendly](https://github.com/friendly))
**Commits**: 7 commits starting from ba2e6f6
**Lines Changed**: ~2000+ (estimates: 1500 additions, 100 deletions, 400+ documentation)
