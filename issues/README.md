# Loglinear Model Residual Shading for ggmosaic

This folder contains the implementation, documentation, and examples for the new loglinear model residual shading feature in ggmosaic.

## Quick Start

### 1. Load the Package

From the ggmosaic package directory:

```r
devtools::load_all()
```

### 2. Run Quick Test

```r
source("issues/quick-test.R")
```

This will run 5 basic tests to verify the implementation works.

### 3. Try an Example

```r
library(ggmosaic)
data(titanic)

ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual() +
  theme_mosaic()
```

## Files in This Folder

| File | Purpose |
|------|---------|
| `README.md` | This file - quick start guide |
| `implementation-summary.md` | Comprehensive technical documentation |
| `loglinear-residual-shading-plan.md` | Original implementation plan |
| `residual-shading-examples.R` | 10 detailed examples with explanations |
| `quick-test.R` | Quick verification script (5 tests) |

## What Was Implemented

### New Parameter: `expected`

Added to `geom_mosaic()` and `stat_mosaic()`:

```r
geom_mosaic(aes(...), expected = "independence")  # Shortcut
geom_mosaic(aes(...), expected = ~ A + B)         # Formula
geom_mosaic(aes(...), expected = NULL)            # Default (no model)
```

### Three Shortcuts

1. **"independence"**: All variables are independent
   - Formula: `~ A + B + C` (main effects only)
   - Tests for association between variables

2. **"saturated"**: Perfect fit
   - Formula: `~ A * B * C` (all interactions)
   - Diagnostic check (should show near-zero residuals)

3. **"conditional"**: Conditional independence
   - Requires `conds` aesthetic
   - Tests if margin variables are independent given conditions

### New Function: `scale_fill_residual()`

Diverging color scale for Pearson residuals:
- Blue = fewer observations than expected
- White = matches expected
- Red = more observations than expected

```r
scale_fill_residual(low = "steelblue",    # Negative residuals
                    mid = "white",         # Zero residuals
                    high = "firebrick",    # Positive residuals
                    limits = NULL)         # Auto or custom range
```

## How It Works

1. **Model Fitting**: Fits Poisson GLM to contingency table
2. **Expected Values**: Calculates fitted values under the model
3. **Residuals**: Computes Pearson residuals `(obs - exp) / sqrt(exp)`
4. **Mapping**: Maps residuals to fill aesthetic (auto or manual)
5. **Visualization**: Applies diverging color scale

## Examples by Use Case

### Testing Independence

```r
# Are Class and Sex independent?
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual()
```

### Custom Model

```r
# Test specific hypothesis
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Survived)),
              expected = ~ Class + Sex) +  # Class and Sex independent
  scale_fill_residual()
```

### Weighted Survey Data

```r
# With survey weights
data(happy)
ggplot(data = happy) +
  geom_mosaic(aes(weight = wtssall,
                  x = product(happy, health)),
              expected = "independence",
              na.rm = TRUE) +
  scale_fill_residual()
```

### Conditional Independence

```r
# Are health and marital independent given sex?
ggplot(data = happy) +
  geom_mosaic(aes(weight = wtssall,
                  x = product(health, marital),
                  conds = sex),
              expected = "conditional") +
  scale_fill_residual()
```

## Interpreting Residuals

### Color Meaning

- **Blue tiles**: Fewer observations than expected (negative association)
- **White tiles**: Observed matches expected (no evidence against model)
- **Red tiles**: More observations than expected (positive association)

### Magnitude

- **|Residual| < 2**: Not statistically significant
- **|Residual| > 2**: Significant at ~5% level (strong color)
- **|Residual| > 3**: Very strong evidence against model (very strong color)

### Model Interpretation

- **Many white tiles**: Model fits data well
- **Scattered colored tiles**: Model mostly fits with some deviations
- **Strong patterns of color**: Model doesn't fit (variables are associated)

## Advanced Usage

### Manual Residual Mapping

```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex),
                  fill = after_stat(.residual)),
              expected = "independence") +
  scale_fill_gradient2(low = "purple", high = "orange")
```

### Combining with Other Aesthetics

```r
# Fill aesthetic takes precedence, but residuals are still calculated
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex),
                  fill = Survived),  # Explicit fill
              expected = "independence") +
  labs(subtitle = "Residuals calculated but not shown")
```

### Custom Color Limits

```r
# Emphasize strong deviations
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)),
              expected = "independence") +
  scale_fill_residual(limits = c(-5, 5))  # Saturate at ±5
```

## Backward Compatibility

**All existing code works unchanged!**

```r
# This still works exactly as before (expected defaults to NULL)
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex), fill = Survived))
```

No performance overhead when `expected = NULL`.

## Testing

### Quick Test (5 tests)

```r
source("issues/quick-test.R")
```

### Comprehensive Examples (10 examples)

```r
source("issues/residual-shading-examples.R")
```

### What to Check

- ✓ Diverging colors appear correctly (blue/white/red)
- ✓ Independence model shows colored tiles for associated variables
- ✓ Saturated model shows near-white tiles (residuals ≈ 0)
- ✓ Backward compatibility (no `expected` parameter works as before)
- ✓ Custom formulas work correctly
- ✓ Weighted data works
- ✓ Error messages are informative

## Troubleshooting

### Model Doesn't Converge

**Symptom**: Warning message "Loglinear model fitting failed"

**Causes**:
- Sparse contingency table (many zero cells)
- Too many variables relative to sample size
- Separation in the data

**Solutions**:
- Simplify the model (fewer interactions)
- Combine small categories
- Use a different model specification

### All Residuals Are Zero

**Symptom**: Plot is entirely white

**Causes**:
- Using saturated model (expected behavior)
- Model is too complex (overfitting)

**Solutions**:
- Use simpler model (e.g., "independence")
- Check that you have enough data

### Colors Don't Show

**Symptom**: No diverging colors visible

**Causes**:
- Fill aesthetic explicitly set (overrides residuals)
- Forgot to add `scale_fill_residual()`
- Very small residuals (model fits well)

**Solutions**:
- Remove explicit fill aesthetic
- Add `scale_fill_residual()`
- Adjust color limits: `scale_fill_residual(limits = c(-2, 2))`

## Next Steps

### For Users

1. Run `issues/quick-test.R` to verify installation
2. Explore `issues/residual-shading-examples.R` for ideas
3. Try with your own data
4. Report any issues or unexpected behavior

### For Developers

1. Add unit tests (`tests/testthat/test-loglinear.R`)
2. Add visual regression tests
3. Create vignette section
4. Update package README
5. Compare results with vcd package

## Comparison with vcd

### When to Use ggmosaic

- ✓ Want ggplot2 integration
- ✓ Need faceting or complex layouts
- ✓ Prefer tidyverse workflow
- ✓ Want customization flexibility

### When to Use vcd

- ✓ Prefer base R graphics
- ✓ Need more shading schemes
- ✓ Want simpler syntax
- ✓ Need other vcd-specific features

Both packages calculate residuals the same way (Pearson residuals from Poisson GLM).

## Technical Details

See `implementation-summary.md` for:
- Architecture overview
- Data flow diagrams
- Function documentation
- Design decisions
- Code structure

## License

This implementation follows the ggmosaic package license (GPL >= 2).

## Contributors

Implementation by Claude (Anthropic) based on user requirements and existing ggmosaic architecture.

## Questions?

Check the documentation:
- This README for quick start
- `implementation-summary.md` for technical details
- `residual-shading-examples.R` for usage examples
