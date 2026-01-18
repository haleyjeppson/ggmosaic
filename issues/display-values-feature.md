# Display Values Feature for geom_mosaic_text()

## Overview

Extended `geom_mosaic_text()` to display numeric values in mosaic cells, similar to vcd's `labeling_values`. This allows users to show observed counts, expected values, or Pearson residuals directly in the visualization.

## New Parameters

### `display_values`
Character string specifying what to display in cells:
- `"label"` (default) - Factor labels (original behavior)
- `"observed"` - Observed counts (`.n` column)
- `"expected"` - Expected values from fitted model
- `"residual"` - Pearson residuals

### `format_digits`
Number of decimal places for formatting numeric values (default: 1). Only used when `display_values` is not "label".

## Usage Examples

### Display Observed Counts

```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex), fill = Survived)) +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   display_values = "observed",
                   format_digits = 0)
```

### Display Residuals with Shading

```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   display_values = "residual",
                   format_digits = 2,
                   colour = "white")
```

### Display Expected Values

```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   display_values = "expected",
                   format_digits = 1)
```

## Implementation Details

### Modified Files

**R/geom-mosaic-text.R**:
- Added `display_values` and `format_digits` parameters to `geom_mosaic_text()`
- Modified `GeomMosaicText$draw_panel()` to:
  1. Check which value type to display
  2. Format numeric values appropriately
  3. Create `display_text` column
  4. Rename to `label` for GeomText/GeomLabel compatibility

### How It Works

1. **Parameter validation**: `match.arg()` ensures valid `display_values` choice
2. **Value selection**:
   - If `"observed"`: Use `.n` column (always available)
   - If `"expected"`: Use `.expected` column (requires `expected` in `geom_mosaic()`)
   - If `"residual"`: Use `.residual` column (requires `expected` in `geom_mosaic()`)
   - If `"label"`: Use `label` column (default)
3. **Formatting**: Apply `round()` and `format()` with specified digits
4. **Warning**: If `expected` or `residual` requested but not available, warn and show empty string

### Error Handling

If user requests `"expected"` or `"residual"` but hasn't specified `expected` parameter in `geom_mosaic()`:

```r
Warning: Expected values not available. Use 'expected' parameter in geom_mosaic().
# or
Warning: Residuals not available. Use 'expected' parameter in geom_mosaic().
```

Cells will show empty strings in this case.

## Comparison with vcd

### vcd Approach

```r
library(vcd)
mosaic(~ Class + Sex, data = Titanic,
       labeling = labeling_values,
       value_type = "observed")
```

### ggmosaic Approach

```r
library(ggmosaic)
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex))) +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   display_values = "observed")
```

### Advantages of ggmosaic Implementation

1. **Layer-based**: Can combine with other geoms/annotations
2. **Full ggplot2 control**: Size, color, repel, labels, etc.
3. **Integrated with residual shading**: Works seamlessly with `expected` parameter
4. **Consistent syntax**: Follows ggplot2 conventions

## Use Cases

### 1. Publication-Ready Count Tables

Show exact counts in cells for readers:

```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived), fill = Survived)) +
  geom_mosaic_text(aes(x = product(Class, Survived)),
                   display_values = "observed",
                   format_digits = 0,
                   size = 4) +
  theme_mosaic()
```

### 2. Model Diagnostics

Show residual magnitudes alongside color coding:

```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   display_values = "residual",
                   format_digits = 2)
```

### 3. Educational Materials

Compare observed vs expected side-by-side:

```r
# Observed
p1 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)), expected = "independence") +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   display_values = "observed") +
  ggtitle("Observed")

# Expected
p2 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)), expected = "independence") +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   display_values = "expected") +
  ggtitle("Expected under Independence")

gridExtra::grid.arrange(p1, p2, ncol = 2)
```

## Testing

Run the comprehensive test script:

```r
source("issues/test-display-values.R")
```

This demonstrates:
- All four display_values options
- Different format_digits settings
- Integration with residual shading
- Use with repel and labels
- Multi-way tables
- Custom models

## Backward Compatibility

Fully backward compatible. Default `display_values = "label"` preserves original behavior of showing factor labels.

## Future Enhancements

Potential additions:
- `"count_percent"` - Show "n (xx%)"
- `"stars"` - Show significance stars based on residual magnitude
- Custom format functions via parameter
- Automatic color contrast (white text on dark cells, black on light)
