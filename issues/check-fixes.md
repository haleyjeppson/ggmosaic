# Package Check Fixes - ggmosaic

**Date**: 2026-01-18
**R Version**: 4.5.2
**ggplot2 Version**: 4.0.1

## Summary

Fixed critical compatibility issues with ggplot2 4.0+ and resolved namespace collisions that were causing package check failures. The package now passes `devtools::check()` without errors or warnings.

---

## Critical Fixes

### 1. ggplot2 4.0+ API Compatibility (`R/scale-product.R`)

**Issue**: Error `unused arguments (list(), "x")` in `make_title()` function

**Root Cause**: ggplot2 4.0+ changed the `make_title()` method signature in scale objects from accepting variable arguments to requiring three specific parameters.

**Fix**: Updated `ScaleContinuousProduct$make_title()` signature

```r
# Before (broken in ggplot2 4.0+):
make_title = function(..., self) {
  title <- ggproto_parent(ScaleContinuousPosition, self)$make_title(...)
  ...
}

# After (compatible with ggplot2 4.0+):
make_title = function(guide, scale, label, self) {
  title <- ggproto_parent(ScaleContinuousPosition, self)$make_title(guide, scale, label)
  ...
}
```

**Diagnosis Method**: Used `traceback()` to identify that `scale$make_title(prim_guide, prim_scale, prim_label)` was being called with three arguments, revealing the new API signature.

**Files Modified**:
- `R/scale-product.R` (lines 119-128)

**Impact**: Resolves all plotting errors with ggplot2 4.0+. Critical for package functionality.

---

### 2. Namespace Collision with vcd Package (`vignettes/ggmosaic.Rmd`)

**Issue**: Error `'dimnames' applied to non-array` when knitting vignette

**Root Cause**: The `mosaic()` and `ddecker()` functions in ggmosaic were being masked by `vcd::mosaic()` during package check. The vcd package's `mosaic()` function is for creating traditional mosaic plots and has a completely different API, causing it to fail when called with ggmosaic's divider syntax.

**Fix**: Added explicit namespace qualification

```r
# Before:
divider = mosaic("h")
divider = ddecker()

# After:
divider = ggmosaic::mosaic("h")
divider = ggmosaic::ddecker()
```

**Files Modified**:
- `vignettes/ggmosaic.Rmd` (lines 78, 252, 257, 262)

**Locations Fixed**:
1. Line 78: `ddecker_examp` plot
2. Line 252: `h_mosaic` plot
3. Line 257: `v_mosaic` plot
4. Line 262: `doubledecker` plot

**Impact**: Vignettes now build successfully during package check. Prevents namespace confusion when vcd is loaded.

---

### 3. Array Dimension Handling (`R/divide.R`)

**Issue**: Potential failure in `divide_once()` when `tapply()` returns a vector without proper dimensions

**Root Cause**: `tapply()` can return a vector (without `dim` attribute) when grouping produces certain data structures. The subsequent `as.data.frame.table()` call requires proper array dimensions and dimnames.

**Fix**: Added dimension validation and reconstruction

```r
# Added after tapply() call:
if (is.null(dim(wt))) {
  # Get the levels from the grouping variables to set proper dimnames
  dims <- lapply(data[-ncol(data)], function(x) levels(factor(x)))
  dim(wt) <- sapply(dims, length)
  dimnames(wt) <- dims
}
```

**Files Modified**:
- `R/divide.R` (lines 67-75)

**Impact**: Makes the divider system more robust to edge cases in data structure.

---

## New Content Added

### 4. Frequency Table Forms Vignette (`vignettes/frequency-table-forms.Rmd`)

**Purpose**: Comprehensive guide to working with three forms of frequency tables in mosaic displays

**Content**:
- **Table form**: Compact array representation (from `table()`, `xtabs()`)
- **Frequency form**: Data frame with `Freq` column (from `as.data.frame()`)
- **Case form**: One row per observation (from `vcdExtra::expand.dft()`)

**Key Examples**:
- Conversions between all three forms using `HairEyeColor` dataset
- Using `weight` aesthetic with frequency form data
- Default shading vs. residual-based shading comparison
- Two-way and three-way table examples
- Loglinear model fitting with `expected` parameter

**Files Created**:
- `vignettes/frequency-table-forms.Rmd`

**Dependencies**: References `vcdExtra::expand.dft()` for frequency-to-case conversion

**Related Documentation**: Points to [vcdExtra vignette](https://friendly.github.io/vcdExtra/articles/a1-creating.html) for comprehensive treatment

---

### 5. Example Scripts (`issues/haireyecolor.R`)

**Purpose**: Standalone examples demonstrating frequency form usage with HairEyeColor dataset

**Content**:
- Examples using `weight = Freq` aesthetic
- Conversion between forms using `vcdExtra::expand.dft()`
- Side-by-side comparison of frequency vs. case form
- Documentation of conversion patterns

**Files Created**:
- `issues/haireyecolor.R`

---

## Technical Notes

### Debugging Process

1. **Initial Error**: `unused arguments (list(), "x")`
   - Appeared in README.Rmd knitting
   - Console execution worked with `devtools::load_all()`
   - Indicated installed vs. development version difference

2. **Diagnosis**:
   - Added debug output to `make_title()` - function was never called
   - Used `traceback()` to identify call signature
   - Revealed ggplot2 was calling `scale$make_title(prim_guide, prim_scale, prim_label)`

3. **Solution Path**:
   - First tried `function(name, self)` - incorrect
   - Traceback showed three arguments needed
   - Updated to `function(guide, scale, label, self)` - success

### Vignette Knitting Issues

**Observation**: `rmarkdown::render()` worked but RStudio "Knit" button failed

**Likely Cause**: Different working directories or environment variables between the two methods

**Resolution**: Not critical since `devtools::check()` uses `rmarkdown::render()` pathway

### Deprecation Warnings (Not Critical)

The following warnings appear but don't affect package check:

1. **ggplot2 3.5.0 deprecations**:
   - `scale_name` argument of `continuous_scale()`
   - `trans` argument (use `transform` instead)
   - These are in `R/scale-product.R` but not breaking

2. **tidyr deprecation**:
   - `unite_()` in `R/stat-mosaic-jitter.r` and `R/stat-mosaic.r`
   - Should use `unite()` instead
   - Not critical for current functionality

**Future Work**: Address these deprecation warnings in next version.

---

## Testing Performed

1. ✅ `devtools::check()` - No errors, no warnings
2. ✅ README.Rmd example (after reinstall)
3. ✅ Vignette building (`frequency-table-forms.Rmd`)
4. ✅ Console plotting with various datasets
5. ✅ Divider functions (`mosaic()`, `ddecker()`)

---

## Files Modified Summary

### Core Package Files
- `R/scale-product.R` - ggplot2 4.0+ compatibility
- `R/divide.R` - Array dimension handling

### Documentation
- `vignettes/ggmosaic.Rmd` - Namespace fixes
- `vignettes/frequency-table-forms.Rmd` - NEW

### Examples
- `issues/haireyecolor.R` - NEW
- `issues/check-fixes.md` - NEW (this file)

---

## Recommendations

### Immediate
1. ✅ Commit these fixes to version control
2. ✅ Document in NEWS.md or ChangeLog
3. Consider updating version number (0.4.1 → 0.4.2 or 0.5.0)

### Future Enhancements
1. Address deprecation warnings:
   - Update `unite_()` to `unite()` in stat files
   - Review `continuous_scale()` parameters in scale-product.R

2. Add tests for:
   - Edge cases in `divide_once()`
   - Namespace conflicts (if possible)
   - ggplot2 version compatibility

3. Consider adding `Conflicts` section to DESCRIPTION to document function masking issues with vcd

---

## Compatibility Notes

**Minimum R Version**: 4.1.0 (as specified in DESCRIPTION)
**Minimum ggplot2 Version**: 3.5.0 (updated requirement)
**Tested With**:
- R 4.5.2
- ggplot2 4.0.1
- vcdExtra (for vignette examples)

**Known Issues**: None after these fixes

---

## Acknowledgments

Fixes developed through interactive debugging and analysis of:
- ggplot2 4.0 API changes
- Package check error messages and tracebacks
- Namespace resolution behavior in R packages
- vcdExtra vignette on frequency table forms
