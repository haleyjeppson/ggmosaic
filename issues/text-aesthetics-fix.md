# Text Aesthetics Fix for geom_mosaic_text()

## Problem

User reported warning when using text aesthetics like `fontface`:
```
Warning: Ignoring unknown parameters: `fontface`
```

## Root Cause

Text aesthetics (`fontface`, `family`, `angle`, `hjust`, `vjust`, `lineheight`) were hardcoded in the `draw_panel()` function instead of being controllable parameters.

**Location**: R/geom-mosaic-text.R, lines 260-266

**Before**:
```r
data.frame(
  x = (d$xmin + d$xmax)/2,
  y = (d$ymin + d$ymax)/2,
  angle = 0,              # Hardcoded!
  hjust = 0.5,            # Hardcoded!
  vjust = 0.5,            # Hardcoded!
  alpha = NA,
  family = "",            # Hardcoded!
  fontface = 1,           # Hardcoded!
  lineheight = 1.2,       # Hardcoded!
  dplyr::select(d, -any_of(c("x", "y", "alpha")))
)
```

## Solution

### 1. Added Text Aesthetics to default_aes

**File**: R/geom-mosaic-text.R, line 210

Added text aesthetics with sensible defaults:
```r
default_aes = ggplot2::aes(
  ...,
  angle = 0, hjust = 0.5, vjust = 0.5,
  family = "", fontface = 1, lineheight = 1.2
)
```

### 2. Use Data Values Instead of Hardcoded Values

**File**: R/geom-mosaic-text.R, lines 257-268

**After**:
```r
data.frame(
  x = (d$xmin + d$xmax)/2,
  y = (d$ymin + d$ymax)/2,
  angle = d$angle[1],           # From data
  hjust = d$hjust[1],           # From data
  vjust = d$vjust[1],           # From data
  alpha = NA,
  family = d$family[1],         # From data
  fontface = d$fontface[1],     # From data
  lineheight = d$lineheight[1], # From data
  dplyr::select(d, -any_of(c("x", "y", "alpha", "angle", "hjust", "vjust",
                             "family", "fontface", "lineheight")))
)
```

### 3. Updated Documentation

**File**: R/geom-mosaic-text.R, lines 34-37

Added clear documentation of controllable text aesthetics:
```r
#' @param ... other arguments passed on to \code{layer}. These are often aesthetics,
#'   used to set an aesthetic to a fixed value, like \code{color = 'red'} or \code{size = 3}.
#'   Text aesthetics that can be controlled include: \code{size} (default: 2.7),
#'   \code{colour}/\code{color}, \code{fontface} ('plain', 'bold', 'italic', 'bold.italic'),
#'   \code{family} (font family), \code{angle} (rotation in degrees),
#'   \code{hjust}/\code{vjust} (justification), and \code{lineheight}.
```

## Usage Examples

All text aesthetics now work correctly:

```r
# Bold text
geom_mosaic_text(aes(x = product(Class, Sex)),
                 display_values = "observed",
                 size = 4,
                 fontface = "bold")

# Italic serif font
geom_mosaic_text(aes(x = product(Class, Sex)),
                 display_values = "observed",
                 size = 3.5,
                 fontface = "italic",
                 family = "serif")

# Rotated text
geom_mosaic_text(aes(x = product(Class, Sex)),
                 angle = 45)
```

## Testing

Updated `issues/test-text-size.R` with 6 examples demonstrating:
1. Default size (2.7)
2. Small text (size = 2)
3. Large text (size = 5)
4. Bold white text (fontface = "bold")
5. Residuals with custom size
6. Italic serif font (fontface = "italic", family = "serif")

## Result

✅ Warning eliminated
✅ All text aesthetics now controllable
✅ Backward compatible (defaults unchanged)
✅ Properly documented
