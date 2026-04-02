# Calculate frequencies.

Calculate frequencies.

## Usage

``` r
prodcalc(
  data,
  formula,
  divider = mosaic(),
  cascade = 0,
  scale_max = TRUE,
  na.rm = FALSE,
  offset = offset
)
```

## Arguments

- data:

  input data frame

- formula:

  formula specifying display of plot

- divider:

  divider function

- cascade:

  cascading amount, per nested layer

- scale_max:

  Logical vector of length 1. If `TRUE` maximum values within each
  nested layer will be scaled to take up all available space. If
  `FALSE`, areas will be comparable between nested layers.

- na.rm:

  Logical vector of length 1 - should missing levels be silently
  removed?

## Examples

``` r
if (FALSE) { # \dontrun{
library(productplots)
prodcalc(happy, ~ happy, "hbar", offset = 0.005)
prodcalc(happy, ~ happy, "hspine", offset = 0.01)
} # }
```
