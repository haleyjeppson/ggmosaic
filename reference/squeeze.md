# Internal helper function

Squeeze pieces to lie within specified bounds; directly copied from
package productplots

## Usage

``` r
squeeze(pieces, bounds = bound())
```

## Arguments

- pieces:

  rectangle specified via l(eft), r(ight), b(ottom), t(op)

- bounds:

  rectangle specified via l(eft), r(ight), b(ottom), t(op)

## Value

re-scaled values for piece according to boundaries given by bounds

## Author

Hadley Wickham
