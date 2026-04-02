# Theme for mosaic plots

Themes set the general aspect of the plot such as the colour of the
background, gridlines, the size and colour of fonts. `theme_mosaic`
provides access to the regular ggplot2 theme, but removes any
background, most of the gridlines, and ensures an aspect ratio of 1 for
better viewing of the mosaics.

## Arguments

- base_size:

  base font size

- base_family:

  base font family

## Examples

``` r
library(ggmosaic)
data(happy)
ggplot(data = happy) +
  geom_mosaic(aes(weight=wtssall, x=product(health), fill=happy), na.rm=TRUE) +
  theme_mosaic()

```
