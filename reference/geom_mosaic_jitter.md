# Jittered dots in Mosaic plots.

A mosaic plat with jittered dots

## Usage

``` r
geom_mosaic_jitter(
  mapping = NULL,
  data = NULL,
  stat = "mosaic_jitter",
  position = "identity",
  na.rm = FALSE,
  divider = mosaic(),
  offset = 0.01,
  drop_level = FALSE,
  seed = NA,
  show.legend = NA,
  inherit.aes = FALSE,
  ...
)

stat_mosaic_jitter(
  mapping = NULL,
  data = NULL,
  geom = "mosaic_jitter",
  position = "identity",
  na.rm = FALSE,
  divider = mosaic(),
  show.legend = NA,
  inherit.aes = TRUE,
  offset = 0.01,
  drop_level = FALSE,
  seed = NA,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- na.rm:

  If `FALSE` (the default), removes missing values with a warning. If
  `TRUE` silently removes missing values.

- divider:

  Divider function. The default divider function is mosaic() which will
  use spines in alternating directions. The four options for
  partitioning:

  - `vspine` Vertical spine partition: width constant, height varies.

  - `hspine` Horizontal spine partition: height constant, width varies.

  - `vbar` Vertical bar partition: height constant, width varies.

  - `hbar` Horizontal bar partition: width constant, height varies.

- offset:

  Set the space between the first spine

- drop_level:

  Generate points for the max - 1 level

- seed:

  Random seed passed to
  [`set.seed`](https://rdrr.io/r/base/Random.html). Defaults to `NA`,
  which means that `set.seed` will not be called.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- ...:

  other arguments passed on to `layer`. These are often aesthetics, used
  to set an aesthetic to a fixed value, like `color = 'red'` or
  `size = 3`. They may also be parameters to the paired geom/stat.

- geom:

  The geometric object to use to display the data for this layer. When
  using a `stat_*()` function to construct a layer, the `geom` argument
  can be used to override the default coupling between stats and geoms.
  The `geom` argument accepts the following:

  - A `Geom` ggproto subclass, for example `GeomPoint`.

  - A string naming the geom. To give the geom as a string, strip the
    function name of the `geom_` prefix. For example, to use
    [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html),
    give the geom as `"point"`.

  - For more information and other ways to specify the geom, see the
    [layer
    geom](https://ggplot2.tidyverse.org/reference/layer_geoms.html)
    documentation.

## Computed variables

- xmin:

  location of bottom left corner

- xmax:

  location of bottom right corner

- ymin:

  location of top left corner

- ymax:

  location of top right corner

## Examples

``` r
data(titanic)

ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class), fill = Survived), alpha = 0.3) +
  geom_mosaic_jitter(aes(x = product(Class), color = Survived))


ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class)), alpha = 0.1) +
  geom_mosaic_jitter(aes(x = product(Class), color = Survived), drop_level = TRUE)
#> Joining with `by = join_by(x__Class)`


ggplot(data = titanic) +
  geom_mosaic(alpha = 0.3, aes(x = product(Class, Sex),  fill = Survived),
              divider = c("vspine", "hspine", "hspine")) +
  geom_mosaic_jitter(aes(x = product(Class, Sex), color = Survived),
              divider = c("vspine", "hspine", "hspine"))


 ggplot(data = titanic) +
  geom_mosaic(alpha = 0.3, aes(x = product(Class), conds = product(Sex),  fill = Survived),
              divider = c("vspine", "hspine", "hspine")) +
  geom_mosaic_jitter(aes(x = product(Class), conds = product(Sex), fill = Survived),
              divider = c("vspine", "hspine", "hspine"))
```
