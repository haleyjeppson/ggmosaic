# Labeling for Mosaic plots.

A mosaic plot with text or labels

## Usage

``` r
geom_mosaic_text(
  mapping = NULL,
  data = NULL,
  stat = "mosaic",
  position = "identity",
  na.rm = FALSE,
  divider = mosaic(),
  offset = 0.01,
  show.legend = NA,
  inherit.aes = FALSE,
  as.label = FALSE,
  repel = FALSE,
  repel_params = NULL,
  check_overlap = FALSE,
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

  - `hspine`Horizontal spine partition: height constant, width varies.

  - `vbar` Vertical bar partition: height constant, width varies.

  - `hbar` Horizontal bar partition: width constant, height varies.

- offset:

  Set the space between the first spine

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

- as.label:

  Show as a ggplot label (box with round corners)

- repel:

  Use ggrepel wo labels don't overlap

- repel_params:

  List of ggrepel parameters (e.g. list(point.padding = 0))

- check_overlap:

  If `TRUE`, text that overlaps previous text in the same layer will not
  be plotted. `check_overlap` happens at draw time and in the order of
  the data. Therefore data should be arranged by the label column before
  calling
  [`geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
  or
  [`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html).

- ...:

  other arguments passed on to `layer`. These are often aesthetics, used
  to set an aesthetic to a fixed value, like `color = 'red'` or
  `size = 3`. They may also be parameters to the paired geom/stat.

## Examples

``` r
data(titanic)

ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class), fill = Survived)) +
  geom_mosaic_text(aes(x = product(Class), fill = Survived))


ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex),  fill = Survived),
              divider = c("vspine", "hspine", "hspine")) +
  geom_mosaic_text(aes(x = product(Class, Sex), fill = Survived),
              divider = c("vspine", "hspine", "hspine"), size = 2)


ggplot(data = happy) +
  geom_mosaic(aes(x = product(health), fill = happy),
              na.rm = TRUE,
              show.legend = FALSE) +
  geom_mosaic_text(aes(x = product(happy, health)), na.rm = TRUE)


# avoid overlapping text
ggplot(data = happy) +
  geom_mosaic(aes(x = product(health), fill = happy),
              na.rm = TRUE, show.legend = FALSE) +
  geom_mosaic_text(aes(x = product(happy, health)),
                   na.rm = TRUE, check_overlap = TRUE)


# or use ggrepel
ggplot(data = happy) +
  geom_mosaic(aes(x = product(health), fill = happy), na.rm = TRUE, show.legend = FALSE) +
  geom_mosaic_text(aes(x = product(happy, health)), na.rm = TRUE, repel = TRUE)


# and as a label
ggplot(data = happy) +
  geom_mosaic(aes(x = product(health), fill = happy), na.rm = TRUE, show.legend = FALSE) +
  geom_mosaic_text(aes(x = product(happy, health)), na.rm = TRUE, repel = TRUE, as.label=TRUE)

```
