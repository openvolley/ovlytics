# Plot a simulated setter distribution sequence

Plot a simulated setter distribution sequence

## Usage

``` r
ov_plot_sequence_distribution(
  ssd,
  label_setters_by = "id",
  font_size = 11,
  title_wrap = NA,
  split_set = FALSE,
  output = "plot"
)
```

## Arguments

- ssd:

  simulated setter distribution output as returned by
  [`ov_simulate_setter_distribution()`](ov_simulate_setter_distribution.md)

- label_setters_by:

  string: either "id" or "name"

- font_size:

  numeric: font size

- title_wrap:

  numeric: if non-`NA`, use
  [`strwrap()`](https://rdrr.io/r/base/strwrap.html) to break the title
  into lines of this width

- split_set:

  boolean: if `TRUE`, separate the distribution sequence by set

- output:

  string: either "plot" or "list"

## Examples

``` r
dvw <- ovdata_example("NCA-CUB")
ssd <- ov_simulate_setter_distribution(dvw = dvw, play_phase = c("Reception"),
                                       n_sim = 100, attack_by = "zone",
                                       setter_position_by = "front_back")
ov_plot_sequence_distribution(ssd)

```
