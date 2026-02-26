# Print the prior table

Print the prior table

## Usage

``` r
ov_print_history_table(history_table, team, setter_id)
```

## Arguments

- history_table:

  data.frame: the `prior_table` component of the object returned by
  [`ov_create_history_table()`](ov_create_history_table.md)

- team:

  string: team name

- setter_id:

  string: setter_id

## Examples

``` r
hist_dvw <- ovdata_example("NCA-CUB")
history_table <- ov_create_history_table(dvw = hist_dvw, attack_by = "zone")
team = history_table$prior_table$team[1]
setter_id = history_table$prior_table$setter_id[1]
ov_print_history_table(history_table, team, setter_id)
#> Warning: Since gt v0.6.0 `fmt_missing()` is deprecated and will soon be removed.
#> ℹ Use `sub_missing()` instead.
#> This warning is displayed once every 8 hours.
#> Warning: Since gt v0.9.0, the `colors` argument has been deprecated.
#> • Please use the `fn` argument instead.
#> This warning is displayed once every 8 hours.


  
```
