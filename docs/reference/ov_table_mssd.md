# Table of a simulated multi-game setter distribution sequence

Table of a simulated multi-game setter distribution sequence

## Usage

``` r
ov_table_mssd(
  mssd,
  label_setters_by = "name",
  team = NULL,
  nrows = 50,
  groupBy = TRUE
)
```

## Arguments

- mssd:

  simulated multi-game setter distribution output as returned by
  [`ov_simulate_multiple_setter_distribution()`](ov_simulate_multiple_setter_distribution.md)

- label_setters_by:

  string: either "id" or "name"

- team:

  NULL or string: if non-NULL, show sequence just for this team name

- nrows:

  integer: number of rows per page in the table

- groupBy:

  boolean: if TRUE, will group the rows by Opponent

## Examples

``` r
if (FALSE) { # \dontrun{
 list_dv <- list(dv_read(ovdata_example("NCA-CUB"))) # would normally be multiple games
 mssd <- ov_simulate_multiple_setter_distribution(list_dv = list_dv,
             play_phase = c("Reception", "Transition"), attack_by = "attacker_name",
             n_sim = 100, setter_position_by = "front_back")

 res <- ov_table_mssd(mssd, team = "NICARAGUA")
} # }
```
