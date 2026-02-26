# Simulate a Bayesian Bandit choice for a given set of probabilities and a number of points

Simulate a Bayesian Bandit choice for a given set of probabilities and a
number of points

## Usage

``` r
ov_simulate_setter_distribution(
  dvw,
  play_phase = c("Reception", "Transition"),
  n_sim = 500,
  priors = list(name = "beta", par1 = 1, par2 = 1),
  epsilon = 1,
  filter_sim = FALSE,
  attack_options = "use_data",
  killRate_grouping = NULL,
  setter_position_by = "rotation",
  history_table = NULL,
  attack_by = "code",
  exclude_attacks = c("PR"),
  rotation = "SHM",
  shiny_progress = NULL
)
```

## Arguments

- dvw:

  string or datavolley: a datavolley object as returned by
  [`datavolley::dv_read()`](https://datavolley.openvolley.org/reference/dv_read.html)
  or a path to datavolley file

- play_phase:

  character: one or both of "Reception", "Transition"

- n_sim:

  integer: number of simulations

- priors:

  numeric: prior distribution of the kill rate for the different
  attacking options

- epsilon:

  numeric: reward size

- filter_sim:

  logical:

- attack_options:

  string: either "use_data" or "use_history"

- killRate_grouping:

  string: Default to NULL, it will use 'attack by' grouping variables.
  Otherwise a set of additional grouping variables to calculate the kill
  rate.

- setter_position_by:

  string: either "rotation" or "front_back"

- history_table:

  list: (only if `attack_options` is "use_history") the object returned
  by [`ov_create_history_table()`](ov_create_history_table.md)

- attack_by:

  string: either "code", "zone", "tempo", "setter call",
  "attacker_name", "player_role"

- exclude_attacks:

  character: vector of attack codes to exclude

- rotation:

  string: (only relevant when `attack_by` is "player_role") either "SHM"
  (assume a setter-hitter-middle rotation order), or "SMH"
  (setter-middle-hitter)

- shiny_progress:

  numeric: an optional two-element vector. If not `NULL` or `NA`,
  [`shiny::setProgress()`](https://rdrr.io/pkg/shiny/man/withProgress.html)
  calls will be made during simulation with `value`s in this range

## See also

[`ov_create_history_table()`](ov_create_history_table.md)

## Examples

``` r
dvw <- ovdata_example("NCA-CUB")
system.time({
  ssd <- ov_simulate_setter_distribution(dvw = dvw, play_phase = "Reception",
                                         n_sim = 100, attack_by = "setter call",
                                         setter_position_by = "front_back", filter_sim = TRUE)
})
#>    user  system elapsed 
#>   1.475   0.023   1.522 
```
