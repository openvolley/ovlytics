# Create a prior table from a dvw or a directory of dvw files

Create a prior table from a dvw or a directory of dvw files

## Usage

``` r
ov_create_history_table(
  dvw,
  play_phase = c("Reception", "Transition"),
  attack_by = "code",
  setter_position_by = "rotation",
  exclude_attacks = c("PR"),
  normalize_parameters = TRUE
)
```

## Arguments

- dvw:

  string: path to one or more datavolley files, a list of one or more
  datavolley objects, or a directory containing datavolley files

- play_phase:

  character: one or both of "Reception", "Transition"

- attack_by:

  string: either "code", "zone", "tempo" or "setter call"

- setter_position_by:

  string: either "rotation", or "front_back"

- exclude_attacks:

  character: vector of attack codes to exclude

- normalize_parameters:

  logical: reduce the prior parameter values

## Value

A list, currently with one component named "prior_table"

## Examples

``` r
## use this file to create the priors
hist_dvw <- ovdata_example("NCA-CUB")
history_table <- ov_create_history_table(dvw = hist_dvw, attack_by = "attacker_name",
                                   setter_position_by = "front_back")

## use it on another file (here, the same file for demo purposes)
## usually the history would be from a reference set of previous matches

dvw <- ovdata_example("NCA-CUB")
setter <- ov_simulate_setter_distribution(dvw = dvw, play_phase = "Reception", n_sim = 100,
                                  attack_by = "attacker_name", attack_options = "use_history",
                                  setter_position_by = "front_back",
                                  history_table = history_table, filter_sim = TRUE)

## plot the results
ov_plot_ssd(setter, overlay_set_number = TRUE)

ov_plot_distribution(setter)

```
