# Plot the rates

Plot the rates

## Usage

``` r
ov_plot_rate(ssd, team, setter_id, range = c(0.05, 0.95))
```

## Arguments

- ssd:

  simulated setter distribution output as returned by
  [`ov_simulate_setter_distribution()`](ov_simulate_setter_distribution.md)

- team:

  string: team name

- setter_id:

  string: setter_id

- range:

  vector of maximum and minimum quantile value description

## Examples

``` r
dvw <- ovdata_example("NCA-CUB")
system.time({
  ssd <- ov_simulate_setter_distribution(dvw = dvw, play_phase = c("Reception", "Transition"),
                                         n_sim = 150, setter_position_by = "front_back",
                                          attack_by = "zone")
  team <- ssd$raw_data$meta$teams$team[1]
  setter_id <- ssd$raw_data$meta$players_h$player_id[which(ssd$raw_data$meta$players_h$role == "setter")][2]
  ov_plot_rate(ssd, team, setter_id)
})
#>    user  system elapsed 
#>   2.470   0.009   2.479 
```
