# Print the rate table

Print the rate table

## Usage

``` r
ov_print_rate_table(ssd, team, setter_id)
```

## Arguments

- ssd:

  simulated setter distribution output as returned by
  [`ov_simulate_setter_distribution()`](ov_simulate_setter_distribution.md)

- team:

  string: team name

- setter_id:

  string: setter_id

## Examples

``` r
dvw <- ovdata_example("NCA-CUB")
system.time({
  ssd <- ov_simulate_setter_distribution(dvw = dvw, play_phase = "Reception",
                                         n_sim = 100, setter_position_by = "front_back")
  team <- ssd$raw_data$meta$teams$team[1]
  setter_id <- ssd$raw_data$meta$players_h$player_id[which(ssd$raw_data$meta$players_h$role == "setter")][2]
  ov_print_rate_table(ssd, team, setter_id)
})
#>    user  system elapsed 
#>   1.465   0.003   1.479 
```
