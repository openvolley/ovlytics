# Various skill performance indicators

- attack_eff: (number of kills - number of errors and blocked attacks) /
  (number of attacks)

- serve_eff: (number of aces and positive serves - number of errors and
  poor serves) / (number of serves)

- reception_eff: (number of perfect and positive passes - number of
  errors and overpasses) / (number of passes)

## Usage

``` r
attack_eff(evaluation, skill)

serve_eff(evaluation, skill)

reception_eff(evaluation, skill)
```

## Arguments

- evaluation:

  character: vector of skill evaluations ("Winning attack", "Error",
  etc)

- skill:

  character: (optional) vector of skill values ("Attack", "Block", etc).
  If provided, it will be used to filter the `evaluation` vector to the
  elements corresponding to the correct skill. If not provided, all
  elements of `evaluation` will be used

## Value

A numeric scalar

## Examples

``` r
if (FALSE) { # \dontrun{
 library(dplyr)
 x <- ovdata_example("mlafin_braslovce_nkbm", as = "parsed")
 plays(x) %>% dplyr::filter(skill == "Attack") %>% group_by(player_name) %>%
   dplyr::summarize(N_attacks = n(), att_eff = attack_eff(evaluation))
} # }
```
