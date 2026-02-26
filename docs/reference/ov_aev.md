# Calculate attack evenness

Attack evenness is a measure of how balanced a team's attack is. Teams
that rely heavily on one or two attackers will have a relatively low
evenness value, whereas teams that use all of their attackers will be
higher. See <https://untan.gl/attack-evenness.html> for further
background. Evenness is calculated for each lineup used by a team and
averaged (weighting by the number of attacks that each lineup made) to
get an overall evenness value. Liberos are not expected to attack, and
setter attacks (dumps) are ignored. Evenness can be calculated on
grouped data: by default, it is calculated by match but averaged over
matches when reporting the final result.

## Usage

``` r
ov_aev(
  x,
  team,
  rotation,
  reference_props = NULL,
  calculate_by = "match_id",
  report_by,
  min_N_attacks = 10,
  detail = FALSE
)
```

## Arguments

- x:

  data.frame: the `plays` data.frame as returned by
  [`datavolley::read_dv()`](https://datavolley.openvolley.org/reference/dv_read.html)
  or
  [`peranavolley::pv_read()`](https://rdrr.io/pkg/peranavolley/man/pv_read.html)

- team:

  string: the team to calculate attack evenness for. If not provided, it
  will be calculated separately for all teams in `x`

- rotation:

  string: the calculation needs to know what position each player is
  playing, so that it can work out how many attacks that player should
  have made under a "perfectly even attack" scenario. This is important
  for middle hitters, who are usually replaced by the libero in back
  court and therefore cannot make attacks at those times. The `rotation`
  parameter can be one of:

  - "player_role": use the player roles as specified in the
    "player_role" column in `x`. These are typically the player roles
    defined in the roster, but it is left to the user to populate this
    column

  - "SHM": assume a setter-hitter-middle rotation order (i.e. outside
    hitter is in position 2 when the setter is in 1) and infer the
    player roles from that

  - "SMH": assume a setter-middle-hitter rotation order and infer the
    player roles from that

  - "none": don't assume player roles, which will mean that under a
    "perfectly even attack" scenario, each player (excluding the setter)
    will be expected to make 20% of attacks. This option is probably of
    limited use

- reference_props:

  data.frame or NULL: if `NULL`, the default attack profile as returned
  by [`ov_aev_reference_props()`](ov_aev_reference_props.md) will be
  used (recommended). Otherwise, provide a data.frame with the same
  format as that returned by
  [`ov_aev_reference_props()`](ov_aev_reference_props.md)

- calculate_by:

  character: names variables in `x` to group by when doing the
  calculations. Note that "lineup" is always used as a calculation
  grouping variable (it is populated inside the function). See Details

- report_by:

  character: names of variables in `x` to group by for the final
  results. Note that results are always effectively grouped by team. Any
  `report_by` variables will be used in addition to team. Note that:

  - `report_by` can include "lineup" and/or "setter_id" (these variables
    are used internally in calculations)

  - `report_by` variables must also be present in `calculate_by`, with
    the exception of "lineup" and "setter_id"

- min_N_attacks:

  integer: minimum number of attacks that must be made in order to be
  included in the calculations. Attacks are counted by lineup and
  `calculate_by` variables (if any). If `calculate_by` is empty and
  `min_N_attacks` is 10, then only lineups that made 10 or more attacks
  (in total) will be included in the calculations. If `min_N_attacks` is
  10 and `calculate_by` is "match_id", then calculations will be done
  match by match, using only lineups in a match that made 10 or more
  attacks in that match

- detail:

  logical: if `TRUE`, the returned data frame will include additional
  columns: "aev_sd" (standard deviation of aev across the calculate_by
  groups), "rally_win_rate", "kill_rate", "rec_eff" (reception
  efficiency), "N_rallies" (number of rallies played). Note that
  including these details makes the calculation noticeably slower

## Value

A tibble with at least the columns "team", "aev", and "N_attacks". If
`detail` was TRUE, additional columns will also be present (see the
`detail` parameter)

## Details

Note that calculation by group and averaging will not generally give the
same results as calculating the average result in one step (e.g.
calculating for several matches and averaging those results will
probably not give the same answer as calculating for all matches
pooled). This is expected: say that my team under-utilizes a particular
hitter in one match, and over-utilizes her in another. In both of those
matches my team's attack was uneven, and so the average of those two
results should return a low evenness value indicating that on average my
team's attack was uneven. But if the two matches are pooled and the data
analyzed all together, the under-utilization in the first match might be
balanced by the over-utilization in the second match, giving a higher
attack evenness.

## References

<https://untan.gl/attack-evenness.html>

## Examples

``` r
px <- plays(dv_read(ovdata_example()))

## for a single team
ov_aev(px, rotation = "SHM", team = "GKS Katowice")
#> # A tibble: 1 × 4
#>   team         aev_sd   aev N_attacks
#>   <chr>         <dbl> <dbl>     <int>
#> 1 GKS Katowice 0.0330 0.861        99

## for all teams in px, and with extra detail
ov_aev(px, rotation = "SHM", detail = TRUE)
#> # A tibble: 2 × 8
#>   team         aev_sd   aev rally_win_rate kill_rate rec_eff N_attacks N_rallies
#>   <chr>         <dbl> <dbl>          <dbl>     <dbl>   <dbl>     <int>     <int>
#> 1 GKS Katowice 0.0330 0.861          0.711     0.455   0.338        99        76
#> 2 MKS Będzin   0.0242 0.75           0.578     0.403   0.372       124        90

## for a single team, calculated by set number but aggregate results when reporting
ov_aev(px, team = "GKS Katowice", rotation = "SHM", calculate_by = "set_number")
#> # A tibble: 1 × 4
#>   team         aev_sd   aev N_attacks
#>   <chr>         <dbl> <dbl>     <int>
#> 1 GKS Katowice 0.0721 0.813        99

## for a single team, calculated and reported by match and set number
ov_aev(px, team = "GKS Katowice", rotation = "SHM", calculate_by = c("match_id", "set_number"),
       report_by = c("match_id", "set_number"))
#> # A tibble: 5 × 6
#>   match_id                         set_number team        aev_sd   aev N_attacks
#>   <chr>                                 <int> <chr>        <dbl> <dbl>     <int>
#> 1 5ffdb2dc1fe60efb1ef0a5a1d1b6426c          1 GKS Katowi… 0      0.891        23
#> 2 5ffdb2dc1fe60efb1ef0a5a1d1b6426c          2 GKS Katowi… 0      0.8          20
#> 3 5ffdb2dc1fe60efb1ef0a5a1d1b6426c          3 GKS Katowi… 0      0.838        17
#> 4 5ffdb2dc1fe60efb1ef0a5a1d1b6426c          4 GKS Katowi… 0.0958 0.74         25
#> 5 5ffdb2dc1fe60efb1ef0a5a1d1b6426c          5 GKS Katowi… 0      0.804        14

## for a single team, calculated by match and setter position, and reported by setter position
px <- ov_augment_plays(px, to_add = "setters", use_existing = FALSE)
ov_aev(px, team = "GKS Katowice", rotation = "SHM",
       calculate_by = c("match_id", "setter_position"), report_by = "setter_position")
#> # A tibble: 5 × 5
#>   setter_position team           aev_sd   aev N_attacks
#>             <int> <chr>           <dbl> <dbl>     <int>
#> 1               1 GKS Katowice 0        0.8          10
#> 2               3 GKS Katowice 0        0.667        12
#> 3               4 GKS Katowice 0        0.8          10
#> 4               5 GKS Katowice 1.15e-16 0.767        15
#> 5               6 GKS Katowice 0        0.7          15
```
