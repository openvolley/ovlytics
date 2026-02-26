# Infer the role of each player

Infer the role of each player

## Usage

``` r
ov_infer_player_roles(
  x,
  target_team,
  method,
  fall_back = TRUE,
  setter_tip_codes = c("PP", "PR", "P2")
)
```

## Arguments

- x:

  : a datavolley object (as returned by
  [`datavolley::dv_read()`](https://datavolley.openvolley.org/reference/dv_read.html)),
  a list of datavolley objects, or the `plays` component of a datavolley
  object

- target_team:

  string or function: team to report on. If this is a function, it
  should return `TRUE` when passed the target team name

- method:

  string: one of

  - "meta" - rely on player metadata, i.e. the team rosters

  - "SHM" - assume a setter-hitter-middle rotation order. When the
    setter is in position 1 the outside hitters are in 2 and 5, and
    middles in 3 and 6

  - "SMH" - setter-middle-hitter rotation order. When the setter is in
    position 1 the middles are in 2 and 5, and outside hitters in 3 and
    6

  - or "data" - try and identify player roles based on the ball touches
    that each player makes. Note that this assumes certain play
    characteristics: outsides hit pipes and in front row hit from the
    left except in P1 reception, opposites hit from the right (left in
    P1 reception), middles hit quick balls or slides or from zone 3
    Method "meta" is the default if a datavolley object or list of
    objects is provided. Note that "SHM" and "SMH" cannot identify
    liberos, because they do not appear in the point-by-point on-court
    lineups.

- fall_back:

  logical: if `TRUE` and `method` is "meta" and x is a single datavolley
  object BUT player roles are not provided in the DataVolley file
  metadata section, fall back to method "data"

- setter_tip_codes:

  character: vector of attack combination codes that correspond to
  setter tips and other attacks that do not tend to be made from a
  consistent start zone. Only relevant if `method` is "data"

## Value

A data.frame with columns `player_id`, `role`, and `score` (the
approximate confidence in the role assignment, in the range 0 to 1)

## Details

The inference procedure assumes that each player plays the same role
across the entire data set `x`. If `x` is drawn from multiple matches,
and a player changes role from match to match, it will likely give
misleading results for that player. In that case, it is probably better
to run this inference on each match separately.

## Examples

``` r
x <- ovdata_example("mlafin_braslovce_nkbm", as = "parsed")
## guess roles according to the actions that the players made
rx <- ov_infer_player_roles(x, target_team = "Nova KBM Branik", method = "data")
```
