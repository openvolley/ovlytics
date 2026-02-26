# Add some extra columns to a plays object

Add some extra columns to a plays object

## Usage

``` r
ov_augment_plays(
  x,
  to_add = c("receiving_team", "touch_summaries", "setters"),
  rotation = "SHM",
  use_existing = TRUE
)
```

## Arguments

- x:

  data.frame: the `plays` data.frame as returned by
  [`datavolley::read_dv()`](https://datavolley.openvolley.org/reference/dv_read.html)
  or
  [`peranavolley::pv_read()`](https://rdrr.io/pkg/peranavolley/man/pv_read.html).
  Can be `plays` from multiple matches combined using `rbind` or
  `bind_rows`

- to_add:

  character: columns to add

  - "receiving_team" adds the columns "receiving_team" (team name) and
    "receiving_team_id"

  - "winners" adds the columns "set_won_by", "set_won_by_id" (the name
    and ID of the team that won the current set), "match_won_by",
    "match_won_by_id" (the name and ID of the team that won the current
    match), "team_won_set" and "team_won_match" (did the team making the
    action in the current row win the set/match), and "home_sets_won"
    and "visiting_sets_won" (the number of sets won by the home and
    visiting teams)

  - "touch_summaries" adds a number of columns named "ts\_\*" that
    summarize a team touch (e.g. columns "ts_pass_quality",
    "ts_pass_evaluation_code" give the pass quality and pass evaluation
    code of the reception or dig associated with a given team touch).
    "touch_summaries" also adds a column named `freeball_over`, which
    disambiguates the action of putting a freeball over the net from the
    action of digging such a ball. Many scouts code both of these as a
    "Freeball". The `freeball_over` column will be `TRUE` if it was a
    freeball being put over the net, and `FALSE` otherwise (including
    freeball digs). Freeballs over and freeball digs will still both
    have "Freeball" in their `skill` column

  - "setters" adds the columns "home_setter_id", "visiting_setter_id"
    (the player IDs of the home and visiting setter on court), and
    "setter_id", "setter_position", and "setter_front_back" (the player
    ID and position of the setter from the team performing the current
    action)

  - "followed" adds the columns "followed_timeout",
    "followed_technical_timeout", and "followed_sub"

  - "player_role" add the column "player_role" which gives the role
    (outside, middle, opposite, setter) for the active player on each
    row of `x`. This assumes a standard rotation as specified by
    `rotation`. Note that `player_role` does NOT include libero,
    although this can be inferred from the `meta` component of a full
    datavolley object

  - "all" is a shortcut for all of the above

- rotation:

  string: (only relevant when `to_add` includes "player_role") either
  "SHM" (assume a setter-hitter-middle rotation order, i.e. outside
  hitter is in position 2 when the setter is in 1), or "SMH"
  (setter-middle-hitter)

- use_existing:

  logical: if `TRUE` and all of the columns associated with a given
  `to_add` choice are already present in `x`, they won't be re-generated

## Value

`x` with the extra columns added
