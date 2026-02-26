# Add information about liberos to a datavolley plays dataframe

Add information about liberos to a datavolley plays dataframe

## Usage

``` r
ov_augment_liberos(x, liberos, middles)
```

## Arguments

- x:

  data.frame: the `plays` data.frame as returned by
  [`datavolley::read_dv()`](https://datavolley.openvolley.org/reference/dv_read.html)
  or
  [`peranavolley::pv_read()`](https://rdrr.io/pkg/peranavolley/man/pv_read.html).
  Can be `plays` from multiple matches combined using `rbind` or
  `bind_rows`

- liberos:

  character: vector of `player_id` values of liberos. Or a data.frame
  with `match_id`, `player_id`, `team`, `role` columns (only rows with
  `role` == "libero" will be used as liberos). The latter form will be
  necessary if `x` contains data from multiple matches and a player
  plays as a libero in some matches but other positions in other matches

- middles:

  character: vector of `player_id` values of middle blockers. Or a
  data.frame with `match_id`, `player_id`, `team`, `role` columns (only
  rows with `role` == "middle" will be used as middles)

## Value

A data.frame of x augmented with columns `libero_id` and
`libero_on_court`. See Details for the contents of these columns

## Details

DataVolley (`.dvw`) files do not include information about the libero on
court. This function adds that information where it can. It assumes that
the libero replaces the back-court middle blocker, except when a middle
blocker is serving.

The procedure is broadly:

- all recorded ball touches are checked on a per-set basis for each
  team: if there were no ball touches by any libero (for a given team)
  during a set, it is assuumed that no libero was used in that set by
  that team. If ball touches were recorded only for one libero, it is
  assumed that only a single libero was used by that team in that set,
  even if that team has multiple liberos available

- this information is then refined on a rally-by-rally basis: in a rally
  where the libero made a ball touch we can be sure that this libero was
  on court

The function returns a copy of `x` with two columns added:

- the `libero_on_court` column will contain TRUE if a libero was on
  court for that data row, or FALSE if not on court, or NA if unknown

- the `libero_id` column will contain the `player_id` of the libero that
  was on court for that data row. If no libero was on court, the value
  "*NO_LIBERO*" will be used. If a libero was known to be on court but
  we could not identify who it was, the value "*MULTIPLE_LIBEROS*" will
  be used

## Examples

``` r
x <- ovdata_example("190301_kats_beds", as = "parsed")

## identify our liberos and middles
liberos <- c(x$meta$players_h$player_id[which(x$meta$players_h$role == "libero")],
             x$meta$players_v$player_id[which(x$meta$players_v$role == "libero")])
middles <- c(x$meta$players_h$player_id[which(x$meta$players_h$role == "middle")],
             x$meta$players_v$player_id[which(x$meta$players_v$role == "middle")])

## infer libero info
px <- ov_augment_liberos(plays(x), liberos = liberos, middles = middles)
```
