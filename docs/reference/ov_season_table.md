# Create a summary table of a team's matches in a season

Create a summary table of a team's matches in a season

## Usage

``` r
ov_season_table(xl, target_team, target_team_id, show_by = "match date")
```

## Arguments

- xl:

  list: list of datavolley objects (each as returned by
  [`datavolley::dv_read()`](https://datavolley.openvolley.org/reference/dv_read.html)

- target_team:

  string: the name of the target team. Only one of `target_team` or
  `target_team_id` is required

- target_team_id:

  string: the team ID of the target team. Ignored if `target_team` has
  been provided

- show_by:

  string: either "match date" (show each match according to its date) or
  "filename" (show each match according to its filename. This might be
  useful if the match dates are being parsed incorrectly by
  [`datavolley::dv_read()`](https://datavolley.openvolley.org/reference/dv_read.html))

## Value

A tibble with columns "Opponent", "Date" (or "File"), "Result", "Set
scores", and one column for sets 1 to 5

## Examples

``` r
## trivial example of a single-match "season"
library(datavolley)
x <- dv_read(dv_example_file())
ov_season_table(list(x), target_team = home_team(x))
#> # A tibble: 1 × 10
#>   Opponent    Date       Result `Set scores` `W/L` `1`   `2`   `3`   `4`   `5`  
#>   <chr>       <date>     <chr>  <chr>        <lgl> <chr> <chr> <chr> <chr> <chr>
#> 1 Nova KBM B… 2015-01-25 3-0    25-16, 25-1… TRUE  S-3*  R-3*  S-3*  -     -    
```
