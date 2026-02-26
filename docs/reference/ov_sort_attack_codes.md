# Sort DataVolley attack codes

Sort DataVolley attack codes

## Usage

``` r
ov_sort_attack_codes(ac, by = "XV", na.last = NA)
```

## Arguments

- ac:

  character: character vector of attack codes to sort

- by:

  string: method to use, currently only "XV" (any other value will
  default back to using [`sort`](https://rdrr.io/r/base/sort.html)
  without modification). "XV" will place X and V codes first (in
  numerical order, with each X preceding its matching V) then everything
  else in alphabetical order after that

- na.last:

  logical: passed to `sort`

## Value

Sorted character vector

## Examples

``` r
ov_sort_attack_codes(c("V5", "V1", "X6", "CF", "X5"))
#> [1] "V1" "X5" "V5" "X6" "CF"

if (FALSE) { # \dontrun{
  ## sorting might be useful for controlling the plot order when facetting
  ##  a `ggplot` by attack code
  mydata$attack_code <- factor(mydata$attack_code,
            levels = ov_sort_attack_codes(unique(na.omit(mydata$attack_code))))
  ggplot(mydata, ...) + facet_wrap(~attack_code)
} # }
```
