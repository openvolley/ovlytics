# Example DataVolley files provided as part of the ovlytics package

Example DataVolley files provided as part of the ovlytics package

## Usage

``` r
ov_example_file(choice = "190301_kats_beds")
```

## Arguments

- choice:

  string: which data file to return?

  - "190301_kats_beds" - a match between GKS Katowice and MKS Bedzin
    during the 2018/19 Polish Plus Liga

## Value

path to the file

## Examples

``` r
myfile <- ov_example_file()
x <- dv_read(myfile)
summary(x)
#> Match summary:
#> Date: 2019-03-01
#> League: Plus Liga 2018/2019 - Plus Liga 2018/20189- Faza Zasadnicza
#> Teams: GKS Katowice (Gruszka Piotr/Słaby Grzegorz)
#>        vs
#>        MKS Będzin (Siewiorek Emil)
#> Result: 3-2 (25-20, 18-25, 25-15, 19-25, 15-6)
#> Duration: 114 minutes
```
