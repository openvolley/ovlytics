`%eq%` <- function (x, y) x == y & !is.na(x) & !is.na(y)

single_value_or <- function(x, or = as(NA, class(x))) if (length(x) == 1) x else or

most_common_value <- function(x, na.rm=FALSE) {
    ux <- unique(x)
    if (na.rm) {
        ux <- ux[!is.na(ux)]
        if (length(ux)<1) ux <- as(NA, class(x))
    }
    ux[which.max(tabulate(match(x, ux)))]
}

## division, avoiding div by 0 warnings (return NA not NaN)
`%/n/%` <- function(x, y) ifelse(abs(y) < 1e-09, NA_real_, x/y)

## convenience shortcuts
mean_narm <- function(...) mean(..., na.rm = TRUE)
sum_narm <- function(...) sum(..., na.rm = TRUE)
