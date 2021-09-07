#' Kernel density estimates for volleyball heatmaps
#'
#' @param x : either a numeric vector of x-locations, or a three-column data.frame or matrix with columns `x`, `y`, and optionally `N`. If `x` is a grouped tibble, the kernel density estimates will be calculated separately for group
#' @param y numeric: (unless `x` is a data.frame or matrix) a numeric vector of y-locations
#' @param N numeric: (unless `x` is a data.frame or matrix) a numeric vector of counts associated with each location (the corresponding location was observed `N` times)
#' @param resolution string: the resolution of the locations, either "coordinates" or "subzones"
#' @param bw numeric: a vector of bandwidths to use in the x- and y-directions (see [MASS::kde2d()]). If not provided, default values will be used based on the location resolution
#' @param n integer: (scalar or a length-2 integer vector) the number of grid points in each direction. If not provided, 60 points in the x-direction and 60 (for half-court) or 120 points in the y-direction will be used
#' @param court string: "full" (generate the kernel density estimate for the full court) or "lower" or "upper" (only the lower or upper half of the court)
#' @param auto_flip logical: if `TRUE`, and `court` is either "lower" or "upper", then locations corresponding to the non-selected half of the court will be flipped. This might be appropriate if, for example, the heatmap represents attack end locations that were scouted with coordinates (because these aren't necessarily all aligned to the same end of the court by default)
#'
#' @return A data.frame with columns `x`, `y`, and `density`
#'
#' @examples
#'
#' library(ggplot2)
#' library(datavolley)
#'
#' ## Example 1 - by coordinates
#' ## generate some fake coordinate data
#' Na <- 20
#' set.seed(17)
#' px <- data.frame(x = c(runif(Na, min = 0.4, max = 1.2), runif(Na, min = 2, max = 3)),
#'                  y = c(runif(Na, min = 4.5, max = 6.6), runif(Na, min = 4.9, max = 6.6)))
#'
#' ## plot as points
#' ggplot(px, aes(x, y)) + ggcourt(labels = NULL, court = "upper") +
#'        geom_point(colour = "dodgerblue")
#'
#' ## or as a heatmap
#' hx <- ov_heatmap_kde(px, resolution = "coordinates", court = "upper")
#' ggplot(hx, aes(x, y, fill = density)) +
#'        scale_fill_distiller(palette = "Purples", direction = 1, labels = NULL,
#'                             name = "Attack\ndensity") +
#'        geom_raster() + ggcourt(labels = NULL, court = "upper")
#'
#' ## Example 2 - by subzones, with data from two attackers
#' ## generate some fake data
#' Na <- 20
#' set.seed(17)
#' px <- data.frame(zone = sample(c(1, 5:9), Na * 2, replace = TRUE),
#'                  subzone = sample(c("A", "B", "C", "D"), Na * 2, replace = TRUE),
#'                  attacker = c(rep("Attacker 1", Na), rep("Attacker 2", Na)))
#'
#' ## convert to x, y coordinates
#' px <- cbind(px, dv_xy(zones = px$zone, end = "upper", subzone = px$subzone))
#'
#' ## plot as tiles
#' library(dplyr)
#' ggplot(count(px, attacker, x, y), aes(x, y, fill = n)) + geom_tile() +
#'        facet_wrap(~attacker) + ggcourt(labels = NULL, court = "upper")
#'
#' ## or as a heatmap, noting that we group the data by attacker first
#' hx <- ov_heatmap_kde(group_by(px, attacker), resolution = "subzones", court = "upper")
#' ggplot(hx, aes(x, y, fill = density)) + facet_wrap(~attacker) +
#'        scale_fill_distiller(palette = "Purples", direction = 1, labels = NULL,
#'                             name = "Attack\ndensity") +
#'        geom_raster() + ggcourt(labels = NULL, court = "upper")
#'
#' @export
ov_heatmap_kde <- function(x, y, N = NULL, resolution = "coordinates", bw, n, court = "full", auto_flip = FALSE) {
    dokde <- function(x, y, bw, n, lims) {
        temp <- MASS::kde2d(x, y, h = bw, n = n, lims = lims)
        out <- as.data.frame(expand.grid(x = temp$x, y = temp$y))
        out$density <- as.vector(temp$z)
        ## normalize
        out$density <- out$density / max(out$density, na.rm = TRUE)
        out
    }
    assert_that(is.string(resolution))
    resolution <- tolower(resolution)
    if (!resolution %in% c("coordinates", "subzones")) stop("heatmaps can only be generated for coordinates or subzones")
    assert_that(is.string(court))
    court <- tolower(court)
    court <- match.arg(court, c("lower", "upper", "full"))
    if (missing(bw) || length(bw) < 1) bw <- 0.85 + if (resolution == "subzones") 0.2 else 0
    if (missing(y) || length(y) < 1) {
        if (!inherits(x, c("matrix", "data.frame")) && ncol(x) >= 2) stop("either x should be a 2- or 3- column matrix or data.frame, or y (and optionally N) must be provided")
        if (inherits(x, "matrix")) x <- as.data.frame(x)
        ## use named columns if they are as expected, otherwise rely on column ordering
        if (!"x" %in% colnames(x)) {
            xcol <- head(which(!colnames(x) %in% c("y", "N")), 1)
            if (length(xcol) == 1) colnames(x)[xcol] <- "x"
        }
        if (!"y" %in% colnames(x)) {
            ycol <- head(which(!colnames(x) %in% c("x", "N")), 1)
            if (length(ycol) == 1) colnames(x)[ycol] <- "y"
        }
        if (ncol(x) > 2) {
            if (!"N" %in% colnames(x)) {
                Ncol <- head(which(!colnames(x) %in% c("x", "y")), 1)
                if (length(Ncol) == 1) colnames(x)[Ncol] <- "N"
            }
        } else {
            x$N <- rep(1L, nrow(x))
        }
        if (!all(c("x", "y", "N") %in% colnames(x))) stop("could not figure out x, y, N columns")
        xy <- x
    } else {
        if (length(N) < 1) N <- 1
        xy <- as.data.frame(cbind(x = x, y = y, N = N))
    }
    ## make sure N is rounded to integer
    xy$N <- round(xy$N)
    xy <- dplyr::filter(xy, !is.na(.data$x) & !is.infinite(.data$x) & !is.na(.data$y) & !is.infinite(.data$y) & .data$N > 0)
    if (nrow(xy) < 1) {
        return(data.frame(x = numeric(), y = numeric(), density = numeric()))
    }
    if (court %in% c("lower", "upper") && isTRUE(auto_flip)) {
        ## find points in wrong half of court
        flipidx <- if (court == "lower") which(xy$y > 3.5) else which(xy$y < 3.5)
        xy[flipidx, c("x", "y")] <- dv_flip_xy(xy[flipidx, c("x", "y"), drop = FALSE])
    }
    ## expand by N
    if (any(xy$N > 1)) {
        idx <- unlist(lapply(seq_len(nrow(xy)), function(z) rep(z, xy$N[z])))
        xy <- xy[idx, ]
        xy$N <- 1L
    }
    ymin <- if (court %in% c("upper")) 3.5 else 0
    ymax <- if (court %in% c("lower")) 3.5 else 7
    if (missing(n) || length(n) != 2 || any(is.na(n) | is.infinite(n))) {
        n <- c(60, round((ymax - ymin) / 3.5 * 60))
    }
    ## now calculate kde per group
    ## dplyr::do(xy, dokde(.data$x, .data$y, bw = bw, n = n, lims = c(0, 4, ymin, ymax)))
    ## verbose code to avoid use of dplyr::do or summarize
    gind <- dplyr::group_indices(xy)
    gkey <- dplyr::group_keys(xy)
    out <- bind_rows(lapply(seq_len(dplyr::n_groups(xy)), function(grpnum) {
        this <- xy[gind %eq% grpnum, , drop = FALSE]
        cbind(gkey[grpnum, , drop = FALSE], dokde(this$x, this$y, bw = bw, n = n, lims = c(0, 4, ymin, ymax)))
    }))
    if (dplyr::n_groups(xy) > 1) {
        out <- dplyr::group_by_at(out, dplyr::group_vars(xy))
    }
    out
}

#' Sort DataVolley attack codes
#'
#' @param ac character: character vector of attack codes to sort
#' @param by string: method to use, currently only "XV" (any other value will default back to using \code{\link{sort}} without modification). "XV" will place X and V codes first (in numerical order, with each X preceding its matching V) then everything else in alphabetical order after that
#' @param na.last logical: passed to \code{sort}
#'
#' @return Sorted character vector
#'
#' @examples
#' ov_sort_attack_codes(c("V5", "V1", "X6", "CF", "X5"))
#'
#' \dontrun{
#'   ## sorting might be useful for controlling the plot order when facetting
#'   ##  a `ggplot` by attack code
#'   mydata$attack_code <- factor(mydata$attack_code,
#'             levels = ov_sort_attack_codes(unique(na.omit(mydata$attack_code))))
#'   ggplot(mydata, ...) + facet_wrap(~attack_code)
#' }
#'
#' @export
ov_sort_attack_codes <- function(ac, by = "XV", na.last = NA) {
    assert_that(is.string(by))
    if (toupper(by) == "XV") {
        temp <- ac
        xidx <- grep("^X", temp)
        temp[xidx] <- sub("^X", "", temp[xidx])
        temp[xidx] <- paste0(temp[xidx], "aaa")
        vidx <- grep("^V", temp)
        temp[vidx] <- sub("^V", "", temp[vidx])
        temp[vidx] <- paste0(temp[vidx], "aab")
        temp <- sort(temp, na.last = na.last)
        xidx <- grep("aaa$", temp)
        temp[xidx] <- sub("aaa$", "", temp[xidx])
        temp[xidx] <- paste0("X", temp[xidx])
        vidx <- grep("aab$", temp)
        temp[vidx] <- sub("aab$", "", temp[vidx])
        temp[vidx] <- paste0("V", temp[vidx])
        temp
    } else {
        sort(ac, na.last = na.last)
    }
}
