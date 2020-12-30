#' Example DataVolley files provided as part of the ovlytics package
#'
#' @param choice string: which data file to return?
#' - "190301_kats_beds" - a match between GKS Katowice and MKS Bedzin during the 2018/19 Polish Plus Liga
#' @return path to the file
#'
#' @examples
#' myfile <- ov_example_file()
#' x <- dv_read(myfile)
#' summary(x)
#'
#' @export
ov_example_file <- function(choice = "190301_kats_beds") {
    assert_that(is.string(choice))
    switch(choice,
           "190301_kats_beds" = system.file("extdata/&190301_kats_beds.dvw", package = "ovlytics"),
           stop("unrecognized 'choice' value (", choice, ")")
           )
}
