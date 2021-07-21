#' \pkg{ovlytics}
#'
#' Analytical functions for volleyball analytics, to be used in conjunction with the datavolley and peranavolley packages.
#'
#' @name ovlytics
#' @docType package
#' @importFrom assertthat assert_that is.flag is.string
#' @importFrom cowplot plot_grid
#' @importFrom datavolley dv_index2xy dv_flip_xy dv_read dv_xy ggcourt home_team plays visiting_team
#' @importFrom dplyr %>% bind_cols bind_rows case_when distinct everything filter full_join group_by lag lead left_join mutate n pull row_number select tibble ungroup slice matches arrange
#' @importFrom ggplot2 aes_string aes arrow facet_wrap geom_label geom_line geom_point geom_ribbon geom_segment geom_step geom_text geom_tile geom_vline ggplot ggtitle labs scale_fill_brewer scale_fill_continuous scale_fill_gradient2 scale_size theme theme_bw geom_col scale_y_continuous element_text ylab xlab ylim geom_area theme_void
#' @importFrom grid arrow unit
#' @importFrom gt gt fmt_missing data_color cols_align
#' @importFrom methods as
#' @importFrom ovdata ovdata_example
#' @importFrom paletteer paletteer_d
#' @importFrom patchwork plot_layout wrap_plots
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom scales percent col_numeric
#' @importFrom stats na.omit quantile rbeta rbinom runif setNames
#' @importFrom tidyr drop_na pivot_longer pivot_wider complete nest unite
#' @importFrom utils head packageVersion
NULL
