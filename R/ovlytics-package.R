#' \pkg{ovlytics}
#'
#' Analytical functions for volleyball analytics, to be used in conjunction with the datavolley and peranavolley packages.
#'
#' @name ovlytics
#' @docType package
#' @importFrom assertthat assert_that is.flag is.string
#' @importFrom cowplot plot_grid
#' @importFrom datavolley dv_index2xy dv_flip_xy dv_read dv_xy ggcourt home_team plays visiting_team
#' @importFrom dplyr %>% across all_of bind_cols bind_rows case_when distinct everything if_else filter full_join group_by lag lead left_join mutate n pull row_number select tibble ungroup slice matches
#' @importFrom ggplot2 aes_string aes arrow facet_grid facet_wrap geom_area geom_label geom_line geom_point geom_ribbon geom_segment geom_step geom_text geom_tile geom_vline ggplot ggtitle labs scale_colour_manual scale_fill_brewer scale_fill_continuous scale_fill_gradient2 scale_size theme theme_bw theme_void geom_col scale_y_continuous element_text ylab xlab ylim
#' @importFrom grDevices colorRamp colorRampPalette rgb
#' @importFrom grid arrow unit
#' @importFrom gt cols_align data_color fmt_missing gt
#' @importFrom htmltools tags
#' @importFrom methods as
#' @importFrom ovdata ovdata_example
#' @importFrom paletteer paletteer_d
#' @importFrom patchwork plot_layout wrap_plots
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom scales col_numeric percent
#' @importFrom stats na.omit quantile rbeta rbinom runif setNames qbeta
#' @importFrom tidyr complete drop_na nest pivot_longer pivot_wider unite
#' @importFrom utils head packageVersion
"_PACKAGE"
