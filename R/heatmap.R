#####from the Heatmap
#' @title Heatmap
#' @method Heatmap mass_dataset
#' @export
#' @rdname multivariate-mass_dataset
#' @importFrom ComplexHeatmap Heatmap
#' @return A heatmap

# library(massdataset)
# library(tidyverse)
# data("liver_aging_pos")
#
# qc_id <-
#   liver_aging_pos %>%
#   activate_mass_dataset(what = "sample_info") %>%
#   dplyr::filter(group == "QC") %>%
#   dplyr::pull(sample_id)
# object <-
#   mutate_rsd(liver_aging_pos, according_to_samples = qc_id)
#
# ###only remain the features with rt > 100, mz > 150 and rsd < 30
# object <-
#   object %>%
#   activate_mass_dataset(what = "variable_info") %>%
#   dplyr::filter(rt > 100) %>%
#   dplyr::filter(mz > 150) %>%
#   dplyr::filter(rsd < 30)
#
#
# matrix <-
#   object %>%
#   `+`(1) %>%
#   log(10) %>%
#   scale_data()

setMethod(f = "Heatmap",
          signature(matrix = "mass_dataset"),
          function (matrix,
                    col,
                    name,
                    na_col = "grey",
                    color_space = "LAB",
                    rect_gp = gpar(col = NA),
                    border = NA,
                    border_gp = gpar(col = "black"),
                    cell_fun = NULL,
                    layer_fun = NULL,
                    jitter = FALSE,
                    row_title = character(0),
                    row_title_side = c("left", "right"),
                    row_title_gp = gpar(fontsize = 13.2),
                    row_title_rot = switch(row_title_side[1], "left" = 90, "right" = 270),
                    column_title = character(0),
                    column_title_side = c("top", "bottom"),
                    column_title_gp = gpar(fontsize = 13.2),
                    column_title_rot = 0,
                    cluster_rows = TRUE,
                    cluster_row_slices = TRUE,
                    clustering_distance_rows = "euclidean",
                    clustering_method_rows = "complete",
                    row_dend_side = c("left", "right"),
                    row_dend_width = unit(10, "mm"),
                    show_row_dend = TRUE,
                    row_dend_reorder = is.logical(cluster_rows) ||
                      is.function(cluster_rows),
                    row_dend_gp = gpar(),
                    cluster_columns = TRUE,
                    cluster_column_slices = TRUE,
                    clustering_distance_columns = "euclidean",
                    clustering_method_columns = "complete",
                    column_dend_side = c("top", "bottom"),
                    column_dend_height = unit(10, "mm"),
                    show_column_dend = TRUE,
                    column_dend_gp = gpar(),
                    column_dend_reorder = is.logical(cluster_columns) ||
                      is.function(cluster_columns),
                    row_order = NULL,
                    column_order = NULL,
                    row_labels = rownames(matrix),
                    row_names_side = c("right", "left"),
                    show_row_names = TRUE,
                    row_names_max_width = unit(6, "cm"),
                    row_names_gp = gpar(fontsize = 12),
                    row_names_rot = 0,
                    row_names_centered = FALSE,
                    column_labels = colnames(matrix),
                    column_names_side = c("bottom", "top"),
                    show_column_names = TRUE,
                    column_names_max_height = unit(6, "cm"),
                    column_names_gp = gpar(fontsize = 12),
                    column_names_rot = 90,
                    column_names_centered = FALSE,
                    top_annotation = NULL,
                    bottom_annotation = NULL,
                    left_annotation = NULL,
                    right_annotation = NULL,
                    km = 1,
                    split = NULL,
                    row_km = km,
                    row_km_repeats = 1,
                    row_split = split,
                    column_km = 1,
                    column_km_repeats = 1,
                    column_split = NULL,
                    gap = unit(1, "mm"),
                    row_gap = unit(1, "mm"),
                    column_gap = unit(1, "mm"),
                    show_parent_dend_line = ht_opt$show_parent_dend_line,
                    
                    heatmap_width = unit(1, "npc"),
                    width = NULL,
                    heatmap_height = unit(1, "npc"),
                    height = NULL,
                    
                    show_heatmap_legend = TRUE,
                    heatmap_legend_param = list(title = name),
                    
                    use_raster = NULL,
                    raster_device = c("png",
                                      "jpeg",
                                      "tiff",
                                      "CairoPNG",
                                      "CairoJPEG",
                                      "CairoTIFF",
                                      "agg_png"),
                    raster_quality = 1,
                    raster_device_param = list(),
                    raster_resize_mat = FALSE,
                    raster_by_magick = requireNamespace("magick", quietly = TRUE),
                    raster_magick_filter = NULL,
                    post_fun = NULL) {
            expression_data <-
              matrix@expression_data
            ComplexHeatmap::Heatmap(
              as.numeric(expression_data),
              col = col,
              name = name,
              na_col = na_col,
              color_space = color_space,
              rect_gp = rect_gp,
              border = border,
              border_gp = border_gp,
              cell_fun = cell_fun,
              layer_fun = layer_fun,
              jitter = jitter,
              row_title = row_title,
              row_title_side = row_title_side,
              row_title_gp = row_title_gp,
              row_title_rot = row_title_rot,
              column_title = column_title,
              column_title_side = column_title_side,
              column_title_gp = column_title_gp,
              column_title_rot = column_title_rot,
              cluster_rows = cluster_rows,
              cluster_row_slices = cluster_row_slices,
              clustering_distance_rows = clustering_distance_rows,
              clustering_method_rows = clustering_method_rows,
              row_dend_side = row_dend_side,
              row_dend_width = row_dend_width,
              show_row_dend = show_row_dend,
              row_dend_reorder = row_dend_reorder,
              row_dend_gp = row_dend_gp,
              cluster_columns = cluster_columns,
              cluster_column_slices = cluster_column_slices,
              clustering_distance_columns = clustering_distance_columns,
              clustering_method_columns = clustering_method_columns,
              column_dend_side = column_dend_side,
              column_dend_height = column_dend_height,
              show_column_dend = show_column_dend,
              column_dend_gp = column_dend_gp,
              column_dend_reorder = column_dend_reorder,
              row_order = row_order,
              column_order = column_order,
              row_labels = row_labels,
              row_names_side = row_names_side,
              show_row_names = show_row_names,
              row_names_max_width = row_names_max_width,
              row_names_gp = row_names_gp,
              row_names_rot = row_names_rot,
              row_names_centered = row_names_centered,
              column_labels = column_labels,
              column_names_side = column_names_side,
              show_column_names = show_column_names,
              column_names_max_height = column_names_max_height,
              column_names_gp = column_names_gp,
              column_names_rot = column_names_rot,
              column_names_centered = column_names_centered,
              top_annotation = top_annotation,
              bottom_annotation = bottom_annotation,
              left_annotation = left_annotation,
              right_annotation = right_annotation,
              km = km,
              split = split,
              row_km = row_km,
              row_km_repeats = row_km_repeats,
              row_split = row_split,
              column_km = column_km,
              column_km_repeats = column_km_repeats,
              column_split = column_split,
              gap = gap,
              row_gap = row_gap,
              column_gap = column_gap,
              show_parent_dend_line = show_parent_dend_line,
              
              heatmap_width = heatmap_width,
              width = width,
              heatmap_height = heatmap_height,
              height = height,
              
              show_heatmap_legend = show_heatmap_legend,
              heatmap_legend_param = heatmap_legend_param,
              
              use_raster = use_raster,
              raster_device = raster_device,
              raster_quality = raster_quality,
              raster_device_param = raster_device_param,
              raster_resize_mat = raster_resize_mat,
              raster_by_magick = raster_by_magick,
              raster_magick_filter = raster_magick_filter,
              post_fun = post_fun
            )
          })
