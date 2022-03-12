#' @title Heatmap
#' @description Heatmap function from ComplexHeatmap. Credit to Dr. Zuguang Gu.
#' @author Zuguang Gu
#' \email{z.gu@@dkfz.de}
#' @param matrix see ?ComplexHeatmap::Heatmap
#' @param col see ?ComplexHeatmap::Heatmap
#' @param name see ?ComplexHeatmap::Heatmap
#' @param na_col see ?ComplexHeatmap::Heatmap
#' @param color_space see ?ComplexHeatmap::Heatmap
#' @param rect_gp see ?ComplexHeatmap::Heatmap
#' @param border see ?ComplexHeatmap::Heatmap
#' @param border_gp see ?ComplexHeatmap::Heatmap
#' @param cell_fun see ?ComplexHeatmap::Heatmap
#' @param layer_fun see ?ComplexHeatmap::Heatmap
#' @param jitter see ?ComplexHeatmap::Heatmap
#' @param row_title see ?ComplexHeatmap::Heatmap
#' @param row_title_side see ?ComplexHeatmap::Heatmap
#' @param row_title_gp see ?ComplexHeatmap::Heatmap
#' @param row_title_rot see ?ComplexHeatmap::Heatmap
#' @param column_title see ?ComplexHeatmap::Heatmap
#' @param column_title_side see ?ComplexHeatmap::Heatmap
#' @param column_title_gp see ?ComplexHeatmap::Heatmap
#' @param column_title_rot see ?ComplexHeatmap::Heatmap
#' @param cluster_rows see ?ComplexHeatmap::Heatmap
#' @param cluster_row_slices see ?ComplexHeatmap::Heatmap
#' @param clustering_distance_rows see ?ComplexHeatmap::Heatmap
#' @param clustering_method_rows see ?ComplexHeatmap::Heatmap
#' @param row_dend_side see ?ComplexHeatmap::Heatmap
#' @param row_dend_width see ?ComplexHeatmap::Heatmap
#' @param show_row_dend see ?ComplexHeatmap::Heatmap
#' @param row_dend_reorder  ee ?ComplexHeatmap::Heatmap
#' @param row_dend_gp see ?ComplexHeatmap::Heatmap
#' @param cluster_columns see ?ComplexHeatmap::Heatmap
#' @param cluster_column_slices see ?ComplexHeatmap::Heatmap
#' @param clustering_distance_columns see ?ComplexHeatmap::Heatmap
#' @param clustering_method_columns see ?ComplexHeatmap::Heatmap
#' @param column_dend_side see ?ComplexHeatmap::Heatmap
#' @param column_dend_height see ?ComplexHeatmap::Heatmap
#' @param show_column_dend see ?ComplexHeatmap::Heatmap
#' @param column_dend_gp see ?ComplexHeatmap::Heatmap
#' @param column_dend_reorder see ?ComplexHeatmap::Heatmap
#' @param row_order see ?ComplexHeatmap::Heatmap
#' @param column_order see ?ComplexHeatmap::Heatmap
#' @param row_labels see ?ComplexHeatmap::Heatmap
#' @param row_names_side see ?ComplexHeatmap::Heatmap
#' @param show_row_names see ?ComplexHeatmap::Heatmap
#' @param row_names_max_width see ?ComplexHeatmap::Heatmap
#' @param row_names_gp see ?ComplexHeatmap::Heatmap
#' @param row_names_rot see ?ComplexHeatmap::Heatmap
#' @param row_names_centered see ?ComplexHeatmap::Heatmap
#' @param column_labels see ?ComplexHeatmap::Heatmap
#' @param column_names_side see ?ComplexHeatmap::Heatmap
#' @param show_column_names see ?ComplexHeatmap::Heatmap
#' @param column_names_max_height see ?ComplexHeatmap::Heatmap
#' @param column_names_gp see ?ComplexHeatmap::Heatmap
#' @param column_names_rot see ?ComplexHeatmap::Heatmap
#' @param column_names_centered see ?ComplexHeatmap::Heatmap
#' @param top_annotation see ?ComplexHeatmap::Heatmap
#' @param bottom_annotation see ?ComplexHeatmap::Heatmap
#' @param left_annotation see ?ComplexHeatmap::Heatmap
#' @param right_annotation see ?ComplexHeatmap::Heatmap
#' @param km see ?ComplexHeatmap::Heatmap
#' @param split see ?ComplexHeatmap::Heatmap
#' @param row_km see ?ComplexHeatmap::Heatmap
#' @param row_km_repeats see ?ComplexHeatmap::Heatmap
#' @param row_split see ?ComplexHeatmap::Heatmap
#' @param column_km see ?ComplexHeatmap::Heatmap
#' @param column_km_repeats see ?ComplexHeatmap::Heatmap
#' @param column_split see ?ComplexHeatmap::Heatmap
#' @param gap see ?ComplexHeatmap::Heatmap
#' @param row_gap see ?ComplexHeatmap::Heatmap
#' @param column_gap see ?ComplexHeatmap::Heatmap
#' @param show_parent_dend_line see ?ComplexHeatmap::Heatmap
#' @param heatmap_width see ?ComplexHeatmap::Heatmap
#' @param width see ?ComplexHeatmap::Heatmap
#' @param heatmap_height see ?ComplexHeatmap::Heatmap
#' @param height see ?ComplexHeatmap::Heatmap
#' @param show_heatmap_legend see ?ComplexHeatmap::Heatmap
#' @param heatmap_legend_param see ?ComplexHeatmap::Heatmap
#' @param use_raster see ?ComplexHeatmap::Heatmap
#' @param raster_device see ?ComplexHeatmap::Heatmap
#' @param raster_quality see ?ComplexHeatmap::Heatmap
#' @param raster_device_param see ?ComplexHeatmap::Heatmap
#' @param raster_resize_mat see ?ComplexHeatmap::Heatmap
#' @param raster_by_magick see ?ComplexHeatmap::Heatmap
#' @param raster_magick_filter see ?ComplexHeatmap::Heatmap
#' @param post_fun see ?ComplexHeatmap::Heatmap
#' @return A Heatmap-class object.
#' @export

Heatmap <-
  function(matrix,
           col,
           name = "value",
           na_col = "grey",
           color_space = "LAB",
           rect_gp = grid::gpar(col = NA),
           border = NA,
           border_gp = grid::gpar(col = "black"),
           cell_fun = NULL,
           layer_fun = NULL,
           jitter = FALSE,
           row_title = character(0),
           row_title_side = c("left", "right"),
           row_title_gp = grid::gpar(fontsize = 13.2),
           row_title_rot = switch(row_title_side[1], "left" = 90, "right" = 270),
           column_title = character(0),
           column_title_side = c("top", "bottom"),
           column_title_gp = grid::gpar(fontsize = 13.2),
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
           row_dend_gp = grid::gpar(),
           cluster_columns = TRUE,
           cluster_column_slices = TRUE,
           clustering_distance_columns = "euclidean",
           clustering_method_columns = "complete",
           column_dend_side = c("top", "bottom"),
           column_dend_height = unit(10, "mm"),
           show_column_dend = TRUE,
           column_dend_gp = grid::gpar(),
           column_dend_reorder = is.logical(cluster_columns) ||
             is.function(cluster_columns),
           row_order = NULL,
           column_order = NULL,
           row_labels = rownames(matrix),
           row_names_side = c("right", "left"),
           show_row_names = TRUE,
           row_names_max_width = unit(6, "cm"),
           row_names_gp = grid::gpar(fontsize = 12),
           row_names_rot = 0,
           row_names_centered = FALSE,
           column_labels = colnames(matrix),
           column_names_side = c("bottom", "top"),
           show_column_names = TRUE,
           column_names_max_height = unit(6, "cm"),
           column_names_gp = grid::gpar(fontsize = 12),
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
           show_parent_dend_line = ComplexHeatmap::ht_opt$show_parent_dend_line,
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
    UseMethod("Heatmap")
  }

#' @export
Heatmap.default <-
  function(matrix,
           col,
           name = "value",
           na_col = "grey",
           color_space = "LAB",
           rect_gp = grid::gpar(col = NA),
           border = NA,
           border_gp = grid::gpar(col = "black"),
           cell_fun = NULL,
           layer_fun = NULL,
           jitter = FALSE,
           row_title = character(0),
           row_title_side = c("left", "right"),
           row_title_gp = grid::gpar(fontsize = 13.2),
           row_title_rot = switch(row_title_side[1], "left" = 90, "right" = 270),
           column_title = character(0),
           column_title_side = c("top", "bottom"),
           column_title_gp = grid::gpar(fontsize = 13.2),
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
           row_dend_gp = grid::gpar(),
           cluster_columns = TRUE,
           cluster_column_slices = TRUE,
           clustering_distance_columns = "euclidean",
           clustering_method_columns = "complete",
           column_dend_side = c("top", "bottom"),
           column_dend_height = unit(10, "mm"),
           show_column_dend = TRUE,
           column_dend_gp = grid::gpar(),
           column_dend_reorder = is.logical(cluster_columns) ||
             is.function(cluster_columns),
           row_order = NULL,
           column_order = NULL,
           row_labels = rownames(matrix),
           row_names_side = c("right", "left"),
           show_row_names = TRUE,
           row_names_max_width = unit(6, "cm"),
           row_names_gp = grid::gpar(fontsize = 12),
           row_names_rot = 0,
           row_names_centered = FALSE,
           column_labels = colnames(matrix),
           column_names_side = c("bottom", "top"),
           show_column_names = TRUE,
           column_names_max_height = unit(6, "cm"),
           column_names_gp = grid::gpar(fontsize = 12),
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
           show_parent_dend_line = ComplexHeatmap::ht_opt$show_parent_dend_line,
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
    ComplexHeatmap::Heatmap(
      as.matrix(matrix),
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
  }

#' @export
Heatmap.mass_dataset <-
  function(matrix,
           col,
           name = "value",
           na_col = "grey",
           color_space = "LAB",
           rect_gp = grid::gpar(col = NA),
           border = NA,
           border_gp = grid::gpar(col = "black"),
           cell_fun = NULL,
           layer_fun = NULL,
           jitter = FALSE,
           row_title = character(0),
           row_title_side = c("left", "right"),
           row_title_gp = grid::gpar(fontsize = 13.2),
           row_title_rot = switch(row_title_side[1], "left" = 90, "right" = 270),
           column_title = character(0),
           column_title_side = c("top", "bottom"),
           column_title_gp = grid::gpar(fontsize = 13.2),
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
           row_dend_gp = grid::gpar(),
           cluster_columns = TRUE,
           cluster_column_slices = TRUE,
           clustering_distance_columns = "euclidean",
           clustering_method_columns = "complete",
           column_dend_side = c("top", "bottom"),
           column_dend_height = unit(10, "mm"),
           show_column_dend = TRUE,
           column_dend_gp = grid::gpar(),
           column_dend_reorder = is.logical(cluster_columns) ||
             is.function(cluster_columns),
           row_order = NULL,
           column_order = NULL,
           row_labels = rownames(matrix),
           row_names_side = c("right", "left"),
           show_row_names = TRUE,
           row_names_max_width = unit(6, "cm"),
           row_names_gp = grid::gpar(fontsize = 12),
           row_names_rot = 0,
           row_names_centered = FALSE,
           column_labels = colnames(matrix),
           column_names_side = c("bottom", "top"),
           show_column_names = TRUE,
           column_names_max_height = unit(6, "cm"),
           column_names_gp = grid::gpar(fontsize = 12),
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
           show_parent_dend_line = ComplexHeatmap::ht_opt$show_parent_dend_line,
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
      as.matrix(expression_data),
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
  }

#' #####from the Heatmap
#' #' @title Heatmap
#' #' @method Heatmap mass_dataset
#' #' @export
#' #' @rdname multivariate-mass_dataset
#' #' @importFrom ComplexHeatmap Heatmap
#' #' @return A heatmap
#' #'
#' #' library(massdataset)
#' #' library(magrittr)
#' #' library(dplyr)
#' #' data("liver_aging_pos")
#' #'
#' #' qc_id <-
#' #'   liver_aging_pos %>%
#' #'   activate_mass_dataset(what = "sample_info") %>%
#' #'   dplyr::filter(group == "QC") %>%
#' #'   dplyr::pull(sample_id)
#' #' object <-
#' #'   mutate_rsd(liver_aging_pos, according_to_samples = qc_id)
#' #'
#' #' ###only remain the features with rt > 100, mz > 150 and rsd < 30
#' #' object <-
#' #'   object %>%
#' #'   activate_mass_dataset(what = "variable_info") %>%
#' #'   dplyr::filter(rt > 100) %>%
#' #'   dplyr::filter(mz > 150) %>%
#' #'   dplyr::filter(rsd < 30)
#' #'
#' #'
#' #' matrix <-
#' #'   object %>%
#' #'   `+`(1) %>%
#' #'   log(10) %>%
#' #'   scale_data()
#' #'
#' #' Heatmap(matrix = matrix, name = "z-score")
#'
#' setMethod(f = "Heatmap",
#'           signature(matrix = "mass_dataset"),
#'           function (matrix,
#'                     col,
#'                     name = "value",
#'                     na_col = "grey",
#'                     color_space = "LAB",
#'                     rect_gp = grid::gpar(col = NA),
#'                     border = NA,
#'                     border_gp = grid::gpar(col = "black"),
#'                     cell_fun = NULL,
#'                     layer_fun = NULL,
#'                     jitter = FALSE,
#'                     row_title = character(0),
#'                     row_title_side = c("left", "right"),
#'                     row_title_gp = grid::gpar(fontsize = 13.2),
#'                     row_title_rot = switch(row_title_side[1], "left" = 90, "right" = 270),
#'                     column_title = character(0),
#'                     column_title_side = c("top", "bottom"),
#'                     column_title_gp = grid::gpar(fontsize = 13.2),
#'                     column_title_rot = 0,
#'                     cluster_rows = TRUE,
#'                     cluster_row_slices = TRUE,
#'                     clustering_distance_rows = "euclidean",
#'                     clustering_method_rows = "complete",
#'                     row_dend_side = c("left", "right"),
#'                     row_dend_width = unit(10, "mm"),
#'                     show_row_dend = TRUE,
#'                     row_dend_reorder = is.logical(cluster_rows) ||
#'                       is.function(cluster_rows),
#'                     row_dend_gp = grid::gpar(),
#'                     cluster_columns = TRUE,
#'                     cluster_column_slices = TRUE,
#'                     clustering_distance_columns = "euclidean",
#'                     clustering_method_columns = "complete",
#'                     column_dend_side = c("top", "bottom"),
#'                     column_dend_height = unit(10, "mm"),
#'                     show_column_dend = TRUE,
#'                     column_dend_gp = grid::gpar(),
#'                     column_dend_reorder = is.logical(cluster_columns) ||
#'                       is.function(cluster_columns),
#'                     row_order = NULL,
#'                     column_order = NULL,
#'                     row_labels = rownames(matrix),
#'                     row_names_side = c("right", "left"),
#'                     show_row_names = TRUE,
#'                     row_names_max_width = unit(6, "cm"),
#'                     row_names_gp = grid::gpar(fontsize = 12),
#'                     row_names_rot = 0,
#'                     row_names_centered = FALSE,
#'                     column_labels = colnames(matrix),
#'                     column_names_side = c("bottom", "top"),
#'                     show_column_names = TRUE,
#'                     column_names_max_height = unit(6, "cm"),
#'                     column_names_gp = grid::gpar(fontsize = 12),
#'                     column_names_rot = 90,
#'                     column_names_centered = FALSE,
#'                     top_annotation = NULL,
#'                     bottom_annotation = NULL,
#'                     left_annotation = NULL,
#'                     right_annotation = NULL,
#'                     km = 1,
#'                     split = NULL,
#'                     row_km = km,
#'                     row_km_repeats = 1,
#'                     row_split = split,
#'                     column_km = 1,
#'                     column_km_repeats = 1,
#'                     column_split = NULL,
#'                     gap = unit(1, "mm"),
#'                     row_gap = unit(1, "mm"),
#'                     column_gap = unit(1, "mm"),
#'                     show_parent_dend_line = ComplexHeatmap::ht_opt$show_parent_dend_line,
#'                     heatmap_width = unit(1, "npc"),
#'                     width = NULL,
#'                     heatmap_height = unit(1, "npc"),
#'                     height = NULL,
#'
#'                     show_heatmap_legend = TRUE,
#'                     heatmap_legend_param = list(title = name),
#'
#'                     use_raster = NULL,
#'                     raster_device = c("png",
#'                                       "jpeg",
#'                                       "tiff",
#'                                       "CairoPNG",
#'                                       "CairoJPEG",
#'                                       "CairoTIFF",
#'                                       "agg_png"),
#'                     raster_quality = 1,
#'                     raster_device_param = list(),
#'                     raster_resize_mat = FALSE,
#'                     raster_by_magick = requireNamespace("magick", quietly = TRUE),
#'                     raster_magick_filter = NULL,
#'                     post_fun = NULL) {
#'             expression_data <-
#'               matrix@expression_data
#'             ComplexHeatmap::Heatmap(
#'               as.matrix(expression_data),
#'               col = col,
#'               name = name,
#'               na_col = na_col,
#'               color_space = color_space,
#'               rect_gp = rect_gp,
#'               border = border,
#'               border_gp = border_gp,
#'               cell_fun = cell_fun,
#'               layer_fun = layer_fun,
#'               jitter = jitter,
#'               row_title = row_title,
#'               row_title_side = row_title_side,
#'               row_title_gp = row_title_gp,
#'               row_title_rot = row_title_rot,
#'               column_title = column_title,
#'               column_title_side = column_title_side,
#'               column_title_gp = column_title_gp,
#'               column_title_rot = column_title_rot,
#'               cluster_rows = cluster_rows,
#'               cluster_row_slices = cluster_row_slices,
#'               clustering_distance_rows = clustering_distance_rows,
#'               clustering_method_rows = clustering_method_rows,
#'               row_dend_side = row_dend_side,
#'               row_dend_width = row_dend_width,
#'               show_row_dend = show_row_dend,
#'               row_dend_reorder = row_dend_reorder,
#'               row_dend_gp = row_dend_gp,
#'               cluster_columns = cluster_columns,
#'               cluster_column_slices = cluster_column_slices,
#'               clustering_distance_columns = clustering_distance_columns,
#'               clustering_method_columns = clustering_method_columns,
#'               column_dend_side = column_dend_side,
#'               column_dend_height = column_dend_height,
#'               show_column_dend = show_column_dend,
#'               column_dend_gp = column_dend_gp,
#'               column_dend_reorder = column_dend_reorder,
#'               row_order = row_order,
#'               column_order = column_order,
#'               row_labels = row_labels,
#'               row_names_side = row_names_side,
#'               show_row_names = show_row_names,
#'               row_names_max_width = row_names_max_width,
#'               row_names_gp = row_names_gp,
#'               row_names_rot = row_names_rot,
#'               row_names_centered = row_names_centered,
#'               column_labels = column_labels,
#'               column_names_side = column_names_side,
#'               show_column_names = show_column_names,
#'               column_names_max_height = column_names_max_height,
#'               column_names_gp = column_names_gp,
#'               column_names_rot = column_names_rot,
#'               column_names_centered = column_names_centered,
#'               top_annotation = top_annotation,
#'               bottom_annotation = bottom_annotation,
#'               left_annotation = left_annotation,
#'               right_annotation = right_annotation,
#'               km = km,
#'               split = split,
#'               row_km = row_km,
#'               row_km_repeats = row_km_repeats,
#'               row_split = row_split,
#'               column_km = column_km,
#'               column_km_repeats = column_km_repeats,
#'               column_split = column_split,
#'               gap = gap,
#'               row_gap = row_gap,
#'               column_gap = column_gap,
#'               show_parent_dend_line = show_parent_dend_line,
#'
#'               heatmap_width = heatmap_width,
#'               width = width,
#'               heatmap_height = heatmap_height,
#'               height = height,
#'
#'               show_heatmap_legend = show_heatmap_legend,
#'               heatmap_legend_param = heatmap_legend_param,
#'
#'               use_raster = use_raster,
#'               raster_device = raster_device,
#'               raster_quality = raster_quality,
#'               raster_device_param = raster_device_param,
#'               raster_resize_mat = raster_resize_mat,
#'               raster_by_magick = raster_by_magick,
#'               raster_magick_filter = raster_magick_filter,
#'               post_fun = post_fun
#'             )
#'           })
