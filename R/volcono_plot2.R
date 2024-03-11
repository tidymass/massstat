#' @title Volcano Plot Creation
#' @description Creates a volcano plot for visualizing differential expression results, 
#' highlighting significant changes in expression levels.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object An object containing the data to be plotted. Can be a `data.frame`,
#'   `mass_dataset` object, or other types for which methods are defined.
#' @param fc_column_name The name of the column containing fold change values. 
#'   Defaults to `"fc"`.
#' @param log2_fc Logical, indicating if fold change values should be log2 transformed.
#'   Defaults to `TRUE`.
#' @param p_value_column_name The name of the column containing p-value adjustments. 
#'   Defaults to `"p_value_adjust"`.
#' @param labs_x The label for the x-axis. Defaults to `"log2(Fold change)"`.
#' @param labs_y The label for the y-axis. Defaults to `"-log(p-adjust, 10)"`.
#' @param fc_up_cutoff The fold change cutoff for considering an expression level 
#'   significantly up-regulated. Defaults to `2`.
#' @param fc_down_cutoff The fold change cutoff for considering an expression level 
#'   significantly down-regulated. Defaults to `0.5`.
#' @param p_value_cutoff The p-value cutoff for significance. Defaults to `0.05`.
#' @param line_color The color of the lines marking cutoffs. Defaults to `"red"`.
#' @param up_color The color used for points representing up-regulated variables. 
#'   Defaults to `"#EE0000FF"`.
#' @param down_color The color used for points representing down-regulated variables. 
#'   Defaults to `"#3B4992FF"`.
#' @param no_color The color used for points not meeting significance criteria. 
#'   Defaults to `"#808180FF"`.
#' @param point_size The base size of points in the plot. Defaults to `2`.
#' @param point_alpha The transparency level of points in the plot. Defaults to `1`.
#' @param point_size_scale A character string indicating if and how the size of points 
#'   should scale, typically based on significance. Optional.
#' @param line_type The type of line to use for cutoff markers. Defaults to `1`.
#' @param add_text Logical, indicating if labels should be added to significant points.
#'   Defaults to `FALSE`.
#' @param text_for A character vector indicating which points to label based on their 
#'   `marker` status. Possible values are `"marker"`, `"UP"`, `"DOWN"`, or a combination 
#'   thereof. Defaults to `c("marker", "UP", "DOWN")`.
#' @param text_from The name of the column containing labels for text annotations. 
#'   Required if `add_text` is `TRUE`.
#' @return A ggplot object representing the volcano plot.
#' @export
#' @examples
#' library(massdataset)
#' library(magrittr)
#' library(dplyr)
#'
#' data("liver_aging_pos")
#' liver_aging_pos
#'
#' w_78 =
#'   liver_aging_pos %>%
#'   activate_mass_dataset(what = "sample_info") %>%
#'   dplyr::filter(group == "78W") %>%
#'   dplyr::pull(sample_id)
#'
#' w_24 =
#'   liver_aging_pos %>%
#'   activate_mass_dataset(what = "sample_info") %>%
#'   dplyr::filter(group == "24W") %>%
#'   dplyr::pull(sample_id)
#'
#' control_sample_id = w_24
#' case_sample_id = w_78
#'
#' liver_aging_pos =
#'   mutate_fc(
#'     object = liver_aging_pos,
#'     control_sample_id = control_sample_id,
#'     case_sample_id = case_sample_id,
#'     mean_median = "mean"
#'   )
#'
#' liver_aging_pos =
#'   mutate_p_value(
#'     object = liver_aging_pos,
#'     control_sample_id = control_sample_id,
#'     case_sample_id = case_sample_id,
#'     method = "t.test",
#'     p_adjust_methods = "BH"
#'   )
#'
#' object = liver_aging_pos
#' volcano_plot(
#'   object = object,
#'   fc_column_name = "fc",
#'   p_value_column_name = "p_value_adjust",
#'   labs_x = "log2(Fold change)",
#'   labs_y = "-log(p-adjust, 10)",
#'   fc_up_cutoff = 2,
#'   fc_down_cutoff = 0.5,
#'   p_value_cutoff = 0.05,
#'   add_text = TRUE
#' )
#' volcano_plot(
#'   object = object,
#'   fc_column_name = "fc",
#'   p_value_column_name = "p_value",
#'   labs_x = "log2(Fold change)",
#'   labs_y = "-log(p-value, 10)",
#'   fc_up_cutoff = 2,
#'   fc_down_cutoff = 0.5,
#'   p_value_cutoff = 0.05,
#'   add_text = FALSE,
#'   point_alpha = 0.5
#' )
#'
#' volcano_plot(
#'   object = object,
#'   fc_column_name = "fc",
#'   p_value_column_name = "p_value",
#'   labs_x = "log2(Fold change)",
#'   labs_y = "-log(p-value, 10)",
#'   fc_up_cutoff = 2,
#'   fc_down_cutoff = 0.5,
#'   p_value_cutoff = 0.05,
#'   add_text = FALSE,
#'   point_alpha = 0.5,
#'   point_size_scale = "p_value"
#' ) +
#'   scale_size_continuous(range = c(0.5, 3))

volcano_plot <-
  function(object,
           fc_column_name = "fc",
           log2_fc = TRUE,
           p_value_column_name = "p_value_adjust",
           labs_x = "log2(Fold change)",
           labs_y = "-log(p-adjust, 10)",
           fc_up_cutoff = 2,
           fc_down_cutoff = 0.5,
           p_value_cutoff = 0.05,
           line_color = "red",
           up_color = "#EE0000FF",
           down_color = "#3B4992FF",
           no_color = "#808180FF",
           point_size = 2,
           point_alpha = 1,
           point_size_scale = "log10_p",
           line_type = 1,
           add_text = FALSE,
           text_for = c("marker", "UP", "DOWM"),
           text_from = "variable_id") {
    UseMethod("volcano_plot")
  }


#' @method volcano_plot mass_dataset
#' @rdname volcano_plot
#' @export

volcano_plot.mass_dataset <-
  function(object,
           fc_column_name = "fc",
           log2_fc = TRUE,
           p_value_column_name = "p_value_adjust",
           labs_x = "log2(Fold change)",
           labs_y = "-log(p-adjust, 10)",
           fc_up_cutoff = 2,
           fc_down_cutoff = 0.5,
           p_value_cutoff = 0.05,
           line_color = "red",
           up_color = "#EE0000FF",
           down_color = "#3B4992FF",
           no_color = "#808180FF",
           point_size = 2,
           point_alpha = 1,
           point_size_scale = "log10_p",
           line_type = 1,
           add_text = FALSE,
           text_for = c("marker", "UP", "DOWM"),
           text_from = "variable_id") {
    massdataset::check_object_class(object = object, class = "mass_dataset")
    variable_info <-
      object@variable_info
    if (nrow(object@annotation_table) != 0) {
      annotation_table =
        object@annotation_table %>%
        dplyr::group_by(variable_id) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup()
      
      variable_info =
        variable_info %>%
        dplyr::left_join(annotation_table %>%
                           dplyr::select(-c(ms2_files_id:ms2_spectrum_id)),
                         by = "variable_id")
    }
    
    volcano_plot.default(
      object = variable_info,
      fc_column_name = fc_column_name,
      log2_fc = log2_fc,
      p_value_column_name = p_value_column_name,
      labs_x = labs_x,
      labs_y = labs_y,
      fc_up_cutoff = fc_up_cutoff,
      fc_down_cutoff = fc_down_cutoff,
      p_value_cutoff = p_value_cutoff,
      line_color = line_color,
      up_color = up_color,
      down_color = down_color,
      no_color = no_color,
      point_size = point_size,
      point_alpha = point_alpha,
      point_size_scale = point_size_scale,
      line_type = line_type,
      add_text = add_text,
      text_for = text_for,
      text_from = text_from
    )
    
  }

#' @method volcano_plot data.frame
#' @rdname volcano_plot
#' @export
volcano_plot.data.frame <-
  function(object,
           fc_column_name = "fc",
           log2_fc = TRUE,
           p_value_column_name = "p_value_adjust",
           labs_x = "log2(Fold change)",
           labs_y = "-log(p-adjust, 10)",
           fc_up_cutoff = 2,
           fc_down_cutoff = 0.5,
           p_value_cutoff = 0.05,
           line_color = "red",
           up_color = "#EE0000FF",
           down_color = "#3B4992FF",
           no_color = "#808180FF",
           point_size = 2,
           point_alpha = 1,
           point_size_scale = "log10_p",
           line_type = 1,
           add_text = FALSE,
           text_for = c("marker", "UP", "DOWM"),
           text_from = "variable_id") {
    volcano_plot.default(
      object = object,
      fc_column_name = fc_column_name,
      log2_fc = log2_fc,
      p_value_column_name = p_value_column_name,
      labs_x = labs_x,
      labs_y = labs_y,
      fc_up_cutoff = fc_up_cutoff,
      fc_down_cutoff = fc_down_cutoff,
      p_value_cutoff = p_value_cutoff,
      line_color = line_color,
      up_color = up_color,
      down_color = down_color,
      no_color = no_color,
      point_size = point_size,
      point_alpha = point_alpha,
      point_size_scale = point_size_scale,
      line_type = line_type,
      add_text = add_text,
      text_for = text_for,
      text_from = text_from
    )
  }


volcano_plot.default <-
  function(object,
           fc_column_name = "fc",
           log2_fc = TRUE,
           p_value_column_name = "p_value_adjust",
           labs_x = "log2(Fold change)",
           labs_y = "-log(p-adjust, 10)",
           fc_up_cutoff = 2,
           fc_down_cutoff = 0.5,
           p_value_cutoff = 0.05,
           line_color = "red",
           up_color = "#EE0000FF",
           down_color = "#3B4992FF",
           no_color = "#808180FF",
           point_size = 2,
           point_alpha = 1,
           point_size_scale,
           line_type = 1,
           add_text = FALSE,
           text_for = c("marker", "UP", "DOWM"),
           text_from = "variable_id") {
    text_for = match.arg(text_for)
    # massdataset::check_object_class(object = object, class = "mass_dataset")
    
    if (all(colnames(object) != fc_column_name)) {
      stop(paste("no", fc_column_name, "in variable_info.\n"))
    }
    
    if (all(colnames(object) != p_value_column_name)) {
      stop(paste("no", p_value_column_name, "in variable_info.\n"))
    }
    
    variable_info <-
      object
    
    if (log2_fc) {
      variable_info <-
        variable_info %>%
        dplyr::mutate(log2_fc = log(get(fc_column_name), 2)) %>%
        dplyr::mutate(log10_p = -log(get(p_value_column_name), 10)) %>%
        dplyr::mutate(
          marker = case_when(
            log2_fc > log(fc_up_cutoff, 2) &
              log10_p > -log(p_value_cutoff, 10) ~ "UP",
            log2_fc < log(fc_down_cutoff, 2) &
              log10_p > -log(p_value_cutoff, 10) ~ "DOWN",
            TRUE ~ "NO"
          )
        )
    } else{
      variable_info <-
        variable_info %>%
        dplyr::mutate(log2_fc = fc_column_name) %>%
        dplyr::mutate(log10_p = -log(get(p_value_column_name), 10)) %>%
        dplyr::mutate(
          marker = case_when(
            log2_fc > fc_up_cutoff &
              log10_p > -log(p_value_cutoff, 10) ~ "UP",
            log2_fc < fc_down_cutoff &
              log10_p > -log(p_value_cutoff, 10) ~ "DOWN",
            TRUE ~ "NO"
          )
        )
    }
    
    plot =
      variable_info %>%
      ggplot(aes(log2_fc, log10_p)) +
      theme_bw() +
      labs(x = labs_x, y = labs_y) +
      theme(panel.grid.major = element_blank()) +
      geom_vline(
        xintercept = log(fc_up_cutoff, 2),
        color = line_color,
        linetype = line_type
      ) +
      geom_vline(
        xintercept = log(fc_down_cutoff, 2),
        color = line_color,
        linetype = line_type
      ) +
      geom_hline(
        yintercept = -log(p_value_cutoff, 10),
        color = line_color,
        linetype = line_type
      )
    
    
    if (!missing(point_size_scale)) {
      if (all(colnames(variable_info) != point_size_scale)) {
        stop(paste("no", point_size_scale, "in variable_info.\n"))
      } else{
        if (length(grep("p_value", point_size_scale)) > 0) {
          point_size_scale = "log10_p"
        }
        plot =
          plot +
          geom_point(aes(color = marker,
                         size = get(point_size_scale)),
                     alpha = point_alpha) +
          scale_color_manual(values = c(
            "UP" = up_color,
            "DOWN" = down_color,
            "NO" = no_color
          )) +
          guides(size = guide_legend(title = point_size_scale))
      }
    } else{
      plot =
        plot +
        geom_point(aes(color = marker),
                   size = point_size,
                   alpha = point_alpha) +
        scale_color_manual(values = c(
          "UP" = up_color,
          "DOWN" = down_color,
          "NO" = no_color
        ))
    }
    
    
    if (add_text) {
      if (all(colnames(variable_info) != text_from)) {
        stop(paste("no", text_from, "in variable_info.\n"))
      }
      
      if (text_for == "marker") {
        text_data =
          variable_info %>%
          dplyr::filter(marker %in% c("UP", "DOWN"))
      } else{
        text_data =
          variable_info %>%
          dplyr::filter(marker %in% text_for)
      }
      
      
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        plot =
          plot +
          ggrepel::geom_text_repel(aes(log2_fc, log10_p,
                                       label = get(text_from)),
                                   data = text_data)
      } else{
        message("Please install ggrepel package first.")
      }
      
    }
    
    return(plot)
  }
