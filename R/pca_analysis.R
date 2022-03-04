#' @title run_pca
#' @description run_pca
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object tidymass-class object.
#' @return prcomp object.
#' @export
#' @examples
#' library(massdataset)
#' library(tidyverse)
#' data("liver_aging_pos")
#' liver_aging_pos
#'
#' pca_object =
#' liver_aging_pos %>%
#'   scale() %>%
#'   run_pca()

run_pca <-
  function(object) {
    massdataset::check_object_class(object = object, class = "mass_dataset")
    
    if (sum(is.na(object@expression_data)) > 0) {
      warning("MVs in you object,\nwill remove variables > 50% and imputate with zero.\n")
      object =
        object %>%
        massdataset::mutate_variable_na_freq()
      object =
        object %>%
        massdataset::activate_mass_dataset(what = "variable_info") %>%
        dplyr::filter(na_freq < 0.5)
    }
    
    sample_info = object@sample_info
    expression_data = object@expression_data
    
    expression_data =
      expression_data %>%
      apply(1, function(x) {
        x[is.na(x)] = min(x[!is.na(x)])
        x
      }) %>%
      t()
    
    if (all(names(object@process_info) != "scale")) {
      warning("no scale for this dataset, try to scale() before pca.\n")
    }
    
    pca_object = prcomp(x = t(as.matrix(expression_data)),
                        center = FALSE,
                        scale. = FALSE)
    return(pca_object)
  }



#' @title pca_score_plot
#' @description pca_score_plot
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object mass_dataset.
#' @param pca_object pca_object from run_pca()
#' @param x PC x for X axis, default is 1.
#' @param y PC y for Y axis, default is 2.
#' @param color_by which column (sample_info) is used to color samples
#' @param point_alpha point_alpha default is 1.
#' @param point_size point_size, default is 5.
#' @param point_shape point_shape default is 21.
#' @param frame ?ggplot2::autoplot
#' @param frame.type ?ggplot2::autoplot
#' @param ... other paramters for ggplot2::autoplot
#' @return ggplot2 plot.
#' @export
#' @examples
#' library(massdataset)
#' library(tidyverse)
#' data("liver_aging_pos")
#' liver_aging_pos
#' 
#' pca_object =
#'   liver_aging_pos %>%
#'   scale() %>%
#'   run_pca()
#' 
#' pca_score_plot(liver_aging_pos,
#'                pca_object,
#'                color_by = "group",
#'                loadings = TRUE) +
#'   ggsci::scale_fill_lancet() +
#'   ggsci::scale_color_lancet()
#' 
#' pca_score_plot(liver_aging_pos,
#'                pca_object,
#'                color_by = "group",
#'                frame = TRUE) +
#'   ggsci::scale_fill_lancet() +
#'   ggsci::scale_color_lancet() +
#'   ggrepel::geom_text_repel(aes(label = sample_id))
#' 
#' pca_score_plot(liver_aging_pos,
#'                pca_object,
#'                x = 1,
#'                y = 3,
#'                color_by = "group",
#'                frame = TRUE) +
#'   ggsci::scale_fill_lancet() +
#'   ggsci::scale_color_lancet() +
#'   ggrepel::geom_text_repel(aes(label = sample_id))

pca_score_plot <-
  function(object,
           pca_object,
           x = 1,
           y = 2,
           color_by,
           point_alpha = 1,
           point_size = 5,
           point_shape = 21,
           frame = TRUE,
           frame.type = 'norm',
           ...) {
    massdataset::check_object_class(object = object, class = "mass_dataset")
    if (!is(pca_object, "prcomp")) {
      stop("pca_object should be prcomp class from run_pca().\n")
    }
    
    sample_info = object@sample_info
    
    if (missing(color_by)) {
      color_by = "no"
    } else{
      if (all(colnames(object@sample_info) != color_by)) {
        stop("no ", color_by, " in sample_info, please check.\n")
      }
    }
    
    if (color_by == "no") {
      plot =
        ggfortify:::autoplot.pca_common(
          object = pca_object,
          data = sample_info,
          x = x,
          y = y,
          size = point_size,
          shape = point_shape,
          alpha = point_alpha,
          frame = frame,
          frame.type = frame.type,
          ...
        ) +
        geom_vline(xintercept = 0, linetype = 2) +
        geom_hline(yintercept = 0, linetype = 2) +
        theme_bw() +
        theme(panel.grid.minor = element_blank())
    } else{
      plot =
        autoplot(
          object = pca_object,
          data = sample_info,
          x = x,
          y = y,
          fill = color_by,
          frame.colour = color_by,
          size = point_size,
          shape = point_shape,
          alpha = point_alpha,
          frame = frame,
          frame.type = frame.type,
          ...
        ) +
        geom_vline(xintercept = 0, linetype = 2) +
        geom_hline(yintercept = 0, linetype = 2) +
        theme_bw() +
        theme(panel.grid.minor = element_blank())
    }
    
    return(plot)
  }
