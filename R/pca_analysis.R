#' @title run_pca
#' @description Boxplot for each samples.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object tidymass-class object.
#' @param color_by which column (sample_info) is used to color samples
#' @param point_alpha point_alpha
#' @param frame ?ggplot2::autoplot
#' @param frame.type ?ggplot2::autoplot
#' @param ... other paramters for ggplot2::autoplot
#' @return ggplot2 plot.
#' @export
#' @examples
#' library(massdataset)
#' library(demodata)
#' library(tidyverse)
#' data("liver_aging_pos", package = "demodata")
#' liver_aging_pos
#' 
#' liver_aging_pos %>%
#'   run_pca(frame = TRUE)
#' 
#' liver_aging_pos %>%
#'   scale() %>% 
#'   run_pca(color_by = "group",
#'           loadings = TRUE) +
#'   ggsci::scale_fill_lancet() +
#'   ggsci::scale_color_lancet() 
#' 
#' liver_aging_pos %>%
#'   activate_mass_dataset(what = "sample_info") %>% 
#'   dplyr::filter(class == "Subject") %>% 
#'   scale %>%
#'   run_pca(color_by = "group", frame = TRUE) +
#'   ggsci::scale_fill_lancet() +
#'   ggsci::scale_color_lancet() +
#'   ggrepel::geom_text_repel(aes(label = sample_id))

run_pca = function(object,
                   color_by,
                   point_alpha = 0.8,
                   frame = TRUE,
                   frame.type = 'norm',
                   ...) {
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
  
  if (missing(color_by)) {
    color_by = "no"
  } else{
    if (all(colnames(object@sample_info) != color_by)) {
      stop("no ", color_by, " in sample_info, please check.\n")
    }
  }
  
  if (all(names(object@process_info) != "scale")) {
    warning("no scale for this dataset, try to scale() before pca.\n")
  }
  
  pca_object = prcomp(x = t(as.matrix(expression_data)),
                      center = FALSE,
                      scale. = FALSE)
  
  if (color_by == "no") {
    plot =
      ggfortify:::autoplot.pca_common(
        object = pca_object,
        data = sample_info,
        size = 5,
        shape = 21,
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
        fill = color_by,
        frame.colour = color_by,
        size = 5,
        shape = 21,
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
