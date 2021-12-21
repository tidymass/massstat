#' @title mutate_fc
#' @description Calculate fold change.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object tidymass-class object.
#' @param control_sample_id A character vector.
#' @param case_sample_id A character vector
#' @param mean_median mean or median.
#' @return object with fold change (fc) in variable_info.
#' @export
#' @examples
#' library(massdataset)
#' library(tidyverse)
#' library(demodata)
#' 
#' data("liver_aging_pos", package = "demodata")
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
#' head(extract_variable_info(liver_aging_pos))
#' 
#' liver_aging_pos =
#'   mutate_fc(
#'     object = liver_aging_pos,
#'     control_sample_id = control_sample_id,
#'     case_sample_id = case_sample_id,
#'     mean_median = "median"
#'   )
#' 
#' head(extract_variable_info(liver_aging_pos))
#' 
#' extract_variable_info(liver_aging_pos) %>%
#'   ggplot(aes(fc, fc.1)) +
#'   geom_point()



mutate_fc = function(object,
                     control_sample_id,
                     case_sample_id,
                     mean_median = c("mean", "median")) {
  mean_median = match.arg(mean_median)
  massdataset::check_object_class(object = object, class = "mass_dataset")
  
  if (missing(control_sample_id) | missing(case_sample_id)) {
    stop("control_sample_id and/or case_sample_id are not provided.\n")
  }
  
  if (any(!control_sample_id %in% object@sample_info$sample_id)) {
    stop("some control_sample_id are not in object.\n")
  }
  
  if (any(!case_sample_id %in% object@sample_info$sample_id)) {
    stop("some case_sample_id are not in object.\n")
  }
  
  if (sum(is.na(object@expression_data)) > 0) {
    stop("Missing values in object (expression_data).\n")
  }
  
  if (length(control_sample_id) < 3 |
      length(case_sample_id) < 3) {
    stop("control or case group have less than 3 samples.\n")
  }
  
  cat(crayon::green(paste(
    length(control_sample_id), "control samples.\n"
  )))
  
  cat(crayon::green(paste(
    length(case_sample_id), "case samples.\n"
  )))
  
  control_index =
    match(control_sample_id, colnames(object@expression_data))
  
  case_index =
    match(case_sample_id, colnames(object@expression_data))
  
  expression_data =
    object@expression_data
  
  if (mean_median == "mean") {
    fc =
      apply(expression_data, 1, function(x) {
        x = as.numeric(x)
        mean(x[case_index], na.rm = TRUE) / mean(x[control_index], na.rm = TRUE)
      })
  } else{
    fc =
      apply(expression_data, 1, function(x) {
        x = as.numeric(x)
        median(x[case_index], na.rm = TRUE) / median(x[control_index], na.rm = TRUE)
      })
  }
  
  fc[is.na(fc)] = 1
  fc[is.infinite(fc)] = max(fc[!is.infinite(fc)])
  
  object@variable_info =
    data.frame(object@variable_info, fc, stringsAsFactors = FALSE)
  
  process_info = object@process_info
  
  parameter <- new(
    Class = "tidymass_parameter",
    pacakge_name = "massdataset",
    function_name = "mutate_fc()",
    parameter = list(
      "control_sample_id" = control_sample_id,
      case_sample_id = case_sample_id,
      mean_median = mean_median
    ),
    time = Sys.time()
  )
  
  if (all(names(process_info) != "mutate_fc")) {
    process_info$mutate_fc = parameter
  } else{
    process_info$mutate_fc = c(process_info$mutate_fc,
                               parameter)
  }
  
  object@process_info = process_info
  return(object)
}
