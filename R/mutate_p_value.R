#' @title mutate_p_value
#' @description Calculate p values for variables.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param object tidymass-class object.
#' @param control_sample_id A character vector.
#' @param case_sample_id A character vector
#' @param method t test or wilcox test.
#' @param p_adjust_methods see ?p.adjust
#' @param return_mass_dataset logical default TRUE
#' @param ... other parameters
#' @return object with fold change in variable_info.
#' @export
#' @examples
#' library(massdataset)
#' library(tidyverse)
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
#'   mutate_p_value(
#'     object = liver_aging_pos,
#'     control_sample_id = control_sample_id,
#'     case_sample_id = case_sample_id,
#'     method = "t.test",
#'     p_adjust_methods = "BH"
#'   )
#'
#' head(extract_variable_info(liver_aging_pos))
#'
#' liver_aging_pos =
#'   mutate_p_value(
#'     object = liver_aging_pos,
#'     control_sample_id = control_sample_id,
#'     case_sample_id = case_sample_id,
#'     method = "wilcox.test",
#'     p_adjust_methods = "BH"
#'   )
#'
#' head(extract_variable_info(liver_aging_pos))
#'
#' extract_variable_info(liver_aging_pos) %>%
#'   ggplot(aes(-log(p_value_adjust, 10), -log(p_value_adjust.1, 10))) +
#'   geom_point()

mutate_p_value <-
  function(object,
           control_sample_id,
           case_sample_id,
           method = c("t.test", "wilcox.test"),
           p_adjust_methods = c("holm",
                                "hochberg",
                                "hommel",
                                "bonferroni",
                                "BH",
                                "BY",
                                "fdr",
                                "none"),
           return_mass_dataset = TRUE,
           ...) {
    method <- match.arg(method)
    p_adjust_methods = match.arg(p_adjust_methods)
    
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
    
    control_index <-
      match(control_sample_id, colnames(object@expression_data))
    
    case_index <-
      match(case_sample_id, colnames(object@expression_data))
    
    expression_data <-
      object@expression_data
    
    if (method == "t.test") {
      p_value <-
        apply(expression_data, 1, function(x) {
          x = as.numeric(x)
          test =
            tryCatch(
              expr = t.test(x = x[control_index], y = x[case_index],
                            ...),
              error = function(e) {
                NA
              }
            )
          
          if (is(test, "htest")) {
            p = test$p.value
          } else{
            p = 1
          }
        })
    } else{
      p_value <-
        apply(expression_data, 1, function(x) {
          x = as.numeric(x)
          test =
            tryCatch(
              expr = wilcox.test(x = x[control_index], y = x[case_index],
                                 ...),
              error = function(e) {
                NA
              }
            )
          
          if (is(test, "htest")) {
            p = test$p.value
          } else{
            p = 1
          }
        })
    }
    
    p_value_adjust <-
      p.adjust(p_value, method = p_adjust_methods)
    
    
    if (!return_mass_dataset) {
      temp <-
        data.frame(
          variable_id = object@variable_info$variable_id,
          p_value = p_value,
          p_value_adjust = p_value_adjust
        )
      return(temp)
    }
    
    new_column_name <-
      massdataset::check_column_name(object@variable_info,
                                     column.name = "p_value")
    
    object@variable_info <-
      cbind(object@variable_info,
            p_value = p_value) %>%
      as.data.frame()
    
    colnames(object@variable_info)[ncol(object@variable_info)] <-
      new_column_name
    
    new_column_name <-
      massdataset::check_column_name(object@variable_info ,
                                     column.name = "p_value_adjust")
    
    object@variable_info <-
      cbind(object@variable_info,
            p_value_adjust = p_value_adjust) %>%
      as.data.frame()
    
    colnames(object@variable_info)[ncol(object@variable_info)] <-
      new_column_name
    
    process_info <- object@process_info
    
    parameter <- new(
      Class = "tidymass_parameter",
      pacakge_name = "massdataset",
      function_name = "mutate_p_value()",
      parameter = list(
        "control_sample_id" = control_sample_id,
        case_sample_id = case_sample_id,
        method = method,
        p_adjust_methods = p_adjust_methods
      ),
      time = Sys.time()
    )
    
    if (all(names(process_info) != "mutate_p_value")) {
      process_info$mutate_p_value <- parameter
    } else{
      process_info$mutate_p_value <-
        c(process_info$mutate_p_value,
          parameter)
    }
    
    object@process_info <- process_info
    return(object)
  }
