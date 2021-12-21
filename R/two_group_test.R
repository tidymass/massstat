#' @title mutate_p_value
#' @description Calculate p values for variables.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object tidymass-class object.
#' @param by one column of sample_info from object.
#' @param control_group A character vector, names from the by.
#' @param case_group A character vector, names from the by.
#' @param method t test or wilcox test.
#' @param p_adjust_methods see ?p.adjust
#' @return object with fold change in variable_info.
#' @export

mutate_p_value = function(object,
                             by,
                             control_group,
                             case_group,
                             method = c("t", "wilcox"),
                             p_adjust_methods = c("holm",
                                                  "hochberg",
                                                  "hommel",
                                                  "bonferroni",
                                                  "BH",
                                                  "BY",
                                                  "fdr",
                                                  "none")) {
  method = match.arg(method)
  p_adjust_methods = match.arg(p_adjust_methods)
  
  if (class(object)[1] != "tidymass") {
    stop("only for tidymass-class object.\n")
  }
  
  sample_info = object@sample_info
  expression_data = object@expression_data
  
  if (missing(by)) {
    stop("by is not provided.\n")
  }
  
  if (!by %in% colnames(sample_info)) {
    stop(by, " is not in the sample_info.\n")
  }
  
  if (sum(is.na(expression_data)) > 0) {
    warning("NA will be removed in the calculation\n")
  }
  
  if (missing(control_group) | missing(case_group)) {
    stop("control_group and/or case_group are not provided.\n")
  }
  
  control_index = which(sample_info[, by] %in% control_group)
  case_index = which(sample_info[, by] %in% case_group)
  
  if (length(control_index) < 3 |
      length(case_index) < 3) {
    stop("control or case group have less than 3 samples.\n")
  }
  
  cat(crayon::green(paste(
    length(control_index), "control samples.\n"
  )))
  cat(crayon::green(paste(length(case_index), "case samples.\n")))
  
  control_name = paste(control_group, collapse = "_")
  case_name = paste(case_group, collapse = "_")
  
  if (method == "t") {
    p_value =
      apply(expression_data, 1, function(x) {
        x = as.numeric(x)
        test =
          tryCatch(
            expr = t.test(x = x[control_index], y = x[case_index]),
            error = function(e) {
              NA
            }
          )
        
        if (class(test) == "htest") {
          p = test$p.value
        } else{
          p = 1
        }
      })
  } else{
    p_value =
      apply(expression_data, 1, function(x) {
        x = as.numeric(x)
        test =
          tryCatch(
            expr = wilcox.test(x = x[control_index], y = x[case_index]),
            error = function(e) {
              NA
            }
          )
        
        if (class(test) == "htest") {
          p = test$p.value
        } else{
          p = 1
        }
      })
  }
  
  p_value_adjust = p.adjust(p_value, method = p_adjust_methods)
  
  object@variable_info =
    data.frame(object@variable_info,
               p_value,
               p_value_adjust,
               stringsAsFactors = FALSE)
  
  return(object)
  
}
