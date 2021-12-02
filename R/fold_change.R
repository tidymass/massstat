#' @title calculate_fc
#' @description Calculate fold change.
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param object tidymass-class object.
#' @param by one column of sample_info from object.
#' @param control_group A character vector, names from the by.
#' @param case_group A character vector, names from the by.
#' @param mean_median mean or median.
#' @return object with fold change in variable_info.
#' @export
#' @examples 
#' library(massdataset)
#' library(ggplot2)
#' data("expression_data")
#' data("sample_info")
#' data("sample_info_note")
#' data("variable_info")
#' data("variable_info_note")
#' 
#' object =
#' create_tidymass_class(
#' expression_data = expression_data,
#' sample_info = sample_info,
#' variable_info = variable_info,
#' sample_info_note = sample_info_note,
#' variable_info_note = variable_info_note
#' )
#' object =
#'   calculate_fc(
#'   object = object,
#'   by = "class",
#'   control_group = c("Blank", "QC"),
#'   case_group = c("Subject"),
#'   mean_median = "mean"
#' )
#' head(extract_variable_info(object))
#' object =
#'   calculate_fc(
#'     object = object,
#'     by = "class",
#'     control_group = c("Blank", "QC"),
#'     case_group = c("Subject"),
#'     mean_median = "median"
#'   )
#' head(extract_variable_info(object))

calculate_fc = function(object,
                        by,
                        control_group,
                        case_group,
                        mean_median = c("mean", "median")) {
  
  mean_median = match.arg(mean_median)
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
  
  if(missing(control_group) | missing(case_group)){
    stop("control_group and/or case_group are not provided.\n")
  }
  
  control_index = which(sample_info[,by] %in% control_group)
  case_index = which(sample_info[,by] %in% case_group)
  
  if(length(control_index) < 3 |
     length(case_index) < 3) {
    stop("control or case group have less than 3 samples.\n")
  }
  
  cat(crayon::green(paste(length(control_index), "control samples.\n")))
  cat(crayon::green(paste(length(case_index), "case samples.\n")))
  
  control_name = paste(control_group, collapse = "_")
  case_name = paste(case_group, collapse = "_")
  
  if(mean_median == "mean"){
    fc = 
    apply(expression_data, 1, function(x) {
      x = as.numeric(x)
      mean(x[case_index], na.rm = TRUE) / mean(x[control_index], na.rm = TRUE)
    })  
  }else{
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
  
  return(object)
  
}