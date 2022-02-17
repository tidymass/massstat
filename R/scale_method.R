#' @title scale_data
#' @description Scale data.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param x data.frame. Rows are variables and columns are samples.
#' @param center center or not. TRUE or FALSE.
#' @param method auto, uv, range, pareto or none.
#' @return scaled data.frame.
#' @export
#' @examples
#' x <- as.data.frame(matrix(1:10, ncol = 5))
#' colnames(x) = letters[1:5]
#' scale_data(x, method = "auto")
#' t(scale(t(x)))
#' scale_data(x, method = "range")
#' scale_data(x, method = "pareto")
#' scale_data(x, method = "none")

scale_data <-
  function (x,
            center = TRUE,
            method = c("auto", "uv", "range", "pareto", "none")) {
    method = match.arg(method)
    x <- as.matrix(x)
    nr <- nrow(x)
    if (center) {
      center_value <- rowMeans(x, na.rm = TRUE)
      x <-
        sweep(
          x = x,
          MARGIN = 1L,
          STATS = center_value,
          check.margin = FALSE
        )
    }
    
    if (method == "none") {
      return(x)
    }
    
    ###auto scale or uv
    ##Unit-Variance (UV) scale each variable (column).
    ##UV-scaling applied as (value - mean) / stdev.
    ##Unit-Variance Scaling or Autoscaling,
    ##is commonly applied and uses the standard deviation as the scaling factor.
    if (method == "auto" | method == "uv") {
      scale_value <- apply(
        X = x,
        MARGIN = 1L,
        FUN = function(y) {
          sd(y, na.rm = TRUE)
        }
      )
    }
    
    #####range scale
    if (method == "range") {
      scale_value <- apply(
        X = x,
        MARGIN = 1L,
        FUN = function(y) {
          range(y, na.rm = TRUE)[2] - range(y, na.rm = TRUE)[1]
        }
      )
    }
    
    #####pareto scale
    if (method == "pareto") {
      scale_value <- apply(
        X = x,
        MARGIN = 1L,
        FUN = function(y) {
          sqrt(sd(y, na.rm = TRUE))
        }
      )
    }
    
    #####vast scale
    if (method == "vast") {
      scale_value <- apply(
        X = x,
        MARGIN = 1L,
        FUN = function(y) {
          sqrt(sd(y, na.rm = TRUE))
        }
      )
    }
    
    x <-
      sweep(
        x = x,
        MARGIN = 1L,
        STATS = scale_value,
        FUN = "/",
        check.margin = FALSE
      )
    return(as.data.frame(x))
  }


#' @title scale_data
#' @method scale_data mass_dataset
#' @param x x
#' @param center center
#' @param method method for scaling.
#' @export
#' @rdname statistics-mass_dataset
#' @return mass_dataset object

scale_data.mass_dataset <- function(
  x,
  center = TRUE,
  method = c("auto", "uv", "range", "pareto", "none")
){
  method <- match.arg(method)
  expression_data <- x@expression_data
  
  expression_data <- 
    scale_data(expression_data, center = center, method = method)
  
  x@expression_data = expression_data
  
  process_info = x@process_info
  
  parameter <- new(
    Class = "tidymass_parameter",
    pacakge_name = "massstat",
    function_name = "scale_data()",
    parameter = list("center" = center,
                     "method" = method),
    time = Sys.time()
  )
  
  if (all(names(process_info) != "scale_data")) {
    process_info$scale = parameter
  } else{
    process_info$scale = c(process_info$scale, parameter)
  }
  
  x@process_info = process_info
  
  return(x)
  
}