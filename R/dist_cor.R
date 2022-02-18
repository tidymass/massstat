#' @title dist_mass_dataset
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param x mass_dataset class
#' @param margin sample or variable
#' @param method the distance measure to be used.
#' This must be one of "euclidean", "maximum",
#' "manhattan", "canberra", "binary" or "minkowski".
#' Any unambiguous substring can be given.
#' @param diag logical value indicating whether the diagonal of the
#' distance matrix should be printed by print.dist.
#' @param upper logical value indicating whether the upper triangle of
#' the distance matrix should be printed by print.dist.
#' @param p The power of the Minkowski distance.
#' @export
#' @return dist returns an object of class "dist".
#' @examples
#' library(massdataset)
#' library(tidyverse)
#' data("expression_data")
#' data("sample_info")
#' data("sample_info_note")
#' data("variable_info")
#' data("variable_info_note")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'     sample_info_note = sample_info_note,
#'     variable_info_note = variable_info_note
#'   )
#' object
#' x =
#'   object %>%
#'   log(2) %>%
#'   scale()
#' variable_distance <-
#'   dist_mass_dataset(x = x, margin = "variable")
#' head(as.matrix(variable_distance)[, 1:5])
#' sample_distance <-
#'   dist_mass_dataset(x = x, margin = "sample")
#' head(as.matrix(sample_distance)[, 1:5])

dist_mass_dataset <-
  function(x,
           margin = c("variable", "sample"),
           method = c("euclidean",
                      "maximum",
                      "manhattan",
                      "canberra",
                      "binary",
                      "minkowski"),
           diag = FALSE,
           upper = FALSE,
           p = 2) {
    margin <- match.arg(margin)
    method <- match.arg(method)
    expression_data <- x@expression_data
    if (margin == "variable") {
      dist_value <-
        dist(
          expression_data,
          method = method,
          diag = diag,
          upper = upper,
          p = p
        )
    } else{
      dist_value <-
        dist(
          t(expression_data),
          method = method,
          diag = diag,
          upper = upper,
          p = p
        )
    }
    return(dist_value)
  }



#' @title cor_mass_dataset
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param x mass_dataset class
#' @param y NULL
#' @param margin sample or variable
#' @param use an optional character string giving a method for computing
#' covariances in the presence of missing values. This must be
#' (an abbreviation of) one of the strings "everything", "all.obs",
#' "complete.obs", "na.or.complete", or "pairwise.complete.obs".
#' @param method a character string indicating which correlation coefficient
#' (or covariance) is to be computed. One of "pearson" (default),
#' "kendall", or "spearman": can be abbreviated.
#' @export
#' @return dist returns an object of class "dist".
#' @examples
#' library(massdataset)
#' library(tidyverse)
#' data("expression_data")
#' data("sample_info")
#' data("sample_info_note")
#' data("variable_info")
#' data("variable_info_note")
#' object =
#'   create_mass_dataset(
#'     expression_data = expression_data,
#'     sample_info = sample_info,
#'     variable_info = variable_info,
#'     sample_info_note = sample_info_note,
#'     variable_info_note = variable_info_note
#'   )
#' object
#' x =
#'   object %>%
#'   log(2) %>%
#'   scale()
#' 
#' variable_cor <-
#'   cor_mass_dataset(x = x, margin = "variable")
#' head(as.matrix(variable_cor)[, 1:5])
#' sample_cor <-
#'   cor_mass_dataset(x = x, margin = "sample")
#' head(as.matrix(sample_cor)[, 1:5])

cor_mass_dataset <-
  function(x,
           y = NULL,
           margin = c("variable", "sample"),
           use = c("everything",
                   "all.obs",
                   "complete.obs",
                   "na.or.complete",
                   "pairwise.complete.obs"),
           method = c("pearson",
                      "kendall",
                      "spearman")) {
    margin <- match.arg(margin)
    method <- match.arg(method)
    use <- match.arg(use)
    expression_data <- x@expression_data
    
    if (margin == "variable") {
      cor_value <-
        cor(
          x = t(expression_data),
          y = NULL,
          use = use,
          method = method
        )
    } else{
      cor_value <-
        cor(
          x = expression_data,
          y = NULL,
          use = use,
          method = method
        )
    }
    return(cor_value)
  }
