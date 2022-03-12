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
#' library(magrittr)
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
#' @param data_type wider or longer
#' @param p_adjust_method see ?p.adjust
#' @export
#' @importFrom stats p.adjust
#' @importFrom Hmisc rcorr
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter left_join
#' @importFrom magrittr %>%
#' @return dist returns an object of class "dist".
#' @examples
#' library(massdataset)
#' library(magrittr)
#' library(dplyr)
#' 
#' data("liver_aging_pos")
#' liver_aging_pos
#' 
#' qc_id <-
#'   liver_aging_pos %>%
#'   activate_mass_dataset(what = "sample_info") %>%
#'   dplyr::filter(group == "QC") %>%
#'   dplyr::pull(sample_id)
#' 
#' object <-
#'   mutate_rsd(liver_aging_pos, according_to_samples = qc_id)
#' 
#' ###only remain the features with rt > 100, mz > 150 and rsd < 30
#' object <-
#'   object %>%
#'   activate_mass_dataset(what = "variable_info") %>%
#'   dplyr::filter(rt > 100) %>%
#'   dplyr::filter(mz > 150) %>%
#'   dplyr::filter(rsd < 30)
#' 
#' ##only remain the week 24 samples
#' object <-
#'   object %>%
#'   activate_mass_dataset(what = "sample_info") %>%
#'   dplyr::filter(group == "24W")
#' 
#' dim(object)
#' 
#' object <-
#'   object %>%
#'   `+`(1) %>%
#'   log(10) %>%
#'   scale_data(method = "auto")
#' 
#' cor_data <-
#'   object %>%
#'   cor_mass_dataset(margin = "variable", data_type = "wider")
#' 
#' head(cor_data$correlation[,1:5])
#' head(cor_data$p_value[,1:5])
#' head(cor_data$n[,1:5])
#' 
#' cor_data <-
#'   object %>%
#'   cor_mass_dataset(margin = "variable", data_type = "longer")
#' 
#' head(cor_data)

cor_mass_dataset <-
  function(x,
           y = NULL,
           margin = c("variable", "sample"),
           use = c("everything",
                   "all.obs",
                   "complete.obs",
                   "na.or.complete",
                   "pairwise.complete.obs"),
           method = c("spearman",
                      "pearson",
                      "kendall"),
           data_type = c("wider", "longer"),
           p_adjust_method = c(c(
             "BH",
             "holm",
             "hochberg",
             "hommel",
             "bonferroni",
             "BY",
             "fdr",
             "none"
           ))) {
    margin <- match.arg(margin)
    method <- match.arg(method)
    use <- match.arg(use)
    data_type <- match.arg(data_type)
    p_adjust_method <- match.arg(p_adjust_method)
    
    expression_data <- x@expression_data
    
    if (margin == "variable") {
      cor_data <-
        Hmisc::rcorr(x = t(expression_data), type = method)
    } else{
      cor_data <-
        Hmisc::rcorr(x = as.matrix(expression_data), type = method)
    }
    
    return_result <-
      list(correlation = NULL,
           p_value = NULL,
           n = NULL)
    
    return_result$correlation <-
      as.data.frame(cor_data$r)
    
    return_result$p_value <-
      as.data.frame(cor_data$P)
    
    return_result$n <-
      as.data.frame(cor_data$n)
    
    rm(list = c("cor_data"))
    
    if (data_type == "wider") {
      return(return_result)
    } else{
      return_result <-
        return_result %>%
        lapply(function(y) {
          y <- as.matrix(y)
          y[upper.tri(y)] <- NA
          diag(y) <- NA
          y <-
            y %>%
            as.data.frame() %>%
            tibble::rownames_to_column(var = "from") %>%
            tidyr::pivot_longer(cols = -from,
                                names_to = "to",
                                values_to = "v") %>%
            dplyr::filter(!is.na(v))
          as.data.frame(y)
        })
      
      return_result <-
        return_result[[1]] %>%
        dplyr::left_join(return_result[[2]], by = c("from", "to")) %>%
        dplyr::left_join(return_result[[3]], by = c("from", "to"))
      
      colnames(return_result) <-
        c("from", "to", "correlation", "p_value", "number")
      return_result$p_adjust <-
        p.adjust(return_result$p_value,
                 method =  p_adjust_method)
      return(return_result)
    }
  }
