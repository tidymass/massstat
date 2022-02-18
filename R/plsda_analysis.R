#####PLS-DA is from mixOmics
#' @title plsda
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @method pls mass_dataset
#' @param X mass_dataset class
#' @param Y numeric matrix of response(s) with the rows as individual 
#' observations matching X. missing values (NAs) are allowed.
#' @param ncomp Positive Integer. The number of components to include in
#' the model. Default to 2.
#' @param scale Logical. If scale = TRUE, each block is standardized to zero
#' means and unit variances (default: TRUE)
#' @param mode Character string indicating the type of PLS algorithm to use.
#' One of "regression", "canonical", "invariant" or "classic". See Details.
#' @param tol Positive numeric used as convergence criteria/tolerance during
#' the iterative process. Default to 1e-06.
#' @param max.iter Integer, the maximum number of iterations. Default to 100.
#' @param near.zero.var Logical, see the internal nearZeroVar function
#' (should be set to TRUE in particular for data with many zero values).
#' Setting this argument to FALSE (when appropriate) will speed up the
#' computations. Default value is FALSE.
#' @param logratio Character, one of ('none','CLR') specifies the log ratio
#' transformation to deal with compositional values that may arise from
#' specific normalisation in sequencing data. Default to 'none'.
#' See ?logratio.transfo for details.
#' @param multilevel Numeric, design matrix for repeated measurement analysis,
#' where multilevel decomposition is required. For a one factor decomposition,
#' the repeated measures on each individual, i.e. the individuals ID is input
#' as the first column. For a 2 level factor decomposition then 2nd AND 3rd
#' columns indicate those factors. See examplesin ?spls.
#' @param all.outputs Logical. Computation can be faster when some specific
#' (and non-essential) outputs are not calculated. Default = TRUE.
#' @export
#' @rdname pls-mass_dataset
#' @return an object of class "pls"

# data("liver_aging_pos")
# 
# X <- liver_aging_pos
# X <-
#   X %>%
#   activate_mass_dataset(what = "sample_info") %>%
#   dplyr::filter(group != "QC")
# 
# Y <- X@sample_info$group
# Y = convert_dummy_variable(y = Y)
# 
# pls_object <-
#   pls(X, Y = Y, ncomp = 2)
# 
# plotIndiv(pls_object)
# plotVar(pls_object)

pls.mass_dataset <-
  function(X,
           Y = NULL,
           ncomp = 2,
           scale = FALSE,
           mode = c("regression", "canonical", "invariant", "classic"),
           tol = 1e-06,
           max.iter = 100,
           near.zero.var = FALSE,
           logratio = "none",
           multilevel = NULL,
           all.outputs = TRUE) {
    mode <- match.arg(mode)
    expression_data = X@expression_data
    
    linn.pls <- 
    mixOmics::pls(
      X = as.matrix(t(expression_data)),
      Y = Y,
      ncomp = ncomp,
      scale = FALSE,
      mode = mode, tol = tol,
      max.iter = max.iter, 
      near.zero.var = near.zero.var, 
      logratio = logratio,
      multilevel = multilevel, 
      all.outputs = all.outputs
    )
    return(linn.pls)
  }

#' @title convert_dummy_variable
#' @description convert_dummy_variable
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param y a vector of numeric or character
#' @return ggplot2 object
#' @importFrom fastDummies dummy_cols
#' @importFrom tibble column_to_rownames
#' @export
#' @examples
#' y = c(rep("a", 3), rep("b", 3))
#' convert_dummy_variable(y)
convert_dummy_variable <- function(y) {
  temp_y = 
    data.frame(y)
  dummy_variable <- 
  fastDummies::dummy_cols(temp_y) %>% 
    dplyr::select(-y) %>% 
    as.matrix()
  
  colnames(dummy_variable) <- 
    colnames(dummy_variable) %>% 
    stringr::str_replace("y\\_", "")
  
  return(dummy_variable)
}



#' @importFrom mixOmics pls
#' @export
mixOmics::pls