#' @title pls
#' @param X mass_dataset
#' @param Y Y
#' @param ncomp ncomp
#' @param scale scale
#' @param mode mode
#' @param tol tol
#' @param max.iter max.iter
#' @param near.zero.var near.zero.var
#' @param logratio logratio
#' @param multilevel multilevel
#' @param all.outputs all.outputs
#' @importFrom mixOmics pls plotIndiv plotVar
#' @return A pls class
#' @export

pls <-
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
    UseMethod("pls")
  }

#' @export
pls.default <-
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
    mixOmics::pls(
      X = X,
      Y = Y,
      ncomp = ncomp,
      scale = scale,
      mode = mode,
      tol = tol,
      max.iter = max.iter,
      near.zero.var = near.zero.var,
      logratio = logratio,
      multilevel = multilevel,
      all.outputs = all.outputs
    )
  }


#' @export
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
        mode = mode,
        tol = tol,
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
