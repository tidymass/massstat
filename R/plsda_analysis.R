#' @title plsda
#' @method plsda mass_dataset
#' @param X mass_dataset
#' @param Y a factor or a class vector for the discrete outcome.
#' @param ncomp ncomp
#' @param scale scale
#' @param tol tol
#' @param max.iter max.iter
#' @param near.zero.var near.zero.var
#' @param logratio logratio
#' @param multilevel multilevel
#' @param all.outputs all.outputs
#' @export
#' @rdname multivariate-mass_dataset
#' @importFrom mixOmics plsda plotIndiv plotVar
#' @return A plsda class

setMethod(f = "plsda",
          signature(X = "mass_dataset"),
          function (X,
                    Y = NULL,
                    ncomp = 2,
                    scale = FALSE,
                    tol = 1e-06,
                    max.iter = 100,
                    near.zero.var = FALSE,
                    logratio = "none",
                    multilevel = NULL,
                    all.outputs = TRUE) {
            expression_data = X@expression_data
            
            linn.pls <- 
              mixOmics::plsda(
                X = as.matrix(t(expression_data)),
                Y = Y,
                ncomp = ncomp,
                scale = FALSE,
                tol = tol,
                max.iter = max.iter, 
                near.zero.var = near.zero.var, 
                logratio = logratio,
                multilevel = multilevel, 
                all.outputs = all.outputs
              )
            return(linn.pls)
          })


#' @importFrom mixOmics plsda
#' @export
mixOmics::plsda