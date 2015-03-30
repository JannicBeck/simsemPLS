#' Simulate data on given plsm object.
#' 
#' To set up the covariance matrices type \code{scoeffs <- object$D; data.entry(object$D)} for the structural model
#' and \code{mcoeffs <- object$M; data.entry(mcoeffs)} for the measurement model.
#' Note that the correlation of a latent variable measured by exactly 1 manifest variable is by definition 1.
#' 
#' @param object            : An object of class plsm as returned by the method plsm.
#' @param nmonte            : The number of monte carlo replications; the default is 100.
#' @param nobs              : The number of observations; the default is 250.
#' @param scoeffs           : A covariance matrix of latent variable scores.
#' @param mcoeffs           : A correlation matrix of manifest variables on latent variables.
#' @return An object of class simsempls.
#' @examples
#' simplsm(object, nmonte = 10, scoeffs = scoeffs, mcoeffs = mcoeffs)
#'        
simplsm <- function(object, nmonte = 100, nobs = 250, scoeffs = NULL, mcoeffs = NULL, FUN = "sempls", ...) {

    
    # check if object is of type plsm    
    if(is(object, "plsm")){        
        
        if(!(is.matrix(scoeffs)) || !(is.matrix(mcoeffs))){
            
            stop("Covariance matrices must be supplied!")
        }
        
        # get measurement blocks
        blocks <- object$blocks
        
        # check if measurement model is specified reflective
        is_reflective(blocks)
           
    }else{
        
        # object is not of type plsm
        stop("The supplied object must be of class plsm")
    }
    
    # require MASS package
    if(!(require(MASS))){
        
        stop("The package 'MASS' is required, type: install.packages(MASS)")
    }
    
    
    if(FUN == "matrixpls.sempls"){
        
        if(!(require(matrixpls))){
            
            stop("The package 'matrixpls' is required, type: install.packages(matrixpls)")
        }
        
        FUN <- matrixpls.sempls
    }else{
        
        if(!(require(semPLS))){
            
            stop("The package 'sempls' is required, type: install.packages(sempls)")
        }
        FUN <- sempls
    }
    
    result <- core_2(object, nmonte, nobs, scoeffs, mcoeffs, FUN, ...)
    
    return(result)
}




