#' Simulate data on given lavaan object with the simsem package.
#' 
#' @param object            : A character of the equations and coefficients in lavaan syntax.
#' @param nmonte            : The number of monte carlo replications; the default is 100.
#' @param nobs              : The number of observations; the default is 250.
#' @return An object of class simsempls.
#' @examples
#' simlavaan(object, lavaan.model, nmonte = 100, nobs = 250, FUN = "sempls")
#'        
simplsm <- function(object, nmonte = 100, nobs = 250, scoeffs = NULL, mcoeffs = NULL, FUN = "sempls", ...) {
    
    # require simsem package
    if(!(require(simsem))){
        
        stop("The package 'simsem' is required, type: install.packages(simsem)")
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
    
    result <- core_3(object, lavaan.model, nmonte, nobs, scoeffs = scoeffs, mcoeffs = mcoeffs, FUN, ...)
    
    return(result)
}



