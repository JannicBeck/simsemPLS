#' Simulate data on given plsm object.
#' 
#' The exogenous latent variable scores are simulated, according to 
#' the normal distribution. 
#' Coefficients for the structural and measurement model must be supplied.
#' Residuals of the structural and measurement model can be supplied.
#' If residuals are not supplied, they are simulated by rnorm with the 
#' number of observations; the default is 200.
#' The endogenous latent variable scores and manifest variables (the data) 
#' are calculated based on their linear equations.
#' 
#' @param object            : An object of class plsm as returned by the method plsm.
#' @param nmonte            : The number of monte carlo replications; the default is 100.
#' @param nobs              : The number of observations; the default is 200.
#' @param scoeffs           : A vector of coefficients for the structural model.
#' @param mcoeffs           : A vector of coefficients for the measurement model.
#' @param sresid            : A matrix of residuals for the structural model.
#' @param mresid            : A matrix of residuals for the measurement model.
#' @return An object of class simplsm.
#' @examples
#' simplsm(object, nmonte = 10, scoeffs = scoeffs, mcoeffs = mcoeffs)      
#' simplsm(object, nmonte = 10, nobs = 300, scoeffs = scoeffs, mcoeffs = mcoeffs) 
#' simplsm(object, nmonte = 10, scoeffs = scoeffs, mcoeffs = mcoeffs, sresid = sresid)              
#' simplsm(object, nmonte = 10, scoeffs = scoeffs, mcoeffs = mcoeffs, mresid = mresid)    
#' simplsm(object, nmonte = 10, scoeffs = scoeffs, mcoeffs = mcoeffs, sresid = sresid, mresid = mresid) 
#'        
simplsm <- function(object, nmonte = 100, nobs = 200, scoeffs = NULL, mcoeffs = NULL,
                      sresid = NULL, mresid = NULL) {

    
    # check if object is of type plsm    
    if(is(object, "plsm")){        
        
        # test for coefficients
        # TODO test for size of matrix
        if(is.null(scoeffs) || is.null(mcoeffs)){
            
            stop("Coefficients must be supplied")
        }
        
        # get measurement blocks
        blocks <- object$blocks
        
        # check if measurement model is specified reflective
        is_reflective(blocks)
        
        # get names of latent variables
        latent <- object$latent
        
        # get names of manifest variables
        manifest <- object$manifest
        
        # get structural model
        strucmod <- object$strucmod 
        
        # get measurement model
        measuremod <- object$measuremod
        
        # test if residuals of structural model are supplied 
        # TODO: simplify this messy code!
        if(!(is.null(sresid)) || !(is.null(mresid))){
            
            # get index of supplied residuals
            i <- min(which(!(c(is.null(sresid), is.null(mresid)))))
            
            # get the number of observations from the supplied residuals
            ifelse(i == 1, nobs <- nrow(sresid), nobs <- nrow(mresid))
            
        }
        
        # test if residuals of structural model are supplied 
        if(is.null(sresid)){
            
            # simulate residuals by rnorm if not supplied
            sresid <- sim_resid(strucmod, nobs)
        }
        
        # test if residuals of measurement model are supplied
        if(is.null(mresid)){
            
            # simulate residuals by rnorm if not supplied
            mresid <- sim_resid(measuremod, nobs)
        }
        
        # convert vector of structural coefficients to list 
        scoeffs <- to_list(scoeffs, strucmod)
           
    }else{
        
        # object is not of type plsm
        stop("The supplied object must be of class plsm")
    }
    
    # simulate data
    result <- core(object, nmonte, nobs, latent, manifest, strucmod, measuremod, 
                   scoeffs, sresid, mcoeffs, mresid)
    
    return(result)
}




