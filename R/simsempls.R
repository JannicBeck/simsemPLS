#' Simulate data on given sempls object.
#'  
#' The exogenous latent variable scores are simulated according to the 
#' normal distribution.
#' Residuals, coefficients and number of observations are fixed to the 
#' values of the sempls object. 
#' The endogenous latent variable scores and manifest variables (the data) 
#' are calculated based on their linear equations.
#' 
#' @param object            : An object of class sempls as returned by the method sempls.
#' @param nmonte            : The number of monte carlo replications; the default is 100.
#' @return An object of class simsempls.
#' @examples
#' simsempls(object, 100)
#'       
simsempls <- function(object, nmonte = 100) {
  
    # check if object is of type sempls
    if(is(object, "sempls")){
        
        # get measurement blocks
        blocks <- object$model$blocks
        
        # check if measurement model is specified reflective
        is_reflective(blocks)
            
        # get names of latent variables
        latent <- object$model$latent
        
        # get names of manifest variables
        manifest <- object$model$manifest
        
        # get data
        data <- object$data
        
        # get structural model
        strucmod <- object$model$strucmod 
        
        # get measurement model
        measuremod <- object$model$measuremod
        
        # get matrix of estimated factor scores
        fscores <- object$factor_scores  
        
        # get the number of valid observations
        nobs <- object$N-length(object$incomplete)
        
        # refit structural model with lm function to get residuals
        strucmod.lm <- refit_strucmod(latent, strucmod, fscores)
        
        # refit measurement model with lm function to get residuals
        measuremod.lm <- refit_measuremod(data, measuremod, fscores)
    
        # TODO: doesn't work here cause list item has multiple coefficients
        # also code of simfscores is affected by non list structural coeffs
        # extract structural coefficients from lm object
        # scoeffs <- split_lm(strucmod.lm, "coefficients")
        
        # get coefficients from sempls object instead
        # TODO: messy code simplify!
        scoeffs <- object$path_coefficients[object$path_coefficients != 0]
        
        # convert vector of structural coefficients to list 
        scoeffs <- to_list(scoeffs, strucmod)
        
        # extract measurement coefficients from lm object
        mcoeffs <- split_lm(measuremod.lm, "coefficients")
        
        # extract structural residuals from lm object
        sresid <- split_lm(strucmod.lm, "residuals")
        
        # extract measurement residuals from lm object
        mresid <- split_lm(measuremod.lm, "residuals")
        
    }else{
        
        # object is not of type sempls 
        stop("The supplied object must be of class sempls")
    }
    
    # simulate data
    result <- core(object$model, nmonte, nobs, latent, manifest, strucmod, measuremod, 
                   scoeffs, sresid, mcoeffs, mresid)
    
    return(result)
}





