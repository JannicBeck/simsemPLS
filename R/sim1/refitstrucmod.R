#' Refitting the structural model of a sempls or plspm object.
#' 
#' @param latent    : character of latent variables
#'        strucmod  : 2 column "source", "target" matrix of structural model 
#'        fscores   : matrix of factor scores
#' @return A list of objects of class lm.
#' @examples
#' refit_strucmod(latent, strucmod, fscores)
refit_strucmod <- function(latent, strucmod, fscores) { 
    
    # get endogenous latent variable names
    endogenous <- unique(strucmod[, 2])
    
    # initialize empty result list
    result <- vector("list", length(endogenous))
    
    # initialize result index 
    i <- 1

    # fit linear model for each endogenous latent variable
    for (dpndnt in endogenous) {            
        
        # get index/indices of endogenous/target latent variable in strucmod
        dpndnt.index <- which(dpndnt == strucmod[, 2])
            
        # get corresponding name(s) of independent/source latent variable(s)
        indpnt <- strucmod[dpndnt.index, 1] 
            
        # fit linear model and store lm object in result list
        result[[i]] <- lm(fscores[, dpndnt] ~ 0 + fscores[, indpnt])
        
        # increment result index by 1
        i <- i + 1
    }
    
    return(result) 
    
}
