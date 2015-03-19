#' Refitting the measurement model of a sempls or plspm object.
#' 
#' @param measuremod    : 2 column "source", "target" matrix of measurement model 
#'        fscores       : matrix of factor scores
#'        data          : matrix of scaled dataset 
#' @return A list of objects of class lm.
#' @examples
#' refit_measuremod(manifest, measuremod, fscores)
refit_measuremod <- function(data, measuremod, fscores) { 
    
    # get independent latent variable names
    indpndt.latent <- measuremod[, 1]
    
    # initialize empty result list
    result <- vector("list", length(indpndt.latent))           
        
    for(i in seq_along(result)){
        
        result[[i]] <- lm(data[, i] ~ 0 + fscores[, indpndt.latent[i]])
    }
    
    return(result) 
    
}

