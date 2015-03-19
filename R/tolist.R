#' Convert object from matrix or vector to list following a given structural model.
#' 
#' @param object        : Vector of coefficients or matrix of residuals
#'        strucmod      : A structural model of a plsm object.
#' @return A list of residuals or coefficients.
#' @examples
#' to_list(object, strucmod)
to_list <- function(object, strucmod){

    
    # get endogenous latent variable names
    endogenous <- unique(strucmod[, 2])
    
    # intialize result list
    result <- vector("list", length(endogenous))
    
    # convert vector or matrix to list 
    for(i in seq_along(result)){
        
        for (dpndnt in endogenous[i]) {            
            
            # get index/indices of endogenous/target latent variable in strucmod
            dpndnt.index <- which(dpndnt == strucmod[, 2])
            
            if(is.vector(object)){
                
                # write coefficients vector in list
                result[[i]] <- object[dpndnt.index]
            }else{
                
                # write residual matrix in list
                result[[i]] <- object[, dpndnt.index]
            }

            
        }
        
    }
    
    return(result)
    
}