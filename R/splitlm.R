#' Extract attribute from a list of lm objects. 
#' 
#' @param object        : List of lm objects.
#'        attribute     : What to extract from the lm object.
#' @return A matrix or vector of attributes.
#' @examples
#' split_lm(object, coefficients)
#' split_lm(object, residuals)
split_lm <- function(object, attribute){
    
    if(attribute == "coefficients"){
        
        # intialize vector of coefficients
        result <- numeric(length(object))
        
        # extract coefficients from lm object to vector
        for(i in seq_along(object)){
            
            result[i] <- coefficients(object[[i]])
        }
        
    }else if(attribute == "residuals"){
        
        # intialize matrix of residuals
        result <- matrix(numeric(), length(residuals(object[[1]])), length(object))
        
        # extract residuals from lm object to matrix
        for(i in seq_along(object)){
            
            result[, i] <- residuals(object[[i]])
        }
        
        
    }else{
        
        stop("Only coefficients or residuals supported yet")
    }
    
    return(result)
    
}