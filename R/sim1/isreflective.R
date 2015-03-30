#' Test if measurement model of a SEM is specified reflective. 
#' 
#' @param object    : Blocks of a sempls or plsm object.
#' @return Stops if model has formative block.
#' @examples
#' is_reflective(blocks)
is_reflective <- function(blocks){

    # check if measurement model is specified reflective
    for(i in seq_along(blocks)){
        
        if(attr(blocks[[i]], "mode") != "A"){
            
            # stop if block is of mode B (formative)
            stop("Formative models not supported yet")
        }
        
    }  
    
}