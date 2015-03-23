#' Simulate residuals on given structural or measurement model of a plsm object.
#' 
#' @param object        : structural or measurement model of a plsm object like returned by method plsm.
#'        nobs          : number of observations.
#' @return A residual matrix simulated with rnorm.
#' @examples
#' sim_resid(object, 200)
#' 
#' TODO: Implement different simulations based on parameter dist or func f.e. qnorm pnorm
sim_resid <- function(object, nobs, FUN) { 
    
    # simulate normally distributed residuals for equations
    result <- matrix(FUN(nobs * length(unique(object[, 2])), mean=0, sd=1), nobs, length(unique(object[, 2])))
    
}