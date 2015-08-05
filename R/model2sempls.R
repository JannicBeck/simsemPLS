model2sempls <- function(nobs, sigma, sm, mm){
    
    library("MASS")
    library("semPLS")
    
    X <- as.data.frame(mvrnorm(nobs, rep(0, ncol(sigma)), sigma, empirical = TRUE))
    model <- plsm(X, sm, mm)
    result <- sempls(model, X)
    
    return(result)
}