monte_carlo <- function(nmonte, nobs, FUN, empirical.sig, model){
    
    if(FUN == "semPLS"){
        
        if(!(require("semPLS"))){
            
            stop("The package 'semPLS' is required, type: install.packages(\"semPLS\")")
        }
    }else{
        
        if(!(require("matrixpls"))){
            
            stop("The package 'matrixpls' is required, type: install.packages(\"matrixpls\")")
        }    
    }
    
    if(!(require("MASS"))){
        
        stop("The package 'MASS' is required, type: install.packages(\"MASS\")")
    }
    
    result <- list(matrix(numeric(0), nmonte, ncol(empirical.sig)), matrix(numeric(0), nmonte, ncol(empirical.sig)))
    
    for(i in 1:nmonte){
        
        # construct dataset with normally distributed variables mean 0 and variance 1
        X <- mvrnorm(nobs, rep(0, ncol(empirical.sig)), empirical.sig, empirical = FALSE)
        
        # sample covariance matrix
        sample.sig <- cor(X)
        
        # retrieve outer loadings for sample sig
        sample.outer_loadings <- estimate_outer_loadings(sample.sig, model$measuremod)
        
        # specify dataset
        dataset <- as.data.frame(X)
        
        estim.model <- sempls(model, dataset, maxit = 100)
        
        pls.outer_loadings <- as.vector(estim.model$outer_loadings[estim.model$outer_loadings!=0])
        sample.outer_loadings <- as.vector(sample.outer_loadings[sample.outer_loadings!=0])
        
        result[[1]][i, ] <- pls.outer_loadings
        result[[2]][i, ] <- sample.outer_loadings
    }
    
    return(result)
}
    
    