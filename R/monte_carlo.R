monte_carlo <- function(nmonte, nobs, FUN, empirical.sig, model, coefficients){
    
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
    
    eq <- c(get_equations(model)[[1]][, "lam"], get_equations(model)[[2]][, "beta"])
    
    neq <- sum(c(as.numeric(model$M),as.numeric(model$D)))
    
    t <- matrix(numeric(0), nmonte, neq)
    
    t0 <- coefficients
    
    colnames(t) <- eq
    
    for(i in 1:nmonte){
        
        # printing out the iteration
        treil <- paste(rep(" ", floor(log10(nmonte)) - floor(log10(i))), collapse="")
        ndel <- paste(rep("\b", floor(log10(nmonte)) + 1), collapse="")
        if(i==1) cat(paste(treil, i, sep=""))
        if(i!=nmonte) cat(paste(ndel, treil, i, sep=""))
        else cat(paste(ndel, i, " Done.\n", sep=""))
        
        # construct dataset with normally distributed variables mean 0 and variance 1
        X <- mvrnorm(nobs, rep(0, ncol(empirical.sig)), empirical.sig, empirical = FALSE)
        
        # sample covariance matrix
        sample.sig <- cor(X)
        
        # specify dataset
        dataset <- as.data.frame(X)
        
        try(estim.model <- sempls(model, dataset, maxit = 100, verbose = FALSE), silent = TRUE)
        
        t[i, ] <- estim.model$coefficients$Estimate
    }
    
    result <- list(t0 = t0, t = t, nobs = nobs, nmonte = nmonte, model = model)
    
}
    
    