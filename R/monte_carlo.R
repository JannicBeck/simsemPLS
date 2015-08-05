monte_carlo <- function(nmonte = 100, nobs = 1000, sigma, model, coeffs, FUNPLS = sempls, FUNSIM = mvrnorm, ...){
    
    if(!(is.function(FUNPLS))){
        
        stop("Parameter FUNPLS must be set to sempls or matrixpls.sempls")
    }
    
    if(!(is.function(FUNSIM))){
        
        stop("Parameter FUNSIM must be set to mvrnorm or rvnfast")
    }
    
    
    if(identical(FUNPLS, sempls)){
        
        if(!(require("semPLS"))){
            
            stop("The package 'semPLS' is required, type: install.packages('semPLS')")
        }
    }else if(identical(FUNPLS, matrixpls.sempls)){
        
        if(!(require("matrixpls"))){
            
            stop("The package 'matrixpls' is required, type: install.packages('matrixpls')")
        }    
    }
    
    if(identical(FUNSIM, mvrnorm)){
        
        if(!(require("MASS"))){
            
            stop("The package 'MASS' is required, type: install.packages('MASS')")
        }
    }else if(identical(FUNSIM, rmvn)){
        
        if(!(require("mvnfast"))){
            
            stop("The package 'mvnfast' is required, type: install.packages('mvnfast')")
        }    
    }
    
    
    eq <- c(get_equations(model)[[1]][, "lam"], get_equations(model)[[2]][, "beta"])
    
    t <- matrix(numeric(0), nmonte, length(eq))
    colnames(t) <- eq
    
    t0 <- coeffs
    
    for(i in 1:nmonte){
        
        # printing out the iteration
        treil <- paste(rep(" ", floor(log10(nmonte)) - floor(log10(i))), collapse="")
        ndel <- paste(rep("\b", floor(log10(nmonte)) + 1), collapse="")
        if(i==1) cat(paste(treil, i, sep=""))
        if(i!=nmonte) cat(paste(ndel, treil, i, sep=""))
        else cat(paste(ndel, i, " Done.\n", sep=""))
        
        # construct dataset with normally distributed variables mean 0 and variance 1
        X <- as.data.frame(FUNSIM(nobs, rep(0, ncol(sigma)), sigma, ...))
        colnames(X) <- colnames(sigma)

        try(estim.model <- FUNPLS(model, X, verbose = FALSE, ...), silent = TRUE)
        
        t[i, ] <- estim.model$coefficients$Estimate
    }
    
    result <- list(t0 = t0, t = t, nobs = nobs, nmonte = nmonte, model = model)
    
}
    
    