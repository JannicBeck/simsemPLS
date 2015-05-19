#' Core function 2, actual simulating is done here.
#' 
core_2 <- function(object, nmonte, nobs, scoeffs, mcoeffs, FUN, ...) { 
    
    # get number of equations
    neq <- length(c(object$strucmod[, 2], object$measuremod[, 2]))
    
    # initialize simulated coefficient matrix
    t <- matrix(numeric(0), nrow = nmonte, ncol = neq)
    
    # get equations
    equations <- get_equations(object)
    
    # set path attribute
    attr(t, "path") <- c(equations[[1]][, 1], equations[[2]][, 1])
    
    # set column names
    colnames(t) <- c(equations[[1]][, 2], equations[[2]][, 2]) 
    
    # initialize list of datasets
    data <- vector("list", nmonte)
    
    # from correlation matrix to vector
    mcoeffs <- mcoeffs[mcoeffs != 0]
    
    # get structural coefficients vector
    # old method
    scoeffs.t0 <- scoeffs * object$D
    scoeffs.t0 <- scoeffs.t0[scoeffs.t0 != 0]
    
    # new method not working
    # scoeffs.t0 <- scoeffs[scoeffs != 0]
    
    # TODO: Implement getcoeffs from covariance matrix
    # set actual coefficients to provided
    t0 <- c(mcoeffs, scoeffs.t0)
    
    # specify the means of the latent variables
    smeans <- rep(0, nrow(scoeffs))
    
    # specify the means of the latent variables
    mmeans <- rep(0, length(mcoeffs))
    
    sim.data <- matrix(numeric(), nobs, length(mcoeffs))
    
    colnames(sim.data) <- object$manifest
    
    times <- colSums(object$M)
    
    fnames <- rep(names(times), times)
    
    cat("Simulate: ")
    
    for(i in 1:nmonte){
        
        # printing out the iteration
        treil <- paste(rep(" ", floor(log10(nmonte)) - floor(log10(i))), collapse="")
        ndel <- paste(rep("\b", floor(log10(nmonte)) + 1), collapse="")
        if(i==1) cat(paste(treil, i, sep=""))
        if(i!=nmonte) cat(paste(ndel, treil, i, sep=""))
        else cat(paste(ndel, i, " Done.\n", sep=""))
        

        
        # randomly simulate the latent variable scores with given covariance matrix
        # If empirical = FALSE, the correlations will be approx.
        fscores <- mvrnorm(nobs, Sigma = scoeffs, mu = smeans, empirical = FALSE)
                
        for(j in seq_along(mcoeffs)){
            
            # population
            sim.data[, j] <- sim_data_pop(mcoeffs[j], nobs, fscores[, fnames[j]])
        }
        
        # estimate model with simulated data
        try(sim.model <- FUN(object, sim.data, verbose = FALSE, ...), silent = TRUE)
        
        # extract coefficients
        t[i, ] <- sim.model$coefficients[, 2]
        
        # save simulated data in list
        data[[i]] <- sim.data
        
    }
    
    # initialize and assign result list
    result <- list(t0 = t0, t = t, nobs = nobs, nmonte = nmonte, data = data, model = object)
    
    return(result)
}
