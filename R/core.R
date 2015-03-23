#' Core function, actual simulating is done here.
#' 
core <- function(object, nmonte, nobs, latent, manifest, strucmod, measuremod, 
                 scoeffs, sresid, mcoeffs, mresid) { 
    
    # get number of equations
    neq <- length(c(strucmod[, 2], measuremod[, 2]))
    
    # initialize simulated coefficient matrix
    t <- matrix(numeric(0), nrow = nmonte, ncol = neq)
    
    if(is(object, "sempls")){
        
        # get actual coefficients
        t0 <- object$coefficients$Estimate  
        
        # set path attribute
        attr(t, "path") <- object$coefficients[, 1]
        
        # set column names
        colnames(t) <- rownames(object$coefficients)
        
        # get plsm model
        model <- object$model
        
    }else{
        
        # get equations
        equations <- plsm2sem(ecsi.plsm)
        
        # set path attribute
        attr(t, "path") <- c(equations[[1]][, 1], equations[[2]][, 1])
        
        # set column names
        colnames(t) <- c(equations[[1]][, 2], equations[[2]][, 2])
        
        # object is of type plsm
        model <- object
        
        # set actual coefficients to NA
        t0 <- NA
    }
    
    # initialize list of datasets
    data <- vector("list", nmonte)
    
    cat("Simulate: ")
    
    for(i in 1:nmonte){
        
        # printing out the iteration
        treil <- paste(rep(" ", floor(log10(nmonte)) - floor(log10(i))), collapse="")
        ndel <- paste(rep("\b", floor(log10(nmonte)) + 1), collapse="")
        if(i==1) cat(paste(treil, i, sep=""))
        if(i!=nmonte) cat(paste(ndel, treil, i, sep=""))
        else cat(paste(ndel, i, " Done.\n", sep=""))
        
        # solve structural model with simulated exogenous factor scores
        # given coefficients and residuals
        sim.fscores <- sim_fscores(latent, strucmod, nobs, scoeffs, sresid)
        
        # solve measurement model with simulated factor scores,
        # loadings and residuals
        sim.data <- sim_data(manifest, measuremod, sim.fscores, nobs, 
                             mcoeffs, mresid)
        
        # save simulated data in list
        data[[i]] <- sim.data
        
        # estimate model with simulated data
        sim.model <- sempls(model, sim.data, maxit = 1000, verbose = FALSE)
        
        # extract coefficients
        t[i, ] <- sim.model$coefficients[, 2]
        
    }
    
    # initialize and assign result list
    result <- list(t0 = t0, t = t, sresid = sresid, mresid = mresid, nmonte = nmonte, data = data, model = model)
    
    return(result)
}
