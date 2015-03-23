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
        
        # get paths
        attr(t, "path") <- object$coefficients[, 1]
        
        # get column names
        colnames(t) <- rownames(object$coefficients)
        
        # get plsm model
        model <- object$model
        
    }else{
        
        # object is of type plsm
        model <- object
        
        # set actual coefficients to 0
        t0 <- rep(0, neq)
    }
    
    # initialize list of datasets
    data <- vector("list", nmonte)
    
    for(i in 1:nmonte){
        
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
        sim.model <- sempls(model, sim.data, maxit = 100)
        
        # extract coefficients
        t[i, ] <- sim.model$coefficients[, 2]
        
    }
    
    # initialize and assign result list
    result <- list(t0 = t0, t = t, sresid = sresid, mresid = mresid, nmonte = nmonte, data = data, model = model)
    
    return(result)
}
