#' Core function, actual simulating is done here.
#' 
core <- function(model, nmonte, nobs, latent, manifest, strucmod, measuremod, 
                 scoeffs, sresid, mcoeffs, mresid) { 
    
    # initialize result list
    result <- vector("list", nmonte)
    
    for(i in 1:nmonte){
        
        # solve structural model with simulated exogenous factor scores
        # given coefficients and residuals
        sim.fscores <- sim_fscores(latent, strucmod, nobs, scoeffs, sresid)
        
        # solve measurement model with simulated factor scores,
        # loadings and residuals
        sim.data <- sim_data(manifest, measuremod, sim.fscores, nobs, 
                             mcoeffs, mresid)
        
        # write sempls objects in result list
        result[[i]] <- sempls(model, sim.data, maxit = 100)
        
        # TODO: change residual lists to matrices
        result[[i]]$sresiduals <- sresid
        result[[i]]$mresiduals <- mresid
    }
    
    return(result)
}
