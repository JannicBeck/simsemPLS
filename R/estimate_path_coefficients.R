estimate_path_coefficients <- function(sig, sm, mm){
    
    all correlations between manifest variables of latent variables
    
    for(1:nrow(sm)){
        
        
    }
    
    # get number of manifest variables per latent variable
    nmanifest <- table(mm[,1])
    
    # initialize result matrix of outer loadings
    result <- matrix(0, sum(nmanifest), length(latent.vars))
    colnames(result) <- latent.vars
    rownames(result) <- rownames(sig)
    
    # get lambda coefficients for each latent variable
    for(lv in latent.vars){
        
        # get manifest variables vor the latent variable
        manifest.vars <- mm[,2][which(mm == lv)]
        
        # create sub covariance matrix for the latent variable
        sub.sig <- sig[manifest.vars, manifest.vars]
        
        # get correlations
        outer.cor <- sub.sig[lower.tri(sub.sig)]
        
        # denominator
        denom <- sqrt(nmanifest[lv]+2*(sum(outer.cor)))
        
        # estimate lambda coefficients
        for(mv in manifest.vars){
            
            # nominator
            nom <- 1+sum(sub.sig[mv,][names(sub.sig[mv,])!=mv])
            
            result[mv, lv] <- nom/denom
        }
        
    }
    
    return(result)
    
    nom <- sum(sig)
    denom <- 
}