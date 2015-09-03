core <- function(mm, lam, sm, beta, siglist = NULL){
    
    if(!(require("nleqslv"))){
        
        stop("The package 'nleqslv' is required, type: install.packages('nleqslv')")
    }
    
    # get latent variable names
    latent.vars <- unique(mm[, 1])
    
    # get number of manifest variables per latent variable
    # lv.manifest <- table(mm[,1])
    
    # get number of manifest variables
    nmanifest <- nrow(mm)
    
    # initialize covariance matrix
    sigma <- matrix(0, nmanifest, nmanifest)
    colnames(sigma) <- mm[, 2]
    rownames(sigma) <- colnames(sigma)
    diag(sigma) <- 1
    
    
    # for each latent variable lv estimate the outer correlations of the manifest variables
    for(lv in latent.vars){

        # get manifest variables for the latent variable
        lv.manifest <- mm[, 2][which(mm == lv)]
        
        # get the lambda coefficients for the latent variable
        lv.lam <- lam[which(mm[,2] %in% lv.manifest)]
        
        if(length(lv.manifest) == 1){

            manifest.cor <- 1
            
        }else if(length(lv.manifest) == 2){
            
            # check if lam are equal
            if(lv.lam[1] != lv.lam[2]){
                
                stop("lambda coefficients have to be equal in case of only 2 manifest variables")
            }
            
            fun <- function(x) { 
                f <- numeric(length(x))
                f[1] <- (1+x[1])/sqrt(2+2*(x[1]))-lv.lam[1]
                f 
            } 
            
            startx <- 0
            
            manifest.cor <- nleqslv(startx, fun)[[1]] 
            
        }else if(length(lv.manifest) == 3){
            
            fun <- function(x) {
                f <- numeric(length(x))
                f[1] <-  (lv.lam[1]+x[1]*lv.lam[2]+x[2]*lv.lam[3])/sqrt(lv.lam[1]^2+lv.lam[2]^2+lv.lam[3]^2+2*(lv.lam[1]*lv.lam[2]*x[1]+lv.lam[1]*lv.lam[3]*x[2]+lv.lam[2]*lv.lam[3]*x[3]))-lv.lam[1]    
                f[2] <-  (lv.lam[2]+x[1]*lv.lam[1]+x[3]*lv.lam[3])/sqrt(lv.lam[1]^2+lv.lam[2]^2+lv.lam[3]^2+2*(lv.lam[1]*lv.lam[2]*x[1]+lv.lam[1]*lv.lam[3]*x[2]+lv.lam[2]*lv.lam[3]*x[3]))-lv.lam[2]    
                f[3] <-  (lv.lam[3]+x[2]*lv.lam[1]+x[3]*lv.lam[2])/sqrt(lv.lam[1]^2+lv.lam[2]^2+lv.lam[3]^2+2*(lv.lam[1]*lv.lam[2]*x[1]+lv.lam[1]*lv.lam[3]*x[2]+lv.lam[2]*lv.lam[3]*x[3]))-lv.lam[3]
                f
            }
            
            startx <- rep(0, 3)
            
            # get the correlations
            manifest.cor <- nleqslv(startx, fun)[[1]] 
            
        }else if(length(lv.manifest) == 4){
            
            fun <- function(x) { 
                f <- numeric(length(x))
                f[1] <-  (1+x[1]+x[2]+x[3])/sqrt(4+2*(x[1]+x[2]+x[3]+x[4]+coeffs[1]+coeffs[2]))-lv.lam[1]
                f[2] <-  (1+x[1]+x[4]+coeffs[1])/sqrt(4+2*(x[1]+x[2]+x[3]+x[4]+coeffs[1]+coeffs[2]))-lv.lam[2]  
                f[3] <-  (1+x[2]+x[4]+coeffs[2])/sqrt(4+2*(x[1]+x[2]+x[3]+x[4]+coeffs[1]+coeffs[2]))-lv.lam[3]
                f[4] <-  (1+x[3]+coeffs[1]+coeffs[2])/sqrt(4+2*(x[1]+x[2]+x[3]+x[4]+coeffs[1]+coeffs[2]))-lv.lam[4]
                f
            } 
            
            startx <- rep(0, 4)
            
            # get the correlations
            manifest.cor <- nleqslv(startx, fun)[[1]] 
            
        }else{
            
            if(is.null(siglist)){
                
                stop("siglist has to be supplied")
            }
            manifest.cor <- as.vector(siglist[[lv]][upper.tri(siglist[[lv]])])
            
        }
        
        # get the covariance submatrix of the manifest variables of lv
        lv.sig <- sigma[lv.manifest, lv.manifest]
        
        # input estimated correlations
        lv.sig[upper.tri(lv.sig)] <- manifest.cor
        lv.sig[lower.tri(lv.sig)] <- t(lv.sig[upper.tri(lv.sig)])
        
        # merge lv covariance matrix and full covariance matrix
        sigma[lv.manifest, lv.manifest] <- lv.sig
        
    }    
    
    # for each equation of the structural model estimate the implied cross correlations of the manifest variables
    for(i in 1:nrow(sm)){
        
        # get the manifest variables of the source latent variable
        source.manifest <- mm[, 2][which(mm %in% sm[i, 1])]
        # get the manifest variables of the target latent variable
        target.manifest <- mm[, 2][which(mm %in% sm[i, 2])]
        
        # get the lambda coefficients of the source latent variable
        source.lam <- lam[which(mm %in% sm[i, 1])]    
        # get the lambda coefficients of the target latent variable
        target.lam <- lam[which(mm %in% sm[i, 2])]
        
        # get the cross correlations submatrix of the manifest variables
        s.sig <- tcrossprod(source.lam, target.lam)*beta[i]
        
        # merge submatrix and full covariance matrix
        sigma[source.manifest, target.manifest] <- s.sig
        sigma[target.manifest, source.manifest] <- t(s.sig)
        
    }
    
    return(sigma)
  
}