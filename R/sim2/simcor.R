#' Simulate random variable with correlation rho on existing variable fscore.
#' 
sim_cor <- function(rho, nobs, fscore){
    
    if(rho == 1){
        
        result <- fscore
    }else{
        
        theta <- acos(rho)             # corresponding angle
        x2    <- rnorm(nobs, 0, 1)      # new random data
        X     <- cbind(fscore, x2)         # matrix
        Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)
        
        Id   <- diag(nobs)                               # identity matrix
        Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
        P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by fscore
        x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to Xctr
        Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
        Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1
        
        result <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
    }        
    
    return(result)
}