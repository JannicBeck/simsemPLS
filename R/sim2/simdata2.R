# returns a data frame of two variables which correlate with a population correlation of rho
# If desired, one of both variables can be fixed to an existing variable by specifying x
sim_data_pop <- function(rho, nobs, fscore) {
    
    if(rho == 1){
        
        result <- fscore
    }else{
        
        C <- matrix(rho, nrow = 2, ncol = 2)
        diag(C) <- 1
        
        C <- chol(C)
        
        X1 <- rnorm(nobs)
        X <- cbind(fscore, X1)
        
        # induce correlation (does not change X1)
        df <- X %*% C
        
        result <- df[, 2]
        
    }
    
    return(result)
    
}    