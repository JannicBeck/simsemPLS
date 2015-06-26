library("MASS")

ol <- 1:100/100
cl <- rep(0.5, 100)

# n = number of replications
# ol = vector of outer correlations, of a covariance matrix
# cl = vector of cross correlations, of a covariance matrix
sim_struc <- function(n, ol, cl){
    
    beta <- rep(0, n)
    lambda <- rep(0, n)
    
    for(i in 1:n){
        
        sig <- matrix(ol[i], 4, 4)
        colnames(sig) <- c("x1", "x2", "x3", "x4")
        rownames(sig) <- colnames(sig)
        diag(sig) <- 1
        
        sig[3,1] <- cl[i]
        sig[1,3] <- cl[i]
        sig[4,1] <- cl[i]
        sig[1,4] <- cl[i]
        sig[3,2] <- cl[i]
        sig[2,3] <- cl[i]
        sig[4,2] <- cl[i]
        sig[2,4] <- cl[i]
        
        try(X <- mvrnorm(100, c(0, 0, 0, 0), sig, empirical = TRUE))
        
        x1 <- X[, 1]
        x2 <- X[, 2]
        x3 <- X[, 3]
        x4 <- X[, 4]
        
        beta[i] <- cor(x1+x2, x3+x4)
        lambda[i] <- cor(x1, x1+x2)

    }   
    
    result <- cbind(beta, lambda)
        
    return(result)
}

result <- sim_struc(100, ol, cl)

beta <- result[,"beta"]
lambda <- result[,"lambda"]

plot(cl, beta)
plot(ol, beta)

plot(cl, lambda)
plot(ol, lambda)

# obervations:  

# OUTER LOADINGS
#               lambda is almost exactly linear dependent on outer loadings
#               beta is independent from outer loadings if cross loadings = 1 or 0
#               beta is approx -x^0.5 dependent on outer loadings if cross loadings = 0.5

# CROSS LOADINGS
#               beta is linear dependent on cross loadings until the implied covariance to the summed variables,
#               after that its always 1 if outer loadings = 0 or 1
#               beta is independent from cross loadings if outer loadings = 0.5

#               If outer loadings = 1, beta is perfectly linear dependent on cross loadings
#               lambda is independent from cross loadings

# CROSS LOADINGS
# Over 0.5, covariance matrix not positive definite