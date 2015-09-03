library("MASS")

# set up covariance matrix of independent errors
sig <- matrix(0, 8, 8)
rownames(sig) <- c("y2", "e1", "e2", "e3", "e4", "e5", "e6", "d1")
colnames(sig) <- rownames(sig)
diag(sig) <- 1

# simulate random uncorrelated variables
rndm.vars <- mvrnorm(100, Sigma = sig, mu = c(0, 0, 0, 0, 0, 0, 0, 0), empirical = TRUE)

# split the simulated variables to the names in the equation for better readability
y2 <- rndm.vars[,1]
e1 <- rndm.vars[,2]
e2 <- rndm.vars[,3]
e3 <- rndm.vars[,4]
e4 <- rndm.vars[,5]
e5 <- rndm.vars[,6]
e6 <- rndm.vars[,7]
d1 <- rndm.vars[,8]

# set global delta
delta <- 0.6

# set path coefficients of the structural (inner) model
beta_1 <- 0.8

# linear equation of the inner model
y1 <- beta_1*y2+d1*delta

# set outer loadings (coefficients) of the measurement (outer) model
lam_i <- 0.8

## linear equations of the measurement (outer) model
# measurement model of y1
x1 <- y1*lam_i+e1*delta
x2 <- y1*lam_i+e2*delta
x3 <- y1*lam_i+e3*delta

# measurement model of y2
x4 <- y2*lam_i+e4*delta
x5 <- y2*lam_i+e5*delta
x6 <- y2*lam_i+e6*delta

# form a dataset from the manifest variables (indicators)
X <- cbind(x1, x2, x3, x4, x5, x6)
sim.data <- as.data.frame(X)

# get the covariance matrix of X
cor(X)

matrix2latex <- function(matr) {
    
    printmrow <- function(x) {
        
        cat(cat(x,sep=" & "),"\\\\ \n")
    }
    
    cat("\\begin{bmatrix}","\n")
    body <- apply(matr,1,printmrow)
    cat("\\end{bmatrix}")
}

matrix2latex(round(model1.sig, digits = 3))
