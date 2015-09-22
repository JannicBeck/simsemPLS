# example for a function, which has to be implemented
# gives the correlations based on specified regression weights

# desired beta coefficients
beta <- c(0.268, 0.448, 0.171)

# only cor(y5,y4) = 0.433 known
# remaining cor set to 0 (assuming uncorrelated predictors)
sigma <- cbind(c(1, 0, 0, 0),
               c(0, 1, 0.433, 0),
               c(0, 0.433, 1, 0),
               c(0, 0, 0, 1))


colnames(sigma) <- c("y1", "y4", "y5", "y6")
rownames(sigma) <- colnames(sigma)

R <- sigma[-4, -4]
rxy <- beta%*%R

sigma <- cbind(c(1, 0, 0, 0.268),
               c(0, 1, 0.433, 0.522043),
               c(0, 0.433, 1, 0.364984),
               c(0.268, 0.522043, 0.364984, 1))

colnames(sigma) <- c("y1", "y4", "y5", "y6")
rownames(sigma) <- colnames(sigma)

rxy <- beta%*%R
beta <- beta%*%R%*%solve(R)
rxy%*%solve(R)


# check
library("MASS")
X <- mvrnorm(100, rep(0, ncol(sigma)), sigma, empirical = TRUE)
lm(X[, "y6"]~X[, "y1"]+X[, "y4"]+X[, "y5"]+0)