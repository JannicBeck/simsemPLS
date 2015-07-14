# ---- simple full model ----
empirical.sig <- rbind(c(1.0, 0.5, 0.2, 0.2, 0.2, 0.2, 0.2),
                       c(0.5, 1.0, 0.2, 0.2, 0.2, 0.2, 0.2),
                       c(0.2, 0.2, 1.0, 0.28, 0.2, 0.2, 0.2),
                       c(0.2, 0.2, 0.28, 1.0, 0.2, 0.2, 0.2),
                       c(0.2, 0.2, 0.2, 0.2, 1.0, 0.23125, 0.34375),
                       c(0.2, 0.2, 0.2, 0.2, 0.23125, 1.0, 0.45625),
                       c(0.2, 0.2, 0.2, 0.2, 0.34375, 0.45625, 1.0))

colnames(empirical.sig) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")
rownames(empirical.sig) <- colnames(empirical.sig)

mm <- cbind(c("y1", "y1", "y2", "y2", "y3", "y3", "y3"),
            c("x1", "x2", "x3", "x4", "x5", "x6", "x7"))

# ---- sample model ----
empirical.sig <- rbind(c(1.0, 0.5, 0.2, 0.2, 0.2),
                       c(0.5, 1.0, 0.2, 0.2, 0.2),
                       c(0.2, 0.2, 1.0, 0.23125, 0.34375),
                       c(0.2, 0.2, 0.23125, 1.0, 0.45625),
                       c(0.2, 0.2, 0.34375, 0.45625, 1.0))

colnames(empirical.sig) <- c("x1", "x2", "x3", "x4", "x5")
rownames(empirical.sig) <- colnames(empirical.sig)

mm <- cbind(c("y1", "y1", "y2", "y2", "y2"),
            c("x1", "x2", "x3", "x4", "x5"))

if(!(require("MASS"))){
    
    stop("The package 'MASS' is required, type: install.packages(\"MASS\")")
}

sm <- cbind(c("y2"), c("y1"))

# construct dataset with normally distributed variables mean 0 and variance 1
X <- mvrnorm(100, rep(0, ncol(empirical.sig)), empirical.sig, empirical = TRUE)

# sample covariance matrix
sample.sig <- cor(X)

# retrieve outer loadings for empirical sig
empirical.outer_loadings <- estimate_outer_loadings(empirical.sig, mm)

# retrieve outer loadings for sample sig
sample.outer_loadings <- estimate_outer_loadings(sample.sig, mm)

# retrieve path coefficients for empirical sig
# empirical.path_coefficients <- estimate_path_coefficients(empirical.sig, sm)

# retrieve path coefficients for sample sig
# sample.path_coefficients <- estimate_path_coefficients(sample.sig, sm)
    
# structural (inner) model
sm <- cbind(c("y2", "y3"), c("y1", "y1"))

library("semPLS")

nobs <- 1000

# construct dataset with normally distributed variables mean 0 and variance 1
X <- mvrnorm(nobs, rep(0, ncol(empirical.sig)), empirical.sig, empirical = TRUE)

dataset <- as.data.frame(X)

# create plsm object
model <- plsm(dataset, sm, mm)

estim.model <- sempls(model, dataset)

outer_loadings <- estim.model$outer_loadings[estim.model$outer_loadings!=0]

result <- monte_carlo(100, nobs, "semPLS", empirical.sig, model)

mean(result[[1]][,1])
mean(result[[2]][,1])
var(result[[1]][,1])
var(result[[2]][,1])

for(i in 1:ncol(result[[1]])){
    
    plot(result[[1]][,i])
    points(result[[2]][,i], col = "green")
    points(rep(outer_loadings[i], nobs), col = "red", type = "lm")    
}
