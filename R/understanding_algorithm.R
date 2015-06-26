# understanding the pls-algorithm

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

# Lets try sempls!!
library("semPLS")

# specify dataset
dataset <- as.data.frame(X)

## specify models
# measurement (outer) model
mm <- cbind(c("y1", "y1", "y1", "y2", "y2", "y2"),
            c("x1", "x2", "x3", "x4", "x5", "x6"))

# structural (inner) model
sm <- cbind(c("y2"), c("y1"))


# create plsm object
model <- plsm(dataset, sm, mm)

estim.model <- sempls(model, dataset)

# manually do the pls algorithm:

# assign weights
w_1_1 <- 1
w_1_2 <- 1
w_1_3 <- 1
w_2_1 <- 1
w_2_2 <- 1
w_2_3 <- 1

# ----- Step 1.1 Outer Approximation
y1 <- x1*1+x2*1+x3*1
y2 <- x4*1+x5*1+x6*1
# Normalization
y1 <- scale(y1)
y2 <- scale(y2)

# ----- Step 1.2 Inner Path calculation
beta_1 <- as.numeric(cor(y1, y2))

# ----- Step 1.3 Inner estimation
y2n <- y1 * beta_1
y1n <- y2 * beta_1

# reassign and normalize
y1 <- scale(y1n)
y2 <- scale(y2n)

# ----- Step 1.4 Update of outer weights
w_1_1 <- as.numeric(cor(y1, x1))
w_1_2 <- as.numeric(cor(y1, x2))
w_1_3 <- as.numeric(cor(y1, x3))
w_2_1 <- as.numeric(cor(y2, x4))
w_2_2 <- as.numeric(cor(y2, x5))
w_2_3 <- as.numeric(cor(y2, x6))

# normalize
stdev1 <- sd(x1*w_1_1+x2*w_1_2+x3*w_1_3)
stdev2 <- sd(x4*w_2_1+x5*w_2_2+x6*w_2_3)

w_1_1 <- w_1_1/stdev1
w_1_2 <- w_1_2/stdev1
w_1_3 <- w_1_3/stdev1
w_2_1 <- w_2_1/stdev2
w_2_2 <- w_2_2/stdev2
w_2_3 <- w_2_3/stdev2

# lets pretend different weights
w_1_1 <- 0.1
w_1_2 <- 0.1
w_1_3 <- 0.1
w_2_1 <- 0.1
w_2_2 <- 0.1
w_2_3 <- 0.1


# check weights
estim.model$outer_weights

# ----- Step 1.1 Outer Approximation
y1 <- x1*w_1_1+x2*w_1_2+x3*w_1_3
y2 <- x4*w_2_1+x5*w_2_2+x6*w_2_3

# note: weights have no effect on loadings, as I could only do y1=x1+x2+x3

# Normalization
y1 <- scale(y1)
y2 <- scale(y2)

# get loadings
lam_1_1 <- cor(x1, y1)
lam_1_2 <- cor(x2, y1)
lam_1_3 <- cor(x3, y1)
lam_2_1 <- cor(x4, y2)
lam_2_2 <- cor(x5, y2)
lam_2_3 <- cor(x6, y2)

# lets get to the equations
lm(y1~y2+0)
lm(x1~y1*0)

# MODIFIED
# manually do the pls algorithm:
# ----- Step 1.1 Outer Approximation
y1 <- x1+x2+x3
y2 <- x4+x5+x6

# ----- Step 1.2 Inner Path calculation
beta_1 <- as.numeric(cor(y1, y2))

# get loadings
lam_1_1 <- cor(x1, y1)
lam_1_2 <- cor(x2, y1)
lam_1_3 <- cor(x3, y1)
lam_2_1 <- cor(x4, y2)
lam_2_2 <- cor(x5, y2)
lam_2_3 <- cor(x6, y2)

# now lets get to the equations
lm(y1~y2+0)

# awkward why scaling neccessary?
lm(x1~scale(y1)+0)
