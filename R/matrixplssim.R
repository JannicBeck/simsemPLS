model <- "
! regressions 
A=~0.7*x1
A=~0.7*x2
A=~0.7*x3
B=~0.7*x4
B=~0.7*x5
B=~0.8*x6
B=~0.8*x7
C=~0.6*x8
C=~0.6*x9
C=~0.6*x10
C=~0.8*x11
C=~0.8*x12
D=~0.8*x13
D=~0.8*x14
D=~0.8*x15
D ~ 0.3*A
C ~ 0.1*B
C ~ 0.5*A
D ~ 0.3*C
! residuals, variances and covariances
A ~~ 1.0*A
B ~~ 1.0*B
C ~~ 0.71*C
D ~~ 0.725*D
B ~~ 0.3*A
x1 ~~ 0.51*x1
x2 ~~ 0.51*x2
x3 ~~ 0.51*x3
x4 ~~ 0.51*x4
x5 ~~ 0.51*x5
x6 ~~ 0.36*x6
x7 ~~ 0.36*x7
x8 ~~ 0.64*x8
x9 ~~ 0.64*x9
x10 ~~ 0.64*x10
x11 ~~ 0.36*x11
x12 ~~ 0.36*x12
x13 ~~ 0.36*x13
x14 ~~ 0.36*x14
x15 ~~ 0.36*x15"

# Because all items are here specified as having unit variance, 
# the error variances are set at the square root of one minus 
# the square of the respective loading.
sqrt(1)-0.8^2

library(matrixpls)
output <- matrixpls.sim(100, model, n = 10000, multicore = TRUE, completeRep = TRUE) 

mm <- cbind(c("y1", "y1", "y2", "y2", "y2", "y3", "y3"),            
            c("x1", "x2", "x3", "x4", "x5", "x6", "x7"))
colnames(mm) <- c("source", "target")

sm <- cbind(c("y2", "y3"),             
            c("y1", "y1"))
colnames(sm) <- c("source", "target")

sigma <- rbind(c(1.0000,0.6200,0.5022000,0.4752000,0.4752000,0.405,0.405),               
               c(0.6200,1.0000,0.5022000,0.4752000,0.4752000,0.405,0.405),               
               c(0.5022,0.5022,1.0000000,0.7470119,0.7470119,0.000,0.000),               
               c(0.4752,0.4752,0.7470119,1.0000000,0.6242442,0.000,0.000),               
               c(0.4752,0.4752,0.7470119,0.6242442,1.0000000,0.000,0.000),               
               c(0.4050,0.4050,0.0000000,0.0000000,0.0000000,1.000,0.620),               
               c(0.4050,0.4050,0.0000000,0.0000000,0.0000000,0.620,1.000))
colnames(sigma) <- mm[, 2]
rownames(sigma) <- colnames(sigma)


W <- rbind(c(0.5555556, 0, 0),
           c(0.5555556, 0, 0),
           c(0, 0.3853006, 0),
           c(0, 0.3645855, 0),
           c(0, 0.3645855, 0),
           c(0, 0, 0.5555556),
           c(0, 0, 0.5555556))

colnames(W) <- c("y1", "y2", "y3")
rownames(W) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")

W%*%sigma%*%W


X <- as.data.frame(mvrnorm(100, rep(0, ncol(sigma)), sigma, empirical = TRUE))
model <- plsm(X, sm, mm)

# works fine
estim.model.sempls <- sempls(model, X)

# Error in `colnames<-`(`*tmp*`, value = rownames(result$total_effects)) : length of 'dimnames' [2] not equal to array extent
estim.model.matrix <- matrixpls.sempls(model, X)
