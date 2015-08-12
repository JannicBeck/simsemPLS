sigma <- cbind(c(1, 0.1, 0.2, 0.3),
               c(0.1, 1, 0.4, 0.5),
               c(0.2, 0.4, 1, 0.6),
               c(0.3, 0.5, 0.6, 1))

colnames(sigma) <- c("x1", "x2", "x3", "x4")
rownames(sigma) <- colnames(sigma)

library("MASS")
X <- mvrnorm(100, rep(0, ncol(sigma)), sigma, empirical = TRUE)
colnames(X) <- colnames(sigma)
x1 <- X[, "x1"]
x2 <- X[, "x2"]
x3 <- X[, "x3"]
x4 <- X[, "x4"]

lm1 <- lm(x1 ~ x2+x3+0)
lm2 <- lm(x1 ~ x2+x3+x4+0)

beta1 <- c(0.02381, 0.19048)
beta2 <- c(-0.07234, 0.04255, 0.31064)

R1 <- sigma[c("x2","x3"),c("x2","x3")]
R2 <- sigma[c("x2","x3","x4"),c("x2","x3","x4")]

beta1%*%R1 # rxy1
beta2%*%R2 # rxy2

beta1%*%R1%*%solve(R1)
beta2%*%R2%*%solve(R2)

beta1
beta2

rxy1%*%solve(R1)
rxy2%*%solve(R2)

beta <- c(0.268, 0.488, 0.171)

sigma <- cbind(c(1, 0.110565, 0.04787465, 0),
               c(0.110565, 1, 0.433, 0),
               c(0.04787465, 0.433, 1, 0),
               c(0, 0, 0, 1))


colnames(sigma) <- c("y1", "y4", "y5", "y6")
rownames(sigma) <- colnames(sigma)

R <- sigma[-4, -4]
beta%*%R

sigma <- cbind(c(1, 0.110565, 0.04787465, 0.3301423),
               c(0.110565, 1, 0.433, 0.5916744),
               c(0.04787465, 0.433, 1, 0.3951344),
               c(0.3301423, 0.5916744, 0.3951344, 1))


colnames(sigma) <- c("y1", "y4", "y5", "y6")
rownames(sigma) <- colnames(sigma)
X <- mvrnorm(100, rep(0, ncol(sigma)), sigma, empirical = TRUE)


y1 <- X[, "y1"]
y4 <- X[, "y4"]
y5 <- X[, "y5"]
y6 <- X[, "y6"]

lm(y6~y1+y4+y5+0)
