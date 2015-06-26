sig <- matrix(0, 4, 4)
#sig <- matrix(0.42, 4, 4)
colnames(sig) <- c("x1", "x2", "x3", "x4")
rownames(sig) <- colnames(sig)
diag(sig) <- 1

# cross loadings
cl <- 0.5
# cl <- 0.343
# guess what? 0.8^3 mofu

sig[3,1] <- cl
sig[1,3] <- cl
sig[4,1] <- cl
sig[1,4] <- cl
sig[3,2] <- cl
sig[2,3] <- cl
sig[4,2] <- cl
sig[2,4] <- cl

library("MASS")
X <- mvrnorm(100, c(0, 0, 0, 0), sig, empirical = TRUE)
x1 <- X[, 1]
x2 <- X[, 2]
x3 <- X[, 3]
x4 <- X[, 4]

cor(x1+x2, x3+x4)

cor(x1, x1+x2)
cor(x2, x1+x2)
cor(x3, x3+x4)
cor(x4, x3+x4)


# Observation: cross loadings influences only the beta coefficient
#              outer loadings influence both beta and lambda coefficients
# Beware of positive definite!

# Lets try sempls!!
library("semPLS")

# specify dataset
dataset <- as.data.frame(X)

## specify models
# measurement (outer) model
mm <- cbind(c("y1", "y1", "y2", "y2"),
            c("x1", "x2", "x3", "x4"))

# structural (inner) model
sm <- cbind(c("y2"), c("y1"))


# create plsm object
model <- plsm(dataset, sm, mm)

estim.model <- sempls(model, dataset)

estim.model

# note: beta_1_2 <- cl/lam_2_1*lam_1_1
# cl <- lam_1_1*beta_1_2*lam_2_1

############################### such analytics wow

e1 <- rnorm(100)
e2 <- rnorm(100)
cor(0.8*(x3+x4)+e1,1.6*(x3+x4)+e1+e2)







beta_1_2 <- cor(x1+x2, x3+x4)
lam_1_1 <- cor(x3, x3+x4)
lam_1_2 <- cor(x4, x3+x4)
lam_2_1 <- cor(x1, x1+x2)
lam_2_2 <- cor(x2, x1+x2)

y2 <- x3 + x4
y1 <- x1 + x2

## without errors

y1_new <- beta_1_2*y2

x1 <- lam_2_1*y1
x2 <- lam_2_2*y1
x3 <- lam_1_1*y2
x4 <- lam_1_2*y2

x1_new <- lam_2_1*(y1_new)
x2_new <- lam_2_2*(y1_new)
x3_new <- lam_1_1*(x3+x4)
x4_new <- lam_1_2*(x3+x4)

cor(x1, y1)
cor(x2, y1)
cor(x3, y2)
cor(x4, y2)

cor(x1_new, y1_new)
cor(x2_new, y1_new)
cor(x3_new, y2)
cor(x4_new, y2)

## with errors
e_1 <- rnorm(100)
e_1_1 <- rnorm(100)
e_1_2 <- rnorm(100)
e_2_1 <- rnorm(100)
e_2_2 <- rnorm(100)

y1_new <- beta_1_2*y2+e_1

x1 <- lam_2_1*y1+e_2_1
x2 <- lam_2_2*y1+e_2_2
x3 <- lam_1_1*y2+e_1_1
x4 <- lam_1_2*y2+e_1_2

x1 <- lam_2_1*(y1_new)+e_2_1
x2 <- lam_2_2*(y1_new)+e_2_2
x3 <- lam_1_1*(x3+x4)+e_1_1
x4 <- lam_1_2*(x3+x4)+e_1_2

beta_1_2 <- cor(x1+x2, x3+x4)


# small
x1 <- rnorm(100)
x2 <- rnorm(100)
e1 <- rnorm(100)
e2 <- rnorm(100)

x1 <- 0.8*(x1+x2)+e1
x2 <- 0.8*(x1+x2)+e2


# some fancy testing
abc <- mvrnorm(100, c(0,0,0), rbind(c(1,0,0),c(0,1,0),c(0,0,1)), empirical = TRUE)
a <- abc[,1]
b <- abc[,2]
c <- abc[,3]
cor(a,b)
cor(a,c)
cor(b,c)
cor(a, a+b+c)
cor(b, a+b+c)
cor(c, a+b+c)
