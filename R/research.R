#### 2 VAR MODEL ####

library("MASS")

sig <- rbind(c(1, 0.28),
             c(0.28, 1))

colnames(sig) <- c("x1", "x2")
rownames(sig) <- colnames(sig)

X <- mvrnorm(100, rep(0, ncol(sig)), sig, empirical = TRUE)

x1 <- X[, 1]
x2 <- X[, 2]

y <- x1+x2

# general formula

# TODO adjust outer.cor retrieval for full model
outer.cor <- sig[lower.tri(sig)]

# TODO change ncol(X)
nmanifest <- ncol(X)

# not sure for what
choose(nmanifest, 2)

denom <- sqrt(nmanifest+2*(sum(outer.cor)))

lam <- numeric(nmanifest)

for(i in 1:nmanifest){
    
    lam[i] <- (1+sum(sig[, i][sig[, i]!=1]))/denom
}

x <- cor(x1,x2)

x <- 1:100/100
plot(x,(1+x)/sqrt(2+2*(x)))

##### 3 VAR MODEL #####
library("nleqslv")

x <- cor(x1,x2)
y <- cor(x1,x3)
z <- cor(x2,x3)

library("nleqslv")

fun <- function(x) { 
    f <- numeric(length(x))     				            # read as:
    f[1] <-  (1+x[1]+x[2])/sqrt(3+2*(x[1]+x[2]+x[3]))-0.8	# 0 = 2z - 2y + 3x + 4
    f[2] <-  (1+x[1]+x[3])/sqrt(3+2*(x[1]+x[2]+x[3]))-0.75	# 0 = 3z + 4y - 3x + 3
    f[3] <-  (1+x[2]+x[3])/sqrt(3+2*(x[1]+x[2]+x[3]))-0.7	# 0 = -z + 2y + 5x + 1
    f 
} 
startx <- c(0,0,0) # start the answer search here
answers <- as.data.frame(nleqslv(startx,fun))  # answers["x"] = x,y,z are the solutions closest to startx if there are multiple solutions

library("MASS")

sig <- rbind(c(1, -0.14, 0.04),
             c(-0.14, 1, 0.22),
             c(0.04, 0.22, 1))

colnames(sig) <- c("x1", "x2", "x3")
rownames(sig) <- colnames(sig)

X <- mvrnorm(100, rep(0, ncol(sig)), sig, empirical = TRUE)

x1 <- X[, 1]
x2 <- X[, 2]
x3 <- X[, 3]

y <- x1+x2+x3

(1+cor(x1,x2)+cor(x1,x3))/(sqrt(var(x1))*sqrt(var(x1)+var(x2)+var(x3)+2*cor(x1,x2)+2*cor(x1,x3)+2*cor(x2,x3)))

denom <- sqrt(choose(ncol(X), 2)+2*(cor(x1,x2)+cor(x1,x3)+cor(x2,x3)))
(1+cor(x1,x2)+cor(x1,x3))/denom
cor(x1, y)

(1+cor(x2,x1)+cor(x2,x3))/denom
cor(x2, y)

(1+cor(x3,x1)+cor(x3,x2))/denom
cor(x3, y)

# general formula

# TODO adjust outer.cor retrieval for full model
outer.cor <- sig[lower.tri(sig)]

# TODO change ncol(X)
nmanifest <- ncol(X)

denom <- sqrt(choose(nmanifest, 2)+2*(sum(outer.cor)))

# should be
# denom <- sqrt(nmanifest+2*(sum(outer.cor)))

lam <- numeric(nmanifest)

for(i in 1:nmanifest){
    
    lam[i] <- (1+sum(sig[, i][sig[, i]!=1]))/denom
}

x <- cor(x1,x2)
y <- cor(x1,x3)
z <- cor(x2,x3)

0.5 = (1+x+y)/sqrt(3+2*(x+y+z))
0.6 = (1+x+z)/sqrt(3+2*(x+y+z))
0.7 = (1+y+z)/sqrt(3+2*(x+y+z))

##################### simple full model

library("nleqslv")

fun <- function(x) { 
    f <- numeric(length(x))         			            # read as:
    f[1] <-  (1+x[1])/sqrt(2+2*x[1])-0.8	# 0 = 2z - 2y + 3x + 4
    f 
} 
startx <- c(0) # start the answer search here
answers <- as.data.frame(nleqslv(startx,fun))  # answers["x"] = x,y,z are the solutions closest to startx if there are multiple solutions


sig <- rbind(c(1, 0.28, 0.5, 0.5),
             c(0.28, 1, 0.5, 0.5),
             c(0.5, 0.5, 1, 0.28),
             c(0.5, 0.5, 0.28, 1))

colnames(sig) <- c("x1", "x2", "x3", "x4")
rownames(sig) <- colnames(sig)

X <- mvrnorm(100, c(0, 0, 0, 0), sig, empirical = TRUE)

x1 <- X[, 1]
x2 <- X[, 2]
x3 <- X[, 3]
x4 <- X[, 4]

y1 <- x1+x2
y2 <- x3+x4

cor(x1, y1)
cor(x2, y1)
cor(x3, y2)
cor(x4, y2)

cor(x1, y2)
cor(x2, y2)
cor(x3, y1)
cor(x4, y1)

cor(y1,y2)
cor(x1+x2,x3+x4)

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

######################################### weird shit
x <- rnorm(100, 0, 0.4)
x <- 1:100/100
lam <- (1+x)/(sqrt(2+2*x))
plot(x, lam)

(1+x)/(sqrt(2+2*x))

x <- rnorm(100, 0, 0.2)
y <- rnorm(100, 0, 0.2)
x <- sort(x)
y <- sort(y)
z <- (1+x+y)/(sqrt(3+2*x+2*y+2*0))

library("akima")
library("rgl")
x1 <- rnorm(100, 0, 0.3)
x2 <- rnorm(100, 0, 0.3)
x3 <- 0.5
x1 <- sort(x1)
x2 <- sort(x2)

x1 <- 0.5
x2 <- 0.5
(1+x1+x2)/(sqrt(3+2*x1+2*x2+2*x3))

zfun <- function(x1,x2) {(1+x1+x2)/(sqrt(3+2*x1+2*x2+2*x3))}
lam    <- outer(x1, x2, FUN="zfun")

persp(x1, x2, lam, xlim = range(x1),theta = 10, ylim = range(x2), zlim = range(lam), col = "lightblue", ticktype = "detailed")



##################### full model

library("MASS")

sig <- rbind(c(1, 0.45625, 0.34375, 0.5, 0.5),
             c(0.45625, 1, 0.23125, 0.5, 0.5),
             c(0.34375, 0.23125, 1, 0.5, 0.5),
             c(0.5, 0.5, 0.5, 1, 0.28),
             c(0.5, 0.5, 0.5, 0.28, 1))

sig

colnames(sig) <- c("x1","x2","x3","x4","x5")
rownames(sig) <- colnames(sig)

X <- mvrnorm(100, c(0, 0, 0, 0, 0), sig, empirical = TRUE)

x1 <- X[, 1]
x2 <- X[, 2]
x3 <- X[, 3]
x4 <- X[, 4]
x5 <- X[, 5]

y1 <- x1+x2+x3
y2 <- x4+x5

# outer loadings
cor(x1,y1)
cor(x2,y1)
cor(x3,y1)
cor(x4,y2)
cor(x5,y2)

# cross loadings
cor(x1,y2)
cor(x2,y2)
cor(x3,y2)
cor(x4,y1)
cor(x5,y1)

# Lets try sempls!!
library("semPLS")

# specify dataset
dataset <- as.data.frame(X)

## specify models
# measurement (outer) model
mm <- cbind(c("y1", "y1", "y1", "y2", "y2"),
            c("x1", "x2", "x3", "x4", "x5"))

# structural (inner) model
sm <- cbind(c("y2"), c("y1"))


# create plsm object
model <- plsm(dataset, sm, mm)

estim.model <- sempls(model, dataset)

estim.model
estim.model.empiric

