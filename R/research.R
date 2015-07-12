library("MASS")

# ---- 2 VAR MODEL ----
# set up covariance matrix
empirical.sig <- rbind(c(1, 0.30),
                        c(0.30, 1))

# set names
colnames(empirical.sig) <- c("x1", "x2")
rownames(empirical.sig) <- colnames(empirical.sig)

# ---- 3 VAR MODEL ----
# set up covariance matrix
empirical.sig <- rbind(c(1, -0.14, 0.04),
             c(-0.14, 1, 0.22),
             c(0.04, 0.22, 1))

# set names
colnames(empirical.sig) <- c("x1", "x2", "x3")
rownames(empirical.sig) <- colnames(empirical.sig)

# TODO change ncol(X)
nmanifest <- ncol(empirical.sig)

# set number of monte carlo repetitions
nmonte <- 10000

# intialize result lambda coefficient matrix
result <- matrix(numeric(), nmonte, nmanifest)

for(i in 1:nmonte){
    
    # construct dataset
    X <- mvrnorm(100, rep(0, ncol(empirical.sig)), empirical.sig, empirical = FALSE)
    
    # sample covariance matrix
    sample.sig <- cor(X)
    
    # TODO adjust outer.cor retrieval for full model
    outer.cor <- sample.sig[lower.tri(sample.sig)]
    
    # denominator
    denom <- sqrt(nmanifest+2*(sum(outer.cor)))
    
    # estimate lambda coefficients
    for(j in 1:nmanifest){
        
        result[i, j] <- (1+sum(sample.sig[, j][sample.sig[, j]!=1]))/denom
    }
    
}




# ---- simple full model ----
empirical.sig <- rbind(c(1, 0.5, 0.2, 0.2),
             c(0.5, 1, 0.2, 0.2),
             c(0.2, 0.2, 1, 0.6),
             c(0.2, 0.2, 0.6, 1))

empirical.sig <- rbind(c(1.0, 0.5, 0.2, 0.2, 0.2, 0.2, 0.2),
                       c(0.5, 1.0, 0.2, 0.2, 0.2, 0.2, 0.2),
                       c(0.2, 0.2, 1.0, 0.28, 0.2, 0.2, 0.2),
                       c(0.2, 0.2, 0.28, 1.0, 0.2, 0.2, 0.2),
                       c(0.2, 0.2, 0.2, 0.2, 1.0, 0.23125, 0.34375),
                       c(0.2, 0.2, 0.2, 0.2, 0.23125, 1.0, 0.45625),
                       c(0.2, 0.2, 0.2, 0.2, 0.34375, 0.45625, 1.0))

colnames(empirical.sig) <- c("x1", "x2", "x3", "x4")
colnames(empirical.sig) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")
rownames(empirical.sig) <- colnames(empirical.sig)

mm <- cbind(c("y1", "y1", "y2", "y2"),
            c("x1", "x2", "x3", "x4"))

mm <- cbind(c("y1", "y1", "y2", "y2", "y3", "y3", "y3"),
            c("x1", "x2", "x3", "x4", "x5", "x6", "x7"))

sm <- cbind(c("y2", "y3"), c("y1", "y1"))

# get latent variable names
latent.vars <- unique(mm[, 1])

# get number of manifest variables per latent variable
nmanifest <- table(mm[,1])

# construct dataset with normally distributed variables mean 0 and variance 1
X <- mvrnorm(100, rep(0, ncol(empirical.sig)), empirical.sig, empirical = TRUE)

# sample covariance matrix
sample.sig <- cor(X)

lam <- rep(0, ncol(empirical.sig))

k <- 1

# get lambda coefficients for each latent variable
for(lv in latent.vars){
    
    # get manifest variables vor the latent variable
    manifest.vars <- mm[,2][which(mm == lv)]
    
    # get outer correlations TODO shorten that
    outer.cor <- sample.sig[manifest.vars, manifest.vars][lower.tri(sample.sig[manifest.vars, manifest.vars])]
    
    # denominator
    denom <- sqrt(nmanifest[lv]+2*(sum(outer.cor)))
    
    # estimate lambda coefficients
    for(mv in manifest.vars){
        
        # nominator
        nom <- (1+sum(sample.sig[mv,][manifest.vars][names(sample.sig[mv,][manifest.vars])!=mv]))
        
        lam[k] <- nom/denom
        
        k <- k+1
    }
    
}
    
# get latent variables
sm[,2][which(sm == latent.var)]
    
# structural (inner) model
sm <- cbind(c("y2"), c("y1"))

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



# ---- retrieve lambda coefficients ----
library("nleqslv")

x <- cor(x1,x2)
y <- cor(x1,x3)
z <- cor(x2,x3)

fun <- function(x) { 
    f <- numeric(length(x))         			            # read as:
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





# ---- weird shit ----

x <- cor(x1,x2)

x <- 1:100/100
plot(x,(1+x)/sqrt(2+2*(x)))

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
