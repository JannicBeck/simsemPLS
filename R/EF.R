result <- 1:1000
result2 <- 1:1000
result3 <- 1:1000

s <- 1
n <- 100

for(i in 1:1000){
    
    a <- rnorm(n)
    e <- rnorm(n)
    b <- a*0.8 + s*e
    e <- rnorm(n)
    c <- a*0.8 + s*e
    
    result[i] <- cor(a, b)
    result2[i] <- cor(a, c)
    result3[i] <- cor(b, c)
}

plot(result)
plot(result2)
plot(result3)

# Beobachtungen: Korrelation zwischen a und b impliziert Korrelation zwischen b und c.
#                Koeffizient von 0.8 erzeugt keine Korrelationen von 0.8.
#                Sigma Term und Koeffizient beeinflussen Korrelation.
#                Stichprobengröße beeinflusst Varianz der Korrelationen.

result <- 1:1000
result2 <- 1:1000
result3 <- 1:1000

library("MASS")

sigma <- rbind(c(1,0.5),c(0.5,1))
n <- 1000

for(i in 1:1000){
    
    ab <- mvrnorm(n, c(0, 0), sigma, empirical = TRUE)
    a <- ab[, 1]
    b <- ab[, 2]
    
    rho   <- 0.5                  # desired correlation = cos(angle)
    theta <- acos(rho)             # corresponding angle
    x1    <- a       # fixed given data
    x2    <- rnorm(n, 2, 0.5)      # new random data
    X     <- cbind(x1, x2)         # matrix
    Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)
    
    Id   <- diag(n)                               # identity matrix
    Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
    P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
    x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
    Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
    Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1
    
    x <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
    cor(x1, x)   
    
    c <- x
    
    result[i] <- cor(a, b)
    result2[i] <- cor(a, c)
    result3[i] <- cor(b, c)
}

plot(result)
plot(result2)
plot(result3)

# Beobachtungen: Korrelation zwischen a und b, sowie a und c Konstant.
#                Korrelation zwischen b und c variiert.
#                Stichprobengröße beeinflusst Varianz der Korrelationen.

sigma <- rbind(c(1,0),c(0,1))
library(MASS)
xe <- mvrnorm(100000, c(0, 0), sigma, empirical = TRUE)
x <- xe[,1]
e <- xe[,2]

a <- 0.3
sig <- 0.5
y <- a*x+e

cor(x,y)

mean(x*y)
mean(a*x^2+e*x*sig)
a*mean(x^2)+mean(e*x)

mean(y*x) - mean(y)*mean(x)
mean(y*x) - a*mean(x)*mean(x)
a*var(x)

a/(sd(x)*sd(y))
(a*var(x))/(sd(x)*sqrt(a^2*var(x)+var(x)^2))

# Beobachtungen: E[e*x] beeinflusst Kovarianz.
#                E[y]*E[x] beeinflusst Kovarianz.
#                Sigma beeinflusst Standardabweichung von y und somit auch Korrelation.
#                Mit Steigender Stichprobengröße scheinen Gleichungen gegen a zu konvergieren.
  

library("MASS")
sigma <- rbind(c(1,0,0),c(0,1,0),c(0,0,1))
f1e1e2 <- mvrnorm(100, c(0, 0, 0), sigma, empirical = TRUE)

f1 <- f1e1e2[,1]
e1 <- f1e1e2[,2]
e2 <- f1e1e2[,3]

lam1 <- 0.8
lam2 <- 0.4

x1 <- lam1*f1+e1*0.6
x2 <- lam2*f1+e2*0.9165

cor(x1,f1)
cor(x2,f1)

cor(x1, x2)

x1 <- lam1*f1
x2 <- lam2*f1
mean(x1*x2)
mean((lam1*f1)*(lam2*f1))
mean(lam1*f1*lam2*f1)
mean(lam1*lam2*f1^2)
lam1*lam2*mean(f1^2)

# not quite right
(lam1*lam2)/(sd(x2)*sqrt((lam1*lam2)^2*var(x2)+var(x2)^2))

(lam1*var(f1))/(sd(f1)*sqrt(lam1^2*var(f1)+var(f1)^2))

# Beobachtungen: Bei gleichen lambda Koeffizienten und unkorrelierten Residuen stimmen Korrelationen exakt überein.
#                Korrelation zwischen x1 und x2 ist exakt.
#                Aus cor(x1, f1) = 0.5 und cor(x2, f1) = 0.5 folgt: cor(x1, x2) = 0.5^2 = 0.25.
#                Frage: Wie muss sigma gewählt werden, dass die linearkombination exakt 0.5 Korrelation erzeugt?
#                Bei lam = 0.8 muss sigma exakt 0.6 betragen.



















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
