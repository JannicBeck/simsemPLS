


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
