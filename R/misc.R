


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



# ---- Performance ----
# sempls and mass
system.time(model1 <- monte_carlo(nmonte, nobs, model1.sig, estim.model1$model, model1.coeffs))
# sempls and mvnfast
system.time(model1 <- monte_carlo(nmonte, nobs, model1.sig, estim.model1$model, model1.coeffs, sempls, rmvn))
# matrixpls and mass
system.time(model1 <- monte_carlo(nmonte, nobs, model1.sig, estim.model1$model, model1.coeffs, matrixpls.sempls))
# matrixpls and mvnfast
system.time(model1 <- monte_carlo(nmonte, nobs, model1.sig, estim.model1$model, model1.coeffs, matrixpls.sempls, rmvn))

# sempls and mass
system.time(model2 <- monte_carlo(nmonte, nobs, model2.sig, estim.model2$model, model2.coeffs))
# sempls and mvnfast
system.time(model2 <- monte_carlo(nmonte, nobs, model2.sig, estim.model2$model, model2.coeffs, sempls, rmvn))
# matrixpls and mass
system.time(model2 <- monte_carlo(nmonte, nobs, model2.sig, estim.model2$model, model2.coeffs, matrixpls.sempls))
# matrixpls and mvnfast
system.time(model2 <- monte_carlo(nmonte, nobs, model2.sig, estim.model2$model, model2.coeffs, matrixpls.sempls, rmvn))

# Call the R code profiler and give it an output file to hold results
Rprof("profiling_monte_carlo.out")
# Call the function to be profiled
model2 <- monte_carlo(nmonte, nobs, model2.sig, estim.model2$model, model2.coeffs, matrixpls.sempls, rmvn)
Rprof(NULL)
summaryRprof("exampleAgg.out")