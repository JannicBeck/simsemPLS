# ---- models ----

# Model 1
mm <- cbind(c("y1", "y1", "y1", "y2", "y2", "y2", "y3", "y3", "y3"),
            c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9"))
lam <- c(0.92, 0.88, 0.84, 0.95, 0.9, 0.85, 0.96, 0.93, 0.91)
colnames(mm) <- c("source", "target")

sm <- cbind(c("y2", "y3", "y3"), 
            c("y1", "y2", "y1"))
beta <- c(0.4, 0.3, 0.12)
colnames(sm) <- c("source", "target")

model1.sig <- core(mm, lam, sm, beta)
sm <- cbind(c("y2", "y3"), 
            c("y1", "y2"))

estim.model1 <- model2sempls(100, model1.sig, sm, mm)
model1.coeffs <- estim.model1$coefficients$Estimate


# Model 2
# ---- 2 var sample model ----
mm <- cbind(c("y1", "y1", "y2", "y2", "y2"),
            c("x1", "x2", "x3", "x4", "x5"))
lam <- c(0.8, 0.8, 0.7, 0.65, 0.6)
colnames(mm) <- c("source", "target")

sm <- cbind(c("y2"), c("y1"))
beta <- 0.36
colnames(sm) <- c("source", "target")

model2.sig <- core(mm, lam, sm, beta)
estim.model2 <- model2sempls(100, model2.sig, sm, mm)
model2.coeffs <- estim.model2$coefficients$Estimate

# Model 3
mm <- cbind(c("y1", "y1", "y2", "y2", "y2", "y3", "y3"),
            c("x1", "x2", "x3", "x4", "x5", "x6", "x7"))
lam <- c(0.9, 0.9, 0.93, 0.88, 0.88, 0.9, 0.9)
colnames(mm) <- c("source", "target")

sm <- cbind(c("y2", "y3"), 
            c("y1", "y1"))
beta <- c(0.6, 0.5)
colnames(sm) <- c("source", "target")

model3.sig <- core(mm, lam, sm, beta)

estim.model3 <- model2sempls(100, model3.sig, sm, mm)
model3.coeffs <- estim.model3$coefficients$Estimate


# cleaning up models
rm(list = setdiff(ls(), lsf.str()))

# ---- Simulating ----
nmonte <- 100
nobs <- 500

library("matrixpls")
library("mvnfast")
model1 <- monte_carlo(nmonte, nobs, model1.sig, estim.model1$model, model1.coeffs, matrixpls.sempls, rmvn)
model2 <- monte_carlo(nmonte, nobs, model2.sig, estim.model2$model, model2.coeffs, matrixpls.sempls, rmvn)
model3 <- monte_carlo(nmonte, nobs, model3.sig, estim.model3$model, model3.coeffs, sempls, rmvn)

# ---- Plotting ----
coeff_plot(model1)
coeff_plot(model2)
coeff_plot(model3)

parallelplot.simsemPLS(model1, subset = 1:ncol(model1$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsemPLS(model2, subset = 1:ncol(model2$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsemPLS(model3, subset = 1:ncol(model3$t), reflinesAt = c(-1, 0, 1))










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
