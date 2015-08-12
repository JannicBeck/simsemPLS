# ---- models ----

# model1
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


# model 2
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

# model 3
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

# model 4
mm <- cbind(c("y1", "y1", 
              "y2", "y2", 
              "y3", "y3", 
              "y4", "y4", "y4",
              "y5", "y5", "y5",
              "y6", "y6", "y6"),
            c("x1", "x2", 
              "x3", "x4", 
              "x5", "x6", 
              "x7", "x8", "x9", 
              "x10", "x11", "x12", 
              "x13", "x14", "x15"))
lam <- c(0.917, 0.917,
         0.85, 0.85,
         0.9, 0.9,
         0.825, 0.825, 0.898,
         0.802, 0.805, 0.858,
         0.806, 0.913, 0.84)
colnames(mm) <- c("source", "target")

#sm <- cbind(c("y1", "y1", "y2", "y3", "y4", "y5", "y5"), 
#            c("y3", "y6", "y3", "y4","y6", "y4", "y6"))
#beta <- c(0.315, 0.268, 0.5, 0.351, 0.448, 0.433, 0.171)

sm <- cbind(c("y1", "y2", "y1", "y3", "y5", "y1", "y4", "y5"), 
            c("y3", "y3", "y4", "y4","y4", "y6", "y6", "y6"))
beta <- c(0.315, 0.5, 0.206, 0.351, 0.433, 0.268, 0.448, 0.171)

colnames(sm) <- c("source", "target")

model4.sig <- core(mm, lam, sm, beta)

estim.model4 <- model2sempls(100, model4.sig, sm, mm)
model4.coeffs <- estim.model4$coefficients$Estimate
model4.coeffs <- c(0.917, 0.917,
                   0.85, 0.85,
                   0.9, 0.9,
                   0.802, 0.805, 0.858,
                   0.825, 0.825, 0.898,
                   0.806, 0.913, 0.84,
                   0.315, 0.5, 0.351, 0.433, 0.268, 0.171, 0.448)


# cleaning up models
rm(list = setdiff(ls(), lsf.str()))

# ---- Simulating ----
nmonte <- 1000
nobs <- 10000

library("matrixpls")
library("mvnfast")
library("MASS")
model1 <- monte_carlo(nmonte, nobs, model1.sig, estim.model1$model, model1.coeffs, matrixpls.sempls, rmvn)
model2 <- monte_carlo(nmonte, nobs, model2.sig, estim.model2$model, model2.coeffs, matrixpls.sempls, rmvn)
model3 <- monte_carlo(nmonte, nobs, model3.sig, estim.model3$model, model3.coeffs, sempls, rmvn)
model4 <- monte_carlo(nmonte, nobs, model4.sig, estim.model4$model, model4.coeffs, sempls, rmvn)

# ---- Plotting ----
coeff_plot(model1)
coeff_plot(model2)
coeff_plot(model3)
coeff_plot(model4)

parallelplot_simsempls(model1, subset = 1:ncol(model1$t), reflinesAt = c(-1, 0, 1))
parallelplot_simsempls(model2, subset = 1:ncol(model2$t), reflinesAt = c(-1, 0, 1))
parallelplot_simsempls(model3, subset = 1:ncol(model3$t), reflinesAt = c(-1, 0, 1))
parallelplot_simsempls(model4, subset = 1:ncol(model4$t), reflinesAt = c(-1, 0, 1))

dist_plot(model1)
dist_plot(model2)
dist_plot(model3)
dist_plot(model4)






