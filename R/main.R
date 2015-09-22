# ---- models ----

# critical model
# model sig
mm <- cbind(c("y1", "y1", "y2", "y2", "y2"),
            c("x11", "x12", "x21", "x22", "x23"))
lam <- c(0.8, 0.8, 0.7, 0.75, 0.8)
colnames(mm) <- c("source", "target")

sm <- cbind(c("y1"), c("y2"))
beta <- 0.2
colnames(sm) <- c("source", "target")

modelc.sig <- core(mm, lam, sm, beta)
estim.modelc <- model2sempls(100, modelc.sig, sm, mm)
modelc.coeffs <- estim.modelc$coefficients$Estimate

# model 1
mm <- cbind(c("y1", "y1", "y1", "y2", "y2", "y2"),
            c("x11", "x12", "x13", "x21", "x22", "x23"))
lam <- c(0.6, 0.7, 0.8, 0.6, 0.7, 0.8)
colnames(mm) <- c("source", "target")

sm <- cbind(c("y1"), c("y2"))
beta <- 0.3
colnames(sm) <- c("source", "target")

model1.sig <- core(mm, lam, sm, beta)
estim.model1 <- model2sempls(100, model1.sig, sm, mm)
model1.coeffs <- estim.model1$coefficients$Estimate

library("Rgraphviz")
pathDiagram(estim.model1, file="estim_model1", edge.labels = "both", output.type = "graphics", digits = 6)


# model 1b
mm <- cbind(c("y1", "y1", "y1", "y2", "y2", "y2"),
            c("x11", "x12", "x13", "x21", "x22", "x23"))
lam <- c(0.6, 0.7, 0.8, 0.6, 0.7, 0.8)
colnames(mm) <- c("source", "target")

sm <- cbind(c("y1"), c("y2"))
beta <- 0.000000001
colnames(sm) <- c("source", "target")

model1b.sig <- core(mm, lam, sm, beta)
estim.model1b <- model2sempls(100, model1b.sig, sm, mm)
model1b.coeffs <- estim.model1b$coefficients$Estimate

# model 2 (Seiler)
mm <- cbind(c("y1", "y1", 
              "y2", "y2", 
              "y3", "y3", 
              "y4", "y4", "y4",
              "y5", "y5", "y5",
              "y6", "y6", "y6"),
            c("x11", "x12", 
              "x21", "x22", 
              "x31", "x32", 
              "x41", "x42", "x43", 
              "x51", "x52", "x53", 
              "x61", "x62", "x63"))
lam <- c(0.917, 0.917,
         0.85, 0.85,
         0.9, 0.9,
         0.825, 0.825, 0.898,
         0.802, 0.805, 0.858,
         0.806, 0.913, 0.84)
colnames(mm) <- c("source", "target")

sm <- cbind(c("y1", "y2", "y3", "y4", "y1", "y4", "y5"), 
            c("y3", "y3", "y5","y5", "y6", "y6", "y6"))
beta <- c(0.315, 0.5, 0, 0.433, 0.268, 0.364984, 0.522043)
colnames(sm) <- c("source", "target")

model2.sig <- core(mm, lam, sm, beta)

estim.model2 <- model2sempls(100, model2.sig, sm, mm)

model2.coeffs <- c(0.917, 0.917,
                   0.85, 0.85,
                   0.9, 0.9,
                   0.825, 0.825, 0.898,
                   0.802, 0.805, 0.858,
                   0.806, 0.913, 0.84,
                   0.315, 0.5, 0, 0.433, 0.268, 0.171, 0.448)

# model 2 (Seiler)
mm <- cbind(c("y1", "y1", 
              "y2", "y2", 
              "y3", "y3", 
              "y4", "y4", "y4",
              "y5", "y5", "y5",
              "y6", "y6", "y6"),
            c("x11", "x12", 
              "x21", "x22", 
              "x31", "x32", 
              "x41", "x42", "x43", 
              "x51", "x52", "x53", 
              "x61", "x62", "x63"))
lam <- c(0.917, 0.917,
         0.85, 0.85,
         0.9, 0.9,
         0.825, 0.825, 0.898,
         0.802, 0.805, 0.858,
         0.806, 0.913, 0.84)
colnames(mm) <- c("source", "target")

sm <- cbind(c("y1", "y2", "y3", "y4", "y1", "y4", "y5"), 
            c("y3", "y3", "y5","y5", "y6", "y6", "y6"))
beta <- c(0.315, 0.5, 351, 0.433, 0.268, 0.364984, 0.522043)
colnames(sm) <- c("source", "target")

model2b.sig <- core(mm, lam, sm, beta)

estim.model2b <- model2sempls(100, model2b.sig, sm, mm)

model2b.coeffs <- c(0.917, 0.917,
                   0.85, 0.85,
                   0.9, 0.9,
                   0.825, 0.825, 0.898,
                   0.802, 0.805, 0.858,
                   0.806, 0.913, 0.84,
                   0.315, 0.5, 351, 0.433, 0.268, 0.171, 0.448)

pathDiagram(estim.model2b, file="estim_model2b", edge.labels = "both", output.type = "graphics", digits = 3)

# model 3 (ECSI)
mm <- cbind(c("Image", "Image", "Image", "Image", "Image",
              "Expectation", "Expectation", "Expectation",
              "Quality", "Quality", "Quality", "Quality", "Quality", "Quality", "Quality",
              "Value", "Value",
              "Satisfaction", "Satisfaction", "Satisfaction",
              "Complaints",
              "Loyalty", "Loyalty", "Loyalty"),
            c("IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5",
              "CUEX1", "CUEX2", "CUEX3",
              "PERQ1", "PERQ2", "PERQ3", "PERQ4", "PERQ5", "PERQ6", "PERQ7",
              "PERV1", "PERV2",
              "CUSA1", "CUSA2", "CUSA3",
              "CUSC0",
              "CUSL1", "CUSL2", "CUSL3"))
lam <- c(0.743, 0.601, 0.578, 0.768, 0.744,
         0.771, 0.687, 0.612,
         0.803, 0.637, 0.784, 0.769, 0.756, 0.775, 0.779,
         0.9, 0.9,
         0.799, 0.846, 0.852,
         1,
         0.814, 0.219, 0.917)
colnames(mm) <- c("source", "target")

sm <- cbind(c("Image", "Image", "Image", 
              "Expectation", "Expectation", "Expectation", 
              "Quality", "Quality", 
              "Value",
              "Satisfaction", "Satisfaction",
              "Complaints"), 
            c("Expectation", "Satisfaction", "Loyalty", 
              "Quality", "Value", "Satisfaction", 
              "Value", "Satisfaction", 
              "Satisfaction",
              "Complaints", "Loyalty",
              "Loyalty"))
beta <- c(0.505, 0.64, 0.195, 0.557, 0.51, 0.64, 0.557, 0.513, 0.192, 0.526, 0.483, 0.71)
colnames(sm) <- c("source", "target")

smc <- cbind(c("Image", "Image", "Image", "Image", "Image", "Image",
              "Expectation", "Expectation", "Expectation", "Expectation", "Expectation", 
              "Quality", "Quality", "Quality", "Quality", 
              "Value", "Value", "Value",
              "Satisfaction", "Satisfaction",
              "Complaints"), 
            c("Expectation", "Quality", "Value", "Satisfaction", "Complaints", "Loyalty", 
              "Quality", "Value", "Satisfaction", "Complaints", "Loyalty",
              "Value", "Satisfaction", "Complaints", "Loyalty", 
              "Satisfaction", "Complaints", "Loyalty",
              "Complaints", "Loyalty",
              "Loyalty"))
betac <- c(0.5047056, 0.7489290, 0.5082422, 0.6926981, 0.4746156, 0.5640697, 
          0.5572479, 0.3612955, 0.5096005, 0.2574177, 0.3797380,
          0.5855181, 0.7946912,  0.5315327, 0.5375482,
          0.6060875, 0.3547311, 0.5296242,
          0.5260973, 0.6562752, 
          0.4183079)

# additional ecsi info
library("semPLS")
data(ECSImobi)
ecsi <- sempls(ECSImobi, mobi)
cor(ecsi$factor_scores)[upper.tri(cor(ecsi$factor_scores))]

Image <- mobi[, c("IMAG1", "IMAG2", "IMAG3", "IMAG4", "IMAG5")]
Quality <- mobi[, c("PERQ1", "PERQ2", "PERQ3", "PERQ4", "PERQ5", "PERQ6", "PERQ7")]

siglist <- list(Image = cor(Image), Quality = cor(Quality))

model3.sig <- core(mm, lam, smc, betac, siglist)

estim.model3 <- model2sempls(100, model3.sig, sm, mm)

model3.coeffs <- estim.model3$coefficients$Estimate

pathDiagram(estim.model3, file="estim_model3", edge.labels = "both", output.type = "graphics", digits = 3)


# cleaning up models
# rm(list = setdiff(ls(), lsf.str()))

# ---- Simulating ----
nmonte <- 10000
nobs <- 1000

library("matrixpls")
library("mvnfast")
library("MASS")
model1 <- monte_carlo(nmonte, nobs, model1.sig, estim.model1$model, model1.coeffs, matrixpls.sempls, rmvn)
model1b <- monte_carlo(nmonte, nobs, model10.sig, estim.model10$model, model10.coeffs, matrixpls.sempls, rmvn)

model2 <- monte_carlo(nmonte, nobs, model2.sig, estim.model2$model, model2.coeffs, sempls, rmvn)
model2b <- monte_carlo(nmonte, nobs, model2.sig, estim.model2$model, model2.coeffs, sempls, rmvn)

model3 <- monte_carlo(nmonte, nobs, model3.sig, estim.model3$model, model3.coeffs, matrixpls.sempls, mvrnorm)

# ---- Plotting ----
coeff_plot(model1)
coeff_plot(model1b)

coeff_plot(model2)
coeff_plot(model2b)

coeff_plot(model3)

parallelplot_simsempls(model1, subset = 1:ncol(model1$t), reflinesAt = c(-1, 0, 1))
parallelplot_simsempls(model1b, subset = 1:ncol(model1b$t), reflinesAt = c(-1, 0, 1))

parallelplot_simsempls(model2, subset = 1:ncol(model2$t), reflinesAt = c(-1, 0, 1))
parallelplot_simsempls(model2b, subset = 1:ncol(model2b$t), reflinesAt = c(-1, 0, 1))

parallelplot_simsempls(model3, subset = 1:ncol(model3$t), reflinesAt = c(-1, 0, 1))

dist_plot(model1)
dist_plot(model1b)

dist_plot(model2)
dist_plot(model2b)

dist_plot(model3)