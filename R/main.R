# ---- Specify Models ----

# ---- ECSI ----
# load package
library("semPLS")

# get data
data(ECSImobi)

# rename model
ecsi.plsm <- ECSImobi

# rename dataframe
ecsi.data <- mobi

# remove unused objects
rm("ECSImm", "ECSIsm", "ECSImobi", "mobi")

# estimate model
ecsi.sempls <- sempls(ecsi.plsm, ecsi.data)

# ---- Specify Input Parameters -----
# input coefficients for sim1
ecsi.scoeffs <- as.vector(ecsi.sempls$path_coefficients[ecsi.sempls$path_coefficients != 0])
ecsi.mcoeffs <- as.vector(ecsi.sempls$outer_loadings[ecsi.sempls$outer_loadings != 0])

# input coefficients for sim2
ecsi.fscores <- ecsi.sempls$factor_scores
ecsi.data <- ecsi.sempls$data
ecsi.scoeffs <- cor(ecsi.fscores)
ecsi.mcoeffs <- ecsi.sempls$outer_loadings



# ---- CFA ----
cfa.mm <- cbind(c("f1", "f1", "f1", "f2", "f2", "f2"),
                c("y1", "y2", "y3", "y4", "y5", "y6"))
cfa.sm <- cbind(c("f1"), c("f2"))

# create artificial data, only used for creating plsm object
cfa.data <- matrix(0, 250, 6)
cfa.data <- as.data.frame(cfa.data)
colnames(cfa.data) <- c("y1", "y2", "y3", "y4", "y5", "y6")

# create plsm object
cfa.plsm <- plsm(cfa.data, cfa.sm, cfa.mm)

# input coefficients for sim1
cfa.scoeffs <- 0.5
cfa.mcoeffs <- c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7)

# input coefficients for sim2
cfa.scoeffs <- cfa.plsm$D; data.entry(cfa.scoeffs)
cfa.mcoeffs <- cfa.plsm$M; data.entry(cfa.mcoeffs)

# input coefficients for sim2 matrixstyle
cfa.scoeffs <- cbind(c(1,0.5),c(0.5,1))
colnames(cfa.scoeffs) <- colnames(cfa.plsm$D)
rownames(cfa.scoeffs) <- rownames(cfa.plsm$D)
cfa.mcoeffs <- cbind(c(0.7, 0.7, 0.7, 0, 0, 0),c(0, 0, 0, 0.7, 0.7, 0.7))
colnames(cfa.mcoeffs) <- colnames(cfa.plsm$M)
rownames(cfa.mcoeffs) <- rownames(cfa.plsm$M)

# ---- JB ----
jb.mm <- cbind(c("image", "image", "image", "image", "image", 
                 "sat", "sat", "sat", 
                 "loyalty", "loyalty", "loyalty"),
               c("imag1", "imag2", "imag3", "imag4", "imag5",
                 "cusa1", "cusa2", "cusa3", 
                 "cusl1", "cusl2", "cusl3"))
jb.sm <- cbind(c("image", "image", "sat"), c("sat", "loyalty", "loyalty"))

# create artificial data, only used for creating plsm object
jb.data <- matrix(0, 250, 11)
jb.data <- as.data.frame(jb.data)
colnames(jb.data) <- c("imag1", "imag2", "imag3", "imag4", "imag5",
                        "cusa1", "cusa2", "cusa3", 
                       "cusl1", "cusl2", "cusl3")

# create plsm object
jb.plsm <- plsm(jb.data, jb.sm, jb.mm)

# input coefficients for sim1
jb.scoeffs <- c(0.695, 0.213, 0.507)
jb.mcoeffs <- c(0.745, 0.586, 0.576, 0.773, 0.752, 0.803, 0.839, 0.854, 0.828, 0.198, 0.911)

# input coefficients for sim2
jb.scoeffs <- jb.plsm$D; data.entry(jb.scoeffs)
jb.mcoeffs <- jb.plsm$M; data.entry(jb.mcoeffs)

# image   sat loyalty
# imag1 0.745 0.000   0.000
# imag2 0.586 0.000   0.000
# imag3 0.576 0.000   0.000
# imag4 0.773 0.000   0.000
# imag5 0.752 0.000   0.000
# cusa1 0.000 0.803   0.000
# cusa2 0.000 0.839   0.000
# cusa3 0.000 0.854   0.000
# cusl1 0.000 0.000   0.828
# cusl2 0.000 0.000   0.198
# cusl3 0.000 0.000   0.911

# image   sat loyalty
# image   1.000 0.695   0.213
# sat     0.695 1.000   0.507
# loyalty 0.213 0.507   1.000



# ---- Estimate Models ----

# ---- ECSI ----
test_plsm(ecsi.plsm, ecsi.scoeffs, ecsi.mcoeffs)
rm("test.ecsi.plsm", "test.ecsi.plsm.nmonte.high", "test.ecsi.plsm.nobs.low", "test.ecsi.plsm.nobs.high", "test.ecsi.plsm.nobs.ultra")

# cheat method
scoeffs.t0 <- as.vector(ecsi.sempls$path_coefficients[ecsi.sempls$path_coefficients != 0])
mcoeffs.t0 <- as.vector(ecsi.sempls$outer_loadings[ecsi.sempls$outer_loadings != 0])
test.ecsi.plsm$t0 <- c(mcoeffs.t0, scoeffs.t0)
test.ecsi.plsm.nmonte.high$t0 <- c(mcoeffs.t0, scoeffs.t0)
test.ecsi.plsm.nobs.low$t0 <- c(mcoeffs.t0, scoeffs.t0)
test.ecsi.plsm.nobs.high$t0 <- c(mcoeffs.t0, scoeffs.t0)
test.ecsi.plsm.nobs.ultra$t0 <- c(mcoeffs.t0, scoeffs.t0)


# ---- CFA ----
test_plsm(cfa.plsm, cfa.scoeffs, cfa.mcoeffs)
rm("test.cfa.plsm", "test.cfa.plsm.nmonte.high", "test.cfa.plsm.nobs.low", "test.cfa.plsm.nobs.high", "test.cfa.plsm.nobs.ultra")

# ---- JB ----
test_plsm(jb.plsm, jb.scoeffs, jb.mcoeffs)
rm("test.jb.plsm", "test.jb.plsm.nmonte.high", "test.jb.plsm.nobs.low", "test.jb.plsm.nobs.high", "test.jb.plsm.nobs.ultra")

# ---- Plotting models ----

# ---- coefficient plots ----
# ---- ECSI ----
coeff_plot(test.ecsi.plsm)
coeff_plot(test.ecsi.plsm.nmonte.high)
coeff_plot(test.ecsi.plsm.nobs.low)
coeff_plot(test.ecsi.plsm.nobs.high)
coeff_plot(test.ecsi.plsm.nobs.ultra)

# ---- CFA ----
coeff_plot(test.cfa.plsm)
coeff_plot(test.cfa.plsm.nmonte.high)
coeff_plot(test.cfa.plsm.nobs.low)
coeff_plot(test.cfa.plsm.nobs.high)
coeff_plot(test.cfa.plsm.nobs.ultra)
coeff_plot(test.cfa.plsm.perfect)

# ---- JB ----
coeff_plot(test.jb.plsm)
coeff_plot(test.jb.plsm.nmonte.high)
coeff_plot(test.jb.plsm.nobs.low)
coeff_plot(test.jb.plsm.nobs.high)
coeff_plot(test.jb.plsm.nobs.ultra)

# ---- parallel plots ----
# ---- ECSI ----
parallelplot.simsempls(test.ecsi.plsm, subset = 1:ncol(test.ecsi.plsm$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.ecsi.plsm.nmonte.high, subset = 1:ncol(test.ecsi.plsm.nmonte.high$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.ecsi.plsm.nobs.low, subset = 1:ncol(test.ecsi.plsm.nobs.low$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.ecsi.plsm.nobs.high, subset = 1:ncol(test.ecsi.plsm.nobs.high$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.ecsi.plsm.nobs.ultra, subset = 1:ncol(test.ecsi.plsm.nobs.ultra$t), reflinesAt = c(-1, 0, 1))

# ---- CFA ----
parallelplot.simsempls(test.cfa.plsm, subset = 1:ncol(test.cfa.plsm$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.cfa.plsm.nmonte.high, subset = 1:ncol(test.cfa.plsm.nmonte.high$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.cfa.plsm.nobs.low, subset = 1:ncol(test.cfa.plsm.nobs.low$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.cfa.plsm.nobs.high, subset = 1:ncol(test.cfa.plsm.nobs.high$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.cfa.plsm.nobs.ultra, subset = 1:ncol(test.cfa.plsm.nobs.ultra$t), reflinesAt = c(-1, 0, 1))

# ---- JB ----
parallelplot.simsempls(test.jb.plsm, subset = 1:ncol(test.jb.plsm$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.jb.plsm.nmonte.high, subset = 1:ncol(test.jb.plsm.nmonte.high$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.jb.plsm.nobs.low, subset = 1:ncol(test.jb.plsm.nobs.low$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.jb.plsm.nobs.high, subset = 1:ncol(test.jb.plsm.nobs.high$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.jb.plsm.nobs.ultra, subset = 1:ncol(test.jb.plsm.nobs.ultra$t), reflinesAt = c(-1, 0, 1))
