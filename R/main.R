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

# lavaan syntax:
ecsi.lavaan      <- " Image =~ 0.743*IMAG1 + 0.601*IMAG2 + 0.578*IMAG3 + 0.768*IMAG4 + 0.744*IMAG5
                      Expectation =~ 0.771*CUEX1 + 0.687*CUEX2 + 0.612*CUEX3 
                      Quality =~ 0.803*PERQ1 + 0.637*PERQ2 + 0.784*PERQ3 + 0.769*PERQ4 + 0.756*PERQ5 + 0.775*PERQ6 + 0.779*PERQ7
                      Value =~ 0.904*PERV1 + 0.938*PERV2 
                      Satisfaction =~ 0.799*CUSA1 + 0.846*CUSA2 + 0.852*CUSA3
                      Complaints =~ 1*CUSC0 
                      Loyalty =~ 0.814*CUSL1 + 0.219*CUSL2 + 0.917*CUSL3

                      Expectation ~ 0.505*Image 
                      Quality ~ 0.557*Expectation 
                      Value ~ 0.051*Expectation + 0.557*Quality 
                      Satisfaction ~ 0.179*Image + 0.064*Expectation + 0.513*Quality + 0.192*Value
                      Complaints ~ 0.526*Satisfaction 
                      Loyalty ~ 0.195*Image + 0.483*Satisfaction + 0.071*Complaints
                    "

# ---- CFA ----
# lavaan test model
cfa.mm <- cbind(c("f1", "f1", "f1", "f2", "f2", "f2"),c("y1", "y2", "y3", "y4", "y5", "y6"))
cfa.sm <- cbind(c("f1"), c("f2"))

# create artificial data, only used for creating plsm object
cfa.data <- matrix(0, 250, 6)
cfa.data <- as.data.frame(cfa.data)
colnames(cfa.data) <- c("y1", "y2", "y3", "y4", "y5", "y6")

# create plsm object
cfa.plsm <- plsm(cfa.data, cfa.sm, cfa.mm)

# lavaan syntax:
cfa.lavaan       <- " f1 =~ 0.7*y1 + 0.7*y2 + 0.7*y3
                      f2 =~ 0.7*y4 + 0.7*y5 + 0.7*y6

                      f2 ~ 0.5*f1 "

# ---- Specify Input Parameters -----
# ---- ECSI ----

# input coefficients for sim1
ecsi.scoeffs <- as.vector(ecsi.sempls$path_coefficients[ecsi.sempls$path_coefficients != 0])
ecsi.mcoeffs <- as.vector(ecsi.sempls$outer_loadings[ecsi.sempls$outer_loadings != 0])

# input coefficients for sim2
ecsi.fscores <- ecsi.sempls$factor_scores
ecsi.data <- ecsi.sempls$data
ecsi.scoeffs <- cor(ecsi.fscores)
ecsi.mcoeffs <- ecsi.sempls$outer_loadings


# improved coefficients of the model
ecsi.scoeffs <- c(0.705, 0.857, 0.710, 0.957, 0.678, 0.564, 0.851, 0.591, 0.825, 0.495, 0.683, 0.371)
ecsi.mcoeffs <- c(0.843, 0.901, 0.978, 0.868, 0.844,
             0.871, 0.987, 0.912,
             0.903, 0.937, 0.848, 0.869, 0.856, 0.875, 0.879,
             0.904, 0.938,
             0.799, 0.846, 0.852,
             1.000,
             0.814, 0.919, 0.917)

# !!!!!!!!!!!! ATTENTION AT SIMULATING COEFFICIENTS RANDOMLY !!!!!!!!!!!!!!
# f.e. LV has 1 MV -> coeff 1.0 fixed! implement!
# simulate random coefficients
ecsi.scoeffs <- sample(seq(-1, 1, by = 0.01), 12)
ecsi.mcoeffs <- sample(seq(-1, 1, by = 0.01), 24)

# ensure only positive coefficients
ecsi.scoeffs <- sqrt(ecsi.scoeffs^2)
ecsi.mcoeffs <- sqrt(ecsi.mcoeffs^2)

# input residuals
ecsi.sresid <- sim_resid(ecsi.plsm$strucmod, 400, rnorm)
ecsi.mresid <- sim_resid(ecsi.plsm$measuremod, 400, rnorm)

# simulate non normal residuals
non_normal <- function(n, mean, sd){

    mean <- 0
    sd <- 0
    
    x = seq(n)
    y = 10 + 10 *x + 20 * rchisq(n,df=2)
    y <- scale(y)
    non_normal_lm = lm(y~x+0) 
    
    result <- as.numeric(non_normal_lm$residuals)
    
    return(result)
}


# input non normal residuals
ecsi.sresid <- sim_resid(ecsi.plsm$strucmod, 400, non_normal)
ecsi.mresid <- sim_resid(ecsi.plsm$measuremod, 400, non_normal)

# ---- CFA ----
# input coefficients for sim1
cfa.scoeffs <- 0.5
cfa.mcoeffs <- c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7)

# input coefficients for sim2
cfa.scoeffs <- cfa.plsm$D; data.entry(cfa.scoeffs)
cfa.mcoeffs <- cfa.plsm$M; data.entry(cfa.mcoeffs)

# input residuals
cfa.sresid <- sim_resid(cfa.plsm$strucmod, 400, rnorm)
cfa.mresid <- sim_resid(cfa.plsm$measuremod, 400, rnorm)

# ---- Estimate Models ----

# ---- ECSI ----
# test with sempls object
test.ecsi.sempls <- simsempls(ecsi.sempls, 100)

# test with sempls object and matrixpls
test.ecsi.sempls.matrix <- simsempls(ecsi.sempls, 100, "matrixpls.sempls")

# test with plsm object
test_plsm(ecsi.plsm, ecsi.scoeffs, ecsi.mcoeffs, ecsi.sresid, ecsi.mresid)

# test with lavaan object
test.ecsi.lavaan <- simlavaan(ecsi.plsm, ecsi.lavaan, 100, 250, ecsi.scoeffs, ecsi.mcoeffs, "sempls")

# ---- CFA ----
# test with plsm object
test_plsm(cfa.plsm, cfa.scoeffs, cfa.mcoeffs, cfa.sresid, cfa.mresid)

# test with lavaan object
test.cfa.lavaan <- simlavaan(cfa.plsm, cfa.lavaan, 100, 250, cfa.scoeffs, cfa.mcoeffs, "sempls")

# ---- Plotting models ----

# ---- coefficient plots ----
# ---- ECSI ----
coeff_plot(test.ecsi.sempls)
coeff_plot(test.ecsi.sempls.matrix)
coeff_plot(test.ecsi.plsm)
coeff_plot(test.ecsi.plsm.matrix)
coeff_plot(test.ecsi.plsm.matrix.nmonte)
coeff_plot(test.ecsi.plsm.matrix.nmonte.high)
coeff_plot(test.ecsi.plsm.nobs)
coeff_plot(test.ecsi.plsm.nobs.low)
coeff_plot(test.ecsi.plsm.nobs.high)
coeff_plot(test.ecsi.plsm.nobs.ultra)
coeff_plot(test.ecsi.plsm.mresid)
coeff_plot(test.ecsi.plsm.sresid)
coeff_plot(test.ecsi.plsm.resid)

# ---- CFA ----
coeff_plot(test.cfa.plsm)
coeff_plot(test.cfa.plsm.matrix)
coeff_plot(test.cfa.plsm.matrix.nmonte)
coeff_plot(test.cfa.plsm.matrix.nmonte.high)
coeff_plot(test.cfa.plsm.nobs)
coeff_plot(test.cfa.plsm.nobs.low)
coeff_plot(test.cfa.plsm.nobs.high)
coeff_plot(test.cfa.plsm.nobs.ultra)
coeff_plot(test.cfa.plsm.mresid)
coeff_plot(test.cfa.plsm.sresid)
coeff_plot(test.cfa.plsm.resid)

# ---- parallel plots ----
# ---- ECSI ----
parallelplot.simsempls(test.ecsi.sempls, subset = 1:ncol(test.ecsi.sempls$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.ecsi.sempls.matrix, subset = 1:ncol(test.ecsi.sempls.matrix$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.ecsi.plsm, subset = 1:ncol(test.ecsi.plsm$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.ecsi.plsm.matrix, subset = 1:ncol(test.ecsi.plsm.matrix$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.ecsi.plsm.matrix.nmonte, subset = 1:ncol(test.ecsi.plsm.matrix.nmonte$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.ecsi.plsm.matrix.nmonte.high, subset = 1:ncol(test.ecsi.plsm.matrix.nmonte.high$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.ecsi.plsm.nobs, subset = 1:ncol(test.ecsi.plsm.nobs$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.ecsi.plsm.nobs.low, subset = 1:ncol(test.ecsi.plsm.nobs.low$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.ecsi.plsm.nobs.high, subset = 1:ncol(test.ecsi.plsm.nobs.high$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.ecsi.plsm.nobs.ultra, subset = 1:ncol(test.ecsi.plsm.nobs.ultra$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.ecsi.plsm.mresid, subset = 1:ncol(test.ecsi.plsm.mresid$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.ecsi.plsm.sresid, subset = 1:ncol(test.ecsi.plsm.sresid$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.ecsi.plsm.resid, subset = 1:ncol(test.ecsi.plsm.resid$t), reflinesAt = c(-1, 0, 1))

# ---- CFA ----
parallelplot.simsempls(test.cfa.plsm, subset = 1:ncol(test.cfa.plsm$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.cfa.plsm.matrix, subset = 1:ncol(test.cfa.plsm.matrix$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.cfa.plsm.matrix.nmonte, subset = 1:ncol(test.cfa.plsm.matrix.nmonte$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.cfa.plsm.matrix.nmonte.high, subset = 1:ncol(test.cfa.plsm.matrix.nmonte.high$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.cfa.plsm.nobs, subset = 1:ncol(test.cfa.plsm.nobs$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.cfa.plsm.nobs.low, subset = 1:ncol(test.cfa.plsm.nobs.low$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.cfa.plsm.nobs.high, subset = 1:ncol(test.cfa.plsm.nobs.high$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.cfa.plsm.nobs.ultra, subset = 1:ncol(test.cfa.plsm.nobs.ultra$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.cfa.plsm.mresid, subset = 1:ncol(test.cfa.plsm.mresid$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.cfa.plsm.sresid, subset = 1:ncol(test.cfa.plsm.sresid$t), reflinesAt = c(-1, 0, 1))
parallelplot.simsempls(test.cfa.plsm.resid, subset = 1:ncol(test.cfa.plsm.resid$t), reflinesAt = c(-1, 0, 1))
