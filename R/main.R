# load package
library("semPLS")

# get data
data(ECSImobi)

# rename model
ecsi.plsm <- ECSImobi

# rename dataframe
data <- mobi

# remove unused objects
rm("ECSImm", "ECSIsm", "ECSImobi", "mobi")

# estimate model
ecsi.sempls <- sempls(ecsi.plsm, data)

# input coefficients
scoeffs <- as.vector(ecsi.sempls$path_coefficients[ecsi.sempls$path_coefficients != 0])
mcoeffs <- as.vector(ecsi.sempls$outer_loadings[ecsi.sempls$outer_loadings != 0])


# !!!!!!!!!!!! ATTENTION AT SIMULATING COEFFICIENTS RANDOMLY !!!!!!!!!!!!!!
# f.e. LV has 1 MV -> coeff 1.0 fixed! implement!

# slightly improved coefficients of the model
scoeffs <- c(0.505, 0.557, 0.510, 0.657, 0.378, 0.264, 0.651, 0.391, 0.625, 0.295, 0.583, 0.171)
mcoeffs <- c(0.843, 0.701, 0.778, 0.868, 0.844,
             0.871, 0.787, 0.712,
             0.903, 0.737, 0.848, 0.869, 0.856, 0.875, 0.879,
             0.904, 0.938,
             0.799, 0.846, 0.852,
             1.000,
             0.814, 0.719, 0.917)

scoeffs <- sample(seq(-1, 1, by = 0.01), 12)
mcoeffs <- sample(seq(-1, 1, by = 0.01), 24)

scoeffs <- sqrt(scoeffs^2)
mcoeffs <- sqrt(mcoeffs^2)

# input number of observations
nobs <- nrow(data)

# input residuals
sresid <- sim_resid(ecsi.plsm$strucmod, 400, rnorm)
mresid <- sim_resid(ecsi.plsm$measuremod, 400, rnorm)

# non normal residuals
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
sresid <- sim_resid(ecsi.plsm$strucmod, 400, non_normal)
mresid <- sim_resid(ecsi.plsm$measuremod, 400, non_normal)


# test with sempls object
testsempls <- simsempls(ecsi.sempls, 100)

# test with plsm object
testplsm <- simplsm(ecsi.plsm, 1, scoeffs = scoeffs, mcoeffs = mcoeffs)

# test with nobs parameter
testnobs <- simplsm(ecsi.plsm, 1000, nobs = 300, scoeffs = scoeffs, mcoeffs = mcoeffs)

# test with many observations
testhighnobs <- simplsm(ecsi.plsm, 1000, nobs = 1000, scoeffs = scoeffs, mcoeffs = mcoeffs)

# test with many observations
testultranobs <- simplsm(ecsi.plsm, 1000, nobs = 10000, scoeffs = scoeffs, mcoeffs = mcoeffs)

# test with few
testlownobs <- simplsm(ecsi.plsm, 1000, nobs = 10, scoeffs = scoeffs, mcoeffs = mcoeffs)

# test with strucmod residuals
teststrucresid <- simplsm(ecsi.plsm, 1000, scoeffs = scoeffs, mcoeffs = mcoeffs
                         , sresid = sresid)

# test with measuremod residuals
testmeasresid <- simplsm(ecsi.plsm, 1000, scoeffs = scoeffs, mcoeffs = mcoeffs
                              , mresid = mresid)

# test with residuals
testresid <- simplsm(ecsi.plsm, 100, scoeffs = scoeffs, mcoeffs = mcoeffs
                         , sresid = sresid, mresid = mresid)

# plot models
coeff_plot(testsempls, ecsi.plsm)
coeff_plot(testplsm, ecsi.plsm)
coeff_plot(testnobs, ecsi.plsm)
coeff_plot(testhighnobs, ecsi.plsm)
coeff_plot(testultranobs, ecsi.plsm)
coeff_plot(testlownobs, ecsi.plsm)
coeff_plot(teststrucresid, ecsi.plsm)
coeff_plot(testmeasresid, ecsi.plsm)
coeff_plot(testresid, ecsi.plsm)

# parallelplot models
parallelplot.sempls(testsempls, subset = 1:ncol(testsempls$t), reflinesAt = c(-1, 0, 1))
parallelplot.sempls(testplsm, subset = 1:ncol(testplsm$t), reflinesAt = c(-1, 0, 1))
parallelplot.sempls(testnobs, subset = 1:ncol(testnobs$t), reflinesAt = c(-1, 0, 1))
parallelplot.sempls(testhighnobs, subset = 1:ncol(testhighnobs$t), reflinesAt = c(-1, 0, 1))
parallelplot.sempls(testultranobs, subset = 1:ncol(testhighnobs$t), reflinesAt = c(-1, 0, 1))
parallelplot.sempls(testlownobs, subset = 1:ncol(testlownobs$t), reflinesAt = c(-1, 0, 1))
parallelplot.sempls(teststrucresid, subset = 1:ncol(teststrucresid$t), reflinesAt = c(-1, 0, 1))
parallelplot.sempls(testmeasresid, subset = 1:ncol(testmeasresid$t), reflinesAt = c(-1, 0, 1))
parallelplot.sempls(testresid, subset = 1:ncol(testresid$t), reflinesAt = c(-1, 0, 1))


# test different models
# input coefficients
scoeffs <- as.vector(ECSIsempls$path_coefficients[ECSIsempls$path_coefficients != 0])
mcoeffs <- as.vector(ECSIsempls$outer_loadings[ECSIsempls$outer_loadings != 0])

seiler.models <- simsempls(seiler.model, 10)

pb.models <- simsempls(PB.model, 100)

final.models <- simsempls(final.model, 10)

ecsi.models <- simsempls(ecsi.sempls, 100)

min.sempls <- simsempls(ECSIsempls, 100)

min.plsm <- simplsm(ECSIpm, 100, scoeffs = scoeffs, mcoeffs = mcoeffs)


coeff_plot(seiler.models, seiler.model)

coeff_plot(pb.models, PB.model)

coeff_plot(final.models, final.model)

coeff_plot(min.sempls, ECSIpm)

coeff_plot(min.plsm, ECSIpm)

