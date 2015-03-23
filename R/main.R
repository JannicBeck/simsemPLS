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

# input number of observations
nobs <- nrow(data)

# input residuals
sresid <- sim_resid(ecsi.plsm$strucmod, 400)
mresid <- sim_resid(ecsi.plsm$measuremod, 400)


# test with sempls object
testsempls <- simsempls(ecsi.sempls, 1000)

# test with plsm object
testplsm <- simplsm(ecsi.plsm, 1000, scoeffs = scoeffs, mcoeffs = mcoeffs)

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

# plot models
coeff_plot(testsempls, ecsi.plsm)
coeff_plot(testplsm, ecsi.plsm)
coeff_plot(testnobs, ecsi.plsm)
coeff_plot(testhighnobs, ecsi.plsm)
coeff_plot(testultranobs, ecsi.plsm)
coeff_plot(testlownobs, ecsi.plsm)
coeff_plot(teststrucresid, ecsi.plsm)
coeff_plot(testmeasresid, ecsi.plsm)

# parallelplot models
parallelplot.sempls(testsempls, subset = 1:ncol(testsempls$t), reflinesAt = c(-1, 0, 1))
parallelplot.plsm(testplsm, subset = 1:ncol(testplsm$t), reflinesAt = c(-1, 0, 1))
parallelplot.sempls(testnobs, subset = 1:ncol(testnobs$t), reflinesAt = c(-1, 0, 1))
parallelplot.sempls(testhighnobs, subset = 1:ncol(testhighnobs$t), reflinesAt = c(-1, 0, 1))
parallelplot.sempls(testultranobs, subset = 1:ncol(testhighnobs$t), reflinesAt = c(-1, 0, 1))
parallelplot.sempls(testlownobs, subset = 1:ncol(testlownobs$t), reflinesAt = c(-1, 0, 1))
parallelplot.plsm(teststrucresid, subset = 1:ncol(teststrucresid$t), reflinesAt = c(-1, 0, 1))
parallelplot.sempls(testmeasresid, subset = 1:ncol(testmeasresid$t), reflinesAt = c(-1, 0, 1))


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

