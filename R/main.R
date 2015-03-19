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

ecsi.sempls <- sempls(ecsi.plsm, data)

# input coefficients
scoeffs <- as.vector(ecsi.sempls$path_coefficients[ecsi.sempls$path_coefficients>0])
mcoeffs <- as.vector(ecsi.sempls$outer_loadings[ecsi.sempls$outer_loadings>0])

nobs <- nrow(data)

sresid <- sim_resid(ecsi.plsm$strucmod, nobs)
mresid <- sim_resid(ecsi.plsm$measuremod, nobs)


# test with sempls object
testsempls <- simsempls(ecsi.sempls, 10)

# test with plsm object
testplsm <- simplsm(ecsi.plsm, 10, scoeffs = scoeffs, mcoeffs = mcoeffs)

# test with nobs parameter
testnobs <- simplsm(ecsi.plsm, 10, nobs = 250, scoeffs = scoeffs, mcoeffs = mcoeffs)

# test with strucmod residuals
teststrucresid <- simplsm(ecsi.plsm, 10, scoeffs = scoeffs, mcoeffs = mcoeffs
                         , sresid = sresid)

# test with measuremod residuals
teststrucresid <- simplsm(ecsi.plsm, 10, scoeffs = scoeffs, mcoeffs = mcoeffs
                              , mresid = mresid)
