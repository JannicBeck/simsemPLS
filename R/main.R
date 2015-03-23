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
scoeffs <- as.vector(ecsi.sempls$path_coefficients[ecsi.sempls$path_coefficients != 0])
mcoeffs <- as.vector(ecsi.sempls$outer_loadings[ecsi.sempls$outer_loadings != 0])

# input coefficients
scoeffs <- as.vector(ECSIsempls$path_coefficients[ECSIsempls$path_coefficients != 0])
mcoeffs <- as.vector(ECSIsempls$outer_loadings[ECSIsempls$outer_loadings != 0])

nobs <- nrow(data)

sresid <- sim_resid(ecsi.plsm$strucmod, 300)
mresid <- sim_resid(ecsi.plsm$measuremod, 300)


# test with sempls object
testsempls <- simsempls(ecsi.sempls, 100)

# test with plsm object
testplsm <- simplsm(ecsi.plsm, 100, scoeffs = scoeffs, mcoeffs = mcoeffs)

# test with nobs parameter
testnobs <- simplsm(ecsi.plsm, 100, nobs = 250, scoeffs = scoeffs, mcoeffs = mcoeffs)

# test with strucmod residuals
teststrucresid <- simplsm(ecsi.plsm, 100, scoeffs = scoeffs, mcoeffs = mcoeffs
                         , sresid = sresid)

# test with measuremod residuals
testmeasresid <- simplsm(ecsi.plsm, 100, scoeffs = scoeffs, mcoeffs = mcoeffs
                              , mresid = mresid)

# test different models
seiler.models <- simsempls(seiler.model, 10)

pb.models <- simsempls(PB.model, 100)

final.models <- simsempls(final.model, 10)

ecsi.models <- simsempls(ecsi.sempls, 100)

min.sempls <- simsempls(ECSIsempls, 100)

min.plsm <- simplsm(ECSIpm, 100, scoeffs = scoeffs, mcoeffs = mcoeffs)


# printing coefficients
ecsi.boot <- bootsempls(ecsi.sempls, nboot = 100)

ecsi.boot$t <- testsempls$t


parallelplot.simsempls(testsempls, subset = 1:ncol(testsempls$t), reflinesAt = c(-1, 0, 1))


CoeffPlot(seiler.models, seiler.model)

CoeffPlot(pb.models, PB.model)

CoeffPlot(final.models, final.model)

CoeffPlot(testsempls, ecsi.plsm)

CoeffPlot(testplsm, ecsi.plsm)

CoeffPlot(testnobs, ecsi.plsm)

CoeffPlot(teststrucresid, ecsi.plsm)

CoeffPlot(min.sempls, ECSIpm)

CoeffPlot(min.plsm, ECSIpm)
