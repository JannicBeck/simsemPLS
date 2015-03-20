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

sresid <- sim_resid(ecsi.plsm$strucmod, 300)
mresid <- sim_resid(ecsi.plsm$measuremod, 300)


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
testmeasresid <- simplsm(ecsi.plsm, 10, scoeffs = scoeffs, mcoeffs = mcoeffs
                              , mresid = mresid)

# test different models
seiler.models <- simsempls(seiler.model, 10)

pb.models <- simsempls(PB.model, 100)

final.models <- simsempls(final.model, 10)

ecsi.models <- simsempls(ecsi.sempls, 100)

ecsi.boot <- bootsempls(ecsi.sempls, nboot = 100)

# printing models
parallelplot(ecsi.boot, subset = 1:ncol(ecsi.boot$t), reflinesAt = c(-1,0,1))

CoeffPlot(seiler.models, seiler.model)

CoeffPlot(pb.models, PB.model)

CoeffPlot(final.models, final.model)

CoeffPlot(ecsi.models, ecsi.sempls)

CoeffPlot(ecsi.plsm, testplsm)

CoeffPlot(ecsi.plsm, testnobs)

CoeffPlot(ecsi.plsm, teststrucresid)

CoeffPlot(ecsi.plsm, testmeasresid)
