test_plsm <- function(object, scoeffs, mcoeffs, sresid, mresid, FUN){
    
    # test with plsm object
    assign(paste("test.",substitute(object), sep = ""), 
           simplsm(object, 100, scoeffs = scoeffs, mcoeffs = mcoeffs),
           envir = .GlobalEnv)
    
    # test with plsm object and matrixpls
    assign(paste("test.",substitute(object),".matrix", sep = ""), 
           simplsm(object, 100, scoeffs = scoeffs, mcoeffs = mcoeffs, FUN = "matrixpls.sempls"),
           envir = .GlobalEnv)
    
    # test with plsm object, matrixpls and high nmonte
    assign(paste("test.",substitute(object),".matrix.nmonte", sep = ""), 
           simplsm(object, 250, scoeffs = scoeffs, mcoeffs = mcoeffs, FUN = "matrixpls.sempls"),
           envir = .GlobalEnv)
    
    # test with plsm object, matrixpls and high nmonte
    assign(paste("test.",substitute(object),".matrix.nmonte.high", sep = ""),
           simplsm(object, 1000, scoeffs = scoeffs, mcoeffs = mcoeffs, FUN = "matrixpls.sempls"),
           envir = .GlobalEnv)
    
    # test with nobs parameter
    assign(paste("test.",substitute(object),".nobs", sep = ""),
           simplsm(object, 100, nobs = 250, scoeffs = scoeffs, mcoeffs = mcoeffs, FUN = "matrixpls.sempls"),
           envir = .GlobalEnv)
    
    # test with few observations
    assign(paste("test.",substitute(object),".nobs.low", sep = ""), 
           simplsm(object, 100, nobs = 10, scoeffs = scoeffs, mcoeffs = mcoeffs, FUN = "matrixpls.sempls", maxit = 1000),
           envir = .GlobalEnv)
    
    # test with high observations
    assign(paste("test.",substitute(object),".nobs.high", sep = ""), 
           simplsm(object, 100, nobs = 1000, scoeffs = scoeffs, mcoeffs = mcoeffs, FUN = "matrixpls.sempls"),
           envir = .GlobalEnv)
    
    # test with ultra observations
    assign(paste("test.",substitute(object),".nobs.ultra", sep = ""), 
           simplsm(object, 100, nobs = 10000, scoeffs = scoeffs, mcoeffs = mcoeffs, FUN = "matrixpls.sempls"),
           envir = .GlobalEnv)
    
    # test with strucmod residuals
    assign(paste("test.",substitute(object),".sresid", sep = ""), 
           simplsm(object, 100, scoeffs = scoeffs, mcoeffs = mcoeffs, sresid = sresid, FUN = "matrixpls.sempls"),
           envir = .GlobalEnv)
    
    # test with measuremod residuals
    assign(paste("test.",substitute(object),".mresid", sep = ""), 
           simplsm(object, 100, scoeffs = scoeffs, mcoeffs = mcoeffs, mresid = mresid, FUN = "matrixpls.sempls"),
           envir = .GlobalEnv)
    
    # test with residuals
    assign(paste("test.",substitute(object),".resid", sep = ""), 
           simplsm(object, 100, scoeffs = scoeffs, mcoeffs = mcoeffs, sresid = sresid, mresid = mresid, FUN = "matrixpls.sempls"),
           envir = .GlobalEnv)
    
}