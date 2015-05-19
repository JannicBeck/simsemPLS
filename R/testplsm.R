test_plsm <- function(object, scoeffs, mcoeffs, FUN){
    
    # test with plsm object
    assign(paste("test.",substitute(object), sep = ""), 
           simplsm(object, 100, scoeffs = scoeffs, mcoeffs = mcoeffs, FUN = "matrixpls.sempls"),
           envir = .GlobalEnv)
    
    # test with plsm object, matrixpls and high nmonte
    assign(paste("test.",substitute(object),".matrix.nmonte.high", sep = ""),
           simplsm(object, 1000, scoeffs = scoeffs, mcoeffs = mcoeffs, FUN = "matrixpls.sempls"),
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

}