#' Simulate data on given plsm object.
#' 
#' @param object            : An object of class plsm as returned by the method plsm.
#' @param nmonte            : The number of monte carlo replications; the default is 100.
#' @param nobs              : The number of observations; the default is 200.
#' @param scoeffs           : A covariance matrix of the latent variable scores.
#' @param mcoeffs           : A covariance matrix of the manifest variables.
#' @param sresid            : A matrix of residuals for the structural model.
#' @param mresid            : A matrix of residuals for the measurement model.
#' @param FUN               : Name of function to estimate the plsm object; the default is "sempls".
#'                            For better performance use: "matrixpls.sempls" from matrixpls package.
#' 
#' @return An object of class simsempls.
#' @examples
#' simplsm(object, nmonte = 10, scoeffs = scoeffs, mcoeffs = mcoeffs, sresid = sresid, mresid = mresid, FUN = sempls)
#'        
simplsm <- function(object, nmonte = 100, nobs = 200, scoeffs = NULL, mcoeffs = NULL, sresid = NULL, mresid = NULL, FUN = "sempls", ...) {
    
    # check if object is of type plsm    
    if(is(object, "plsm")){        
        
        if(!(is.numeric(scoeffs)) || !(is.numeric(mcoeffs))){
            
            stop("Coefficients must be supplied as numeric vectors!")
        }
        
        # get measurement blocks
        blocks <- object$blocks
        
        # check if measurement model is specified reflective
        is_reflective(blocks)
        
        # get names of latent variables
        latent <- object$latent
        
        # get names of manifest variables
        manifest <- object$manifest
        
        # get structural model
        strucmod <- object$strucmod 
        
        # get measurement model
        measuremod <- object$measuremod
        
        # test if structural coefficients are of correct length
        if(length(scoeffs) > nrow(strucmod)){
            
            stop("Oops, you supplied more coefficients than there are equations in your structural model!")
        }else if(length(scoeffs) < nrow(strucmod)){
            
            stop("Oops, you supplied less coefficients than there are equations in your structural model!")
        }
        
        # test if measurement coefficients are of correct length
        if(length(mcoeffs) > nrow(measuremod)){
            
            stop("Oops, you supplied more coefficients than there are equations in your measurement model!")
        }else if(length(mcoeffs) < nrow(measuremod)){
            
            stop("Oops, you supplied less coefficients than there are equations in your measurement model!")
        }
        
        # test if residuals are supplied 
        if(!(is.null(sresid)) || !(is.null(mresid))){
            
            # get the supplied object
            get_object <- function() ifelse(is.null(sresid), return(mresid), return(sresid))
            
            # get number of observations from the supplied object
            nobs <- nrow(get_object())
            
            # get the model of the object which is not supplied
            get_other <- function() ifelse(is.null(sresid), return(strucmod), return(measuremod))
            
            # simulate residuals of the object which is not supplied
            sim.resid <- sim_resid(get_other(), nobs, rnorm)
            
            # re-assign object which is not supplied
            ifelse(is.null(sresid), sresid <- sim.resid, mresid <- sim.resid) 
            
        }else{
            
            # simulate residuals of structural model
            sresid <- sim_resid(strucmod, nobs, rnorm)
            
            # simulate residuals of measurement model
            mresid <- sim_resid(measuremod, nobs, rnorm)
        }
        
        # convert vector of structural coefficients to list 
        scoeffs <- to_list(scoeffs, strucmod)
        
    }else{
        
        # object is not of type plsm
        stop("The supplied object must be of class plsm")
    }
    
    if(FUN == "matrixpls.sempls"){
        
        if(!(require(matrixpls))){
            
            stop("The package 'matrixpls' is required, type: install.packages(matrixpls)")
        }
        
        FUN <- matrixpls.sempls
    }else{
        
        if(!(require(semPLS))){
            
            stop("The package 'sempls' is required, type: install.packages(sempls)")
        }
        FUN <- sempls
    }
    
    # simulate data
    result <- core(object, nmonte, nobs, latent, manifest, strucmod, measuremod, 
                   scoeffs, sresid, mcoeffs, mresid, FUN, ...)
    
    return(result)
}


#' Core function, actual simulating is done here.
#' 
core <- function(object, nmonte, nobs, latent, manifest, strucmod, measuremod, 
                 scoeffs, sresid, mcoeffs, mresid, FUN, ...) { 
    
    # get number of equations
    neq <- length(c(strucmod[, 2], measuremod[, 2]))
    
    # initialize simulated coefficient matrix
    t <- matrix(numeric(0), nrow = nmonte, ncol = neq)
    
    if(is(object, "sempls")){
        
        # get actual coefficients
        t0 <- object$coefficients$Estimate  
        
        # set path attribute
        attr(t, "path") <- object$coefficients[, 1]
        
        # set column names
        colnames(t) <- rownames(object$coefficients)
        
        # get plsm model
        model <- object$model
        
    }else{
        
        # get equations
        equations <- get_equations(object)
        
        # set path attribute
        attr(t, "path") <- c(equations[[1]][, 1], equations[[2]][, 1])
        
        # set column names
        colnames(t) <- c(equations[[1]][, 2], equations[[2]][, 2])
        
        # object is of type plsm
        model <- object
        
        # set actual coefficients to provided
        t0 <- c(mcoeffs, unlist(scoeffs))
    }
    
    # initialize list of datasets
    data <- vector("list", nmonte)
    
    cat("Simulate: ")
    
    for(i in 1:nmonte){
        
        # printing out the iteration
        treil <- paste(rep(" ", floor(log10(nmonte)) - floor(log10(i))), collapse="")
        ndel <- paste(rep("\b", floor(log10(nmonte)) + 1), collapse="")
        if(i==1) cat(paste(treil, i, sep=""))
        if(i!=nmonte) cat(paste(ndel, treil, i, sep=""))
        else cat(paste(ndel, i, " Done.\n", sep=""))
        
        # solve structural model with simulated exogenous factor scores
        # given coefficients and residuals
        sim.fscores <- sim_fscores(latent, strucmod, nobs, scoeffs, sresid)
        
        # solve measurement model with simulated factor scores,
        # loadings and residuals
        sim.data <- sim_data(manifest, measuremod, sim.fscores, nobs, 
                             mcoeffs, mresid)
        
        # save simulated data in list
        data[[i]] <- cbind(sim.data, sim.fscores)
        
        # estimate model with simulated data
        try(sim.model <- FUN(model, sim.data, verbose = FALSE, ...), silent = TRUE)
        
        # extract coefficients
        t[i, ] <- sim.model$coefficients[, 2]
        
    }
    
    # initialize and assign result list
    result <- list(t0 = t0, t = t, sresid = sresid, mresid = mresid, nobs = nobs, nmonte = nmonte, data = data, model = model)
    
    return(result)
}

#' Simulate a dataset on given factor scores, loadings and residuals.
#' 
#' @param manifest      : character of manifest variables
#'        strucmod      : 2 column "source", "target" matrix of structural model 
#'        sim.fscores   : matrix of simulated factor scores returned by SimStrucmod()
#'        nobs          : number of observations
#'        measuremod.lm : list of lm objects generated by RefitMeasuremod()
#' @return A simulated data matrix which fits the given model
#' @examples
#' sim_data(manifest, measuremod, sim.fscores, nobs, measuremod.lm)
sim_data <- function(manifest, measuremod, sim.fscores, nobs, 
                     mcoeffs, mresid) { 
    
    
    # get independent latent variable names
    indpndt.latent <- measuremod[, 1]
    
    # initialize result dataset
    result <- matrix(numeric(), nobs, length(manifest))  
    
    # set column names
    colnames(result) <- manifest
    
    for(i in seq_along(manifest)){
        
        result[, i] <- sim.fscores[, indpndt.latent[i]] * mcoeffs[i] + mresid[, i]
    }
    
    return(result) 
    
}


#' Simulate factor scores on given path coefficients and residuals.
#' 
#' @param latent        : character of latent variables
#'        strucmod      : 2 column "source", "target" matrix of structural model 
#'        nobs          : number of observations
#'        strucmod.lm   : list of lm objects generated by RefitStrucmod()
#' @return A matrix of simulated factor scores
#' @examples
#' sim_fscores(latent, strucmod, nobs, strucmod.lm)
sim_fscores <- function(latent, strucmod, nobs, scoeffs, sresid) { 
    
    # get endogenous latent variable names
    endogenous <- unique(strucmod[, 2])
    
    # get exogenous latent variable names
    exogenous <- setdiff(latent, strucmod[, 2])   
    
    # get number of exogenous latent variables
    mexo <- length(exogenous)
    
    # simulate random score for exogenous latent variable(s)
    exo.scores <- matrix(rnorm(nobs * mexo, mean=0, sd=1), nobs, mexo)
    
    # scale variable to ensure mean 0, sd = 1
    exo.scores <- scale(exo.scores)
    
    # set exogenous column names
    colnames(exo.scores) <- exogenous
    
    # initialize endgenous scores
    endo.scores <- matrix(numeric(), nobs, length(endogenous))
    
    # set endogenous column names
    colnames(endo.scores) <- endogenous
    
    # initialize simulated factor scores
    result <- cbind(exo.scores, endo.scores)
    
    # initialize index 
    i <- 1
    
    # initialize result index 
    j <- length(exogenous) + 1
    
    # estimate new endogenous factor scores
    for (dpndnt in endogenous) {
        
        # get index/indices of endogenous/target latent variable in strucmod
        dpndnt.index <- which(dpndnt == strucmod[, 2])
        
        # get corresponding name(s) of independent/source latent variable(s)
        indpnt <- strucmod[dpndnt.index, 1]  
        
        # solve structural model with simulated exogenous scores
        ifelse(length(scoeffs[[i]]) > 1, 
               result[, j] <- rowSums(t(t(result[, indpnt]) * scoeffs[[i]])) + sresid[, i], 
               result[, j] <- result[, indpnt] * scoeffs[[i]] + sresid[, i])
        
        # increment index by 1
        i <- i + 1
        
        # increment result index by 1
        j <- j + 1
        
    }
    
    return(result)
    
    
}

#' Simulate residuals on given structural or measurement model of a plsm object.
#' 
#' @param object        : structural or measurement model of a plsm object like returned by method plsm.
#'        nobs          : number of observations.
#'        FUN           : Function used for simulating residuals; the default is rnorm.
#' @return A residual matrix simulated with rnorm.
#' @examples
#' sim_resid(object, 200, FUN)
#' 
#' TODO: Implement different simulations based on parameter dist or func f.e. qnorm pnorm
sim_resid <- function(object, nobs, FUN) { 
    
    # simulate normally distributed residuals for equations
    result <- matrix(FUN(nobs * length(unique(object[, 2])), mean=0, sd=1), nobs, length(unique(object[, 2])))
    
}

#' Convert object from matrix or vector to list following a given structural model.
#' 
#' @param object        : Vector of coefficients or matrix of residuals
#'        strucmod      : A structural model of a plsm object.
#' @return A list of residuals or coefficients.
#' @examples
#' to_list(object, strucmod)
to_list <- function(object, strucmod){
    
    
    # get endogenous latent variable names
    endogenous <- unique(strucmod[, 2])
    
    # intialize result list
    result <- vector("list", length(endogenous))
    
    # convert vector or matrix to list 
    for(i in seq_along(result)){
        
        for (dpndnt in endogenous[i]) {            
            
            # get index/indices of endogenous/target latent variable in strucmod
            dpndnt.index <- which(dpndnt == strucmod[, 2])
            
            if(is.vector(object)){
                
                # write coefficients vector in list
                result[[i]] <- object[dpndnt.index]
            }else{
                
                # write residual matrix in list
                result[[i]] <- object[, dpndnt.index]
            }
            
            
        }
        
    }
    
    return(result)
    
}

get_equations <- function(model){
    
    blocks <- model$blocks
    
    mm <- NULL
    lam <- NULL
    
    for (i in 1:length(blocks)){
        
        lam <- append(lam, paste("lam_", i, "_", 1:length(blocks[[i]]), sep=""))
        
        mm <- append(mm, paste(names(blocks)[i], " -> ", blocks[[names(blocks)[i]]], sep=""))
        
    }
    
    mm <- cbind(mm, lam)
    
    indx <- which(model$D!=0, arr.ind=TRUE)
    beta <- paste("beta_", indx[,1], "_", indx[,2], sep="")
    sm <- cbind(paste(model$strucmod[,1], " -> " ,model$strucmod[,2]), beta)
    
    result <- list(mm, sm)
    
    return(result)
}

#' Test if measurement model of a SEM is specified reflective. 
#' 
#' @param object    : Blocks of a sempls or plsm object.
#' @return Stops if model has formative block.
#' @examples
#' is_reflective(blocks)
is_reflective <- function(blocks){
    
    # check if measurement model is specified reflective
    for(i in seq_along(blocks)){
        
        if(attr(blocks[[i]], "mode") != "A"){
            
            # stop if block is of mode B (formative)
            stop("Formative models not supported yet")
        }
        
    }  
    
}

coeff_plot <- function(object){
    
    # get column names
    coeff.names <- colnames(object$t)
    
    # plot box
    plot(seq(-1, 1, by = 0.05), xaxt="n", yaxt="n", las = 1, type="n", 
         xlim = c(1,length(coeff.names)), xlab = "", ylab = "Estimate")
    
    # name axis
    axis(1, at = seq(coeff.names), labels = coeff.names, las = 2)
    axis(2, at = seq(-1, 1, by = 0.05), las = 1)
    
    # create grid
    abline(h = seq(-1, 1, by = 0.05), v = seq(coeff.names), col="black", lty="dotted")
    abline(h = 0, col="red")
    
    # get number of replications
    nmonte <- object$nmonte
    
    # draw simulated coefficients to graph
    for(i in 1:nmonte){
        
        points(object$t[i, ], col = "red", pch = 4, cex = 1.2)
    }
    
    # draw real coefficients to graph
    rep(points(object$t0, col = "black", pch = 5),2)
    
}




parallelplot.simsempls <- function(object, data, pattern="beta", subset=NULL, reflinesAt,
                                   col=c("grey", "darkred", "darkred", "black"),
                                   lty=c("solid", "solid", "dashed", "dotted"), ...)
{
    ifelse(is.null(subset), ind <- grep(pattern, colnames(object$t)), ind <- subset)
    
    p <- length(object$t[1, ])
    upper <- lower <- rep(0, p)
    
    for (i in 1:p){
        
        lower[i] <- min(object$t[, i])
        upper[i] <- max(object$t[, i])
    }
    
    Y <- rbind(object$t, object$t0, lower, upper, deparse.level=0)
    if(!missing(reflinesAt)){
        Y <- rbind(Y, matrix(rep(reflinesAt, each=ncol(object$t)),
                             nrow=length(reflinesAt), byrow=TRUE))
        origin <- c(rep("1resample", object$nmonte), "2sample", "3ci", "3ci",
                    rep("4reflines", times=length(reflinesAt)))
        Y <- data.frame(Y, origin)
    }
    else Y <- data.frame(Y, origin=c(rep("1resample", object$nmonte),
                                     "2sample", "3ci", "3ci"))
    parallelplot(~Y[ind], data=Y, groups=origin,
                 common.scale=TRUE, col=col, lty=lty, ...)
}

test_plsm <- function(object, scoeffs, mcoeffs, FUN){
    
    # test with plsm object
    assign(paste("test.",substitute(object), sep = ""), 
           simplsm(object, 100, scoeffs = scoeffs, mcoeffs = mcoeffs, FUN = "matrixpls.sempls"),
           envir = .GlobalEnv)
    
    # test with plsm object, matrixpls and high nmonte
    assign(paste("test.",substitute(object),".nmonte.high", sep = ""),
           simplsm(object, 1000, scoeffs = scoeffs, mcoeffs = mcoeffs, FUN = "matrixpls.sempls"),
           envir = .GlobalEnv)
    
    # test with few observations
    assign(paste("test.",substitute(object),".nobs.low", sep = ""), 
           simplsm(object, 100, nobs = 50, scoeffs = scoeffs, mcoeffs = mcoeffs, FUN = "matrixpls.sempls", maxit = 1000),
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