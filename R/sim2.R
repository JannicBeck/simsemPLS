#' Simulate data on given plsm object.
#' 
#' To set up the covariance matrices type \code{scoeffs <- object$D; data.entry(object$D)} for the structural model
#' and \code{mcoeffs <- object$M; data.entry(mcoeffs)} for the measurement model.
#' Note that the correlation of a latent variable measured by exactly 1 manifest variable is by definition 1.
#' 
#' @param object            : An object of class plsm as returned by the method plsm.
#' @param nmonte            : The number of monte carlo replications; the default is 100.
#' @param nobs              : The number of observations; the default is 250.
#' @param scoeffs           : A covariance matrix of latent variable scores.
#' @param mcoeffs           : A correlation matrix of manifest variables on latent variables.
#' @return An object of class simsempls.
#' @examples
#' simplsm(object, nmonte = 10, scoeffs = scoeffs, mcoeffs = mcoeffs)
#'        
simplsm <- function(object, nmonte = 100, nobs = 250, scoeffs = NULL, mcoeffs = NULL, FUN = "sempls", ...) {
    
    
    # check if object is of type plsm    
    if(is(object, "plsm")){        
        
        if(!(is.matrix(scoeffs)) || !(is.matrix(mcoeffs))){
            
            stop("Covariance matrices must be supplied!")
        }
        
        # get measurement blocks
        blocks <- object$blocks
        
        # check if measurement model is specified reflective
        is_reflective(blocks)
        
    }else{
        
        # object is not of type plsm
        stop("The supplied object must be of class plsm")
    }
    
    # require MASS package
    if(!(require(MASS))){
        
        stop("The package 'MASS' is required, type: install.packages(MASS)")
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
    
    result <- core_2(object, nmonte, nobs, scoeffs, mcoeffs, FUN, ...)
    
    return(result)
}





#' Core function 2, actual simulating is done here.
#' 
core_2 <- function(object, nmonte, nobs, scoeffs, mcoeffs, FUN, ...) { 
    
    # get number of equations
    neq <- length(c(object$strucmod[, 2], object$measuremod[, 2]))
    
    # initialize simulated coefficient matrix
    t <- matrix(numeric(0), nrow = nmonte, ncol = neq)
    
    # get equations
    equations <- get_equations(object)
    
    # set path attribute
    attr(t, "path") <- c(equations[[1]][, 1], equations[[2]][, 1])
    
    # set column names
    colnames(t) <- c(equations[[1]][, 2], equations[[2]][, 2]) 
    
    # initialize list of datasets
    data <- vector("list", nmonte)
    
    # from correlation matrix to vector
    mcoeffs <- mcoeffs[mcoeffs != 0]
    
    # get structural coefficients vector
    # old method
    scoeffs.t0 <- scoeffs * object$D
    scoeffs.t0 <- scoeffs.t0[scoeffs.t0 != 0]
    
    # new method not working
    # scoeffs.t0 <- scoeffs[scoeffs != 0]
    
    # TODO: Implement getcoeffs from covariance matrix
    # set actual coefficients to provided
    t0 <- c(mcoeffs, scoeffs.t0)
    
    # specify the means of the latent variables
    smeans <- rep(0, nrow(scoeffs))
    
    # specify the means of the latent variables
    mmeans <- rep(0, length(mcoeffs))
    
    sim.data <- matrix(numeric(), nobs, length(mcoeffs))
    
    colnames(sim.data) <- object$manifest
    
    times <- colSums(object$M)
    
    fnames <- rep(names(times), times)
    
    cat("Simulate: ")
    
    for(i in 1:nmonte){
        
        # printing out the iteration
        treil <- paste(rep(" ", floor(log10(nmonte)) - floor(log10(i))), collapse="")
        ndel <- paste(rep("\b", floor(log10(nmonte)) + 1), collapse="")
        if(i==1) cat(paste(treil, i, sep=""))
        if(i!=nmonte) cat(paste(ndel, treil, i, sep=""))
        else cat(paste(ndel, i, " Done.\n", sep=""))
        
        
        
        # randomly simulate the latent variable scores with given covariance matrix
        # If empirical = FALSE, the correlations will be approx.
        fscores <- mvrnorm(nobs, Sigma = scoeffs, mu = smeans, empirical = FALSE)
        
        for(j in seq_along(mcoeffs)){
            
            # empirical
            #sim.data[, j] <- sim_data(mcoeffs[j], nobs, fscores[, fnames[j]])
            # population
            sim.data[, j] <- sim_data_pop(mcoeffs[j], nobs, fscores[, fnames[j]])
        }
        
        # estimate model with simulated data
        try(sim.model <- FUN(object, sim.data, verbose = FALSE, ...), silent = TRUE)
        
        # extract coefficients
        t[i, ] <- sim.model$coefficients[, 2]
        
        # save simulated data in list
        data[[i]] <- sim.data
        
    }
    
    # initialize and assign result list
    result <- list(t0 = t0, t = t, nobs = nobs, nmonte = nmonte, data = data, model = object)
    
    return(result)
}


# returns a data frame of two variables which correlate with a population correlation of rho
# If desired, one of both variables can be fixed to an existing variable by specifying x
sim_data_pop <- function(rho, nobs, fscore) {
    
    if(rho == 1){
        
        result <- fscore
    }else{
        
        C <- matrix(rho, nrow = 2, ncol = 2)
        diag(C) <- 1
        
        C <- chol(C)
        
        X1 <- rnorm(nobs)
        X <- cbind(fscore, X1)
        
        # induce correlation (does not change X1)
        df <- X %*% C
        
        result <- df[, 2]
        
        ## if desired: check results
        #all.equal(fscore, X[,1])
        #cor(result)
        
        
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