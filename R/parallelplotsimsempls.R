#TODO require semPLS package for parallelplot function
parallelplot_simsempls <- function(object, data, pattern="beta", subset=NULL, reflinesAt,
                                    col=c("grey", "darkred", "darkred", "black"),
                                    lty=c("solid", "solid", "dashed", "dotted"), ...){
    
    library("semPLS")
    
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

