coeff_plot <- function(object, model){
    
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
    
    # draw simulated coefficients to graph
    for(i in seq_along(coeff.names)){
        
        points(object$t[i, ], col = "red", pch = 4, cex = 1.2)
    }
    
    # draw real coefficients to graph
    rep(points(object$t0, col = "black", pch = 5),2)
    
}




parallelplot.sempls <- function(object, data, pattern="beta", subset=NULL, reflinesAt,
                                    col=c("grey", "darkred", "darkred", "black"),
                                    lty=c("solid", "solid", "dashed", "dotted"), ...)
{
    ifelse(is.null(subset), ind <- grep(pattern, colnames(object$t)), ind <- subset)
    
    p <- length(testplsm$t[1, ])
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


parallelplot.plsm <- function(object, data, pattern="beta", subset=NULL, reflinesAt,
                                   col=c("grey", "darkred", "black"),
                                   lty=c("solid", "dashed", "dotted"), ...)
{
    ifelse(is.null(subset), ind <- grep(pattern, colnames(object$t)), ind <- subset)
    
    p <- length(testplsm$t[1, ])
    upper <- lower <- rep(0, p)
    
    for (i in 1:p){
        
        lower[i] <- min(object$t[, i])
        upper[i] <- max(object$t[, i])
    }
    
    Y <- rbind(object$t, lower, upper, deparse.level=0)
    if(!missing(reflinesAt)){
        Y <- rbind(Y, matrix(rep(reflinesAt, each=ncol(object$t)),
                             nrow=length(reflinesAt), byrow=TRUE))
        origin <- c(rep("1resample", object$nmonte), "3ci", "3ci",
                    rep("4reflines", times=length(reflinesAt)))
        Y <- data.frame(Y, origin)
    }
    else Y <- data.frame(Y, origin=c(rep("1resample", object$nmonte),
                                      "3ci", "3ci"))
    parallelplot(~Y[ind], data=Y, groups=origin,
                 common.scale=TRUE, col=col, lty=lty, ...)
}

