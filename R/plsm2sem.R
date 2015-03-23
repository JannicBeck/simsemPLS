plsm2sem <- function(model){

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
