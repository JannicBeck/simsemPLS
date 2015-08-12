dist_plot <- function(object){

    if(!(require("ggplot2"))){
        
        stop("The package 'ggplot2' is required, type: install.packages('ggplot2')")
    }
    
    if(!(require("reshape2"))){
        
        stop("The package 'reshape2' is required, type: install.packages('reshape2')")
    }
    
    
    model <- object$model
    latent.vars <- colnames(model$M)
    mm <- model$measuremod
    sm <- model$strucmod
    
    for(lv in latent.vars){
        
        lv.lam <- get_equations(model)[[1]][, "lam"][which(mm == lv)]
        lv.lam.melt <- melt(object$t[, lv.lam])
        print(ggplot(aes(x=value, colour=Var2), data=lv.lam.melt) + geom_density())
    }

    beta <- get_equations(model)[[2]][, "beta"]
    beta.melt <- melt(object$t[, beta])
    
    if(length(beta) == 1){
        print(ggplot(aes(x=value, colour=beta), data=beta.melt) + geom_density())
    }else{
        print(ggplot(aes(x=value, colour=Var2), data=beta.melt) + geom_density())
    }
    
    
}
