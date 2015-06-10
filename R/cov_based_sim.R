# ---- Specify Models ----

# ---- ECSI ----
# load package
if(!require("semPLS")){
    
    install.packages("semPLS")
}

# get data
data(ECSImobi)

# rename model
ecsi.plsm <- ECSImobi

# rename dataframe
ecsi.data <- mobi

# remove unused objects
rm("ECSImm", "ECSIsm", "ECSImobi", "mobi")

# estimate model
ecsi.sempls <- sempls(ecsi.plsm, ecsi.data)

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

sigma <- cor(ecsi.data)
sigma
sigma[1,11] <- 0.8
sigma[11,1] <- 0.8

if(!require("MASS")){
    
    install.packages("MASS")
}

nmonte <- 100

# get equations
equations <- get_equations(ecsi.plsm)

neq <- length(c(ecsi.plsm$strucmod[, 2], ecsi.plsm$measuremod[, 2]))

t <- matrix(numeric(0), nrow = nmonte, ncol = neq)
# set column names
colnames(t) <- c(equations[[1]][, 2], equations[[2]][, 2])

t0 <- ecsi.sempls$coefficients$Estimate

for(i in 1:nmonte){
    
    sim.data <- mvrnorm(1000, rep(0, 24), sigma, empirical = TRUE)
    sim.model <- sempls(ecsi.plsm, sim.data) 
    
    t[i, ] <- sim.model$coefficients[, 2]
}

coeff_plot <- function(){
    
    # get column names
    coeff.names <- colnames(t)
    
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
    for(i in 1:nmonte){
        
        points(t[i, ], col = "red", pch = 4, cex = 1.2)
    }
    
    # draw real coefficients to graph
    rep(points(t0, col = "black", pch = 5),2)
    
}

coeff_plot()
