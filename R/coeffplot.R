CoeffPlot <- function(object, model){
     
    # get row names
    coeff.names <- row.names(coefficients(object[[1]]))
    
    # plot box
    plot(seq(-1, 1, by = 0.05), xaxt="n", yaxt="n", las = 1, type="n", 
         xlim = c(1,length(coeff.names)), xlab = "Coefficient", ylab = "Estimate")
    
    # name axis
    axis(1, at = seq(coeff.names), labels = coeff.names)
    axis(2, at = seq(-1, 1, by = 0.05), las = 1)
    
    # create grid
    abline(h = seq(-1, 1, by = 0.05), v = seq(coeff.names), col="black", lty="dotted")
    abline(h = 0, col="red")
    
    # draw simulated coefficients to graph
    for(i in seq_along(object)){
        
        points(object[[i]]$coefficients$Estimate, col = "red", pch = 4, cex = 1.2)
    }
    
    # draw real coefficients to graph
    rep(points(model$coefficients$Estimate, col = "black", pch = 5),2)
    
}



