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
        
        points(object$t[i, ], col = "#585858", pch = 4, cex = 1.2)
    }
    
    # draw real coefficients to graph
    rep(points(object$t0, col = "green", pch = 5, cex = 1.5, lwd=2),2)
    
    # draw mean coefficients to graph
    rep(points(colMeans(object$t), col = "red", pch = 5, lwd=2),2)
    
}
