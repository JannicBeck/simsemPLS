seiler.models <- MonteSempls(seiler_model, 10)

pb.models <- MonteSempls(PB_model, 100)

final.models <- MonteSempls(final_model, 10)

ecsi.models <- MonteSempls(ecsi_model, 100)

ecsi.boot <- bootsempls(ecsi_model, nboot = 1000)

parallelplot(ecsi.boot, subset = 1:ncol(ecsi.boot$t), reflinesAt = c(-1,0,1))

CoeffPlot <- function(monte.models, model){
    
    # create new plot
    plot.new()
    
    # get row names
    coeff.names <- row.names(monte.models[[1]]$coefficients)
    
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
    for(i in seq_along(monte.models)){
        
        points(monte.models[[i]]$coefficients$Estimate, col = "red", pch = 4, cex = 1.2)
    }
    
    # draw real coefficients to graph
    rep(points(model$coefficients$Estimate, col = "black", pch = 5),2)
    
}

CoeffPlot(seiler.models, seiler_model)

CoeffPlot(pb.models, PB_model)

CoeffPlot(final.models, final_model)

CoeffPlot(ecsi.models, ecsi_model)

coefficients <- 0

for(i in seq_along(monte.models)){
    
    coefficients[i] <- monte.models[[i]]$coefficients$Estimate[[2]]
    

}

densityplot(coefficients)

