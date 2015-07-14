retrieve_outer_coefficients <- function(outer_loadings, mm){
    
    if(!(require("nleqslv"))){
        
        stop("The package 'nleqslv' is required, type: install.packages('nleqslv')")
    }
    
    # denominator
    denom <- sqrt(nmanifest[lv]+2*(sum(outer.cor)))
    # nominator
    nom <- 1+sum(sub.sig[mv,][names(sub.sig[mv,])!=mv])
    
    nom/denom
    
    fun <- function(x) { 
        f <- numeric(length(x))             		            # read as:
        f[1] <-  (1+x[1]+x[2])/sqrt(3+2*(x[1]+x[2]+x[3]))-0.8	# 0 = 2z - 2y + 3x + 4
        f[2] <-  (1+x[1]+x[3])/sqrt(3+2*(x[1]+x[2]+x[3]))-0.75	# 0 = 3z + 4y - 3x + 3
        f[3] <-  (1+x[2]+x[3])/sqrt(3+2*(x[1]+x[2]+x[3]))-0.7	# 0 = -z + 2y + 5x + 1
        f 
    } 
    startx <- c(0,0,0) # start the answer search here
    answers <- as.data.frame(nleqslv(startx,fun))  # answers["x"] = x,y,z are the solutions closest to startx if there are multiple solutions

}