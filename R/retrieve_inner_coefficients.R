retrieve_inner_coefficients <- function(path_coefficients, sm){
    
    # ---- retrieve lambda coefficients ----
    library("nleqslv")
    
    x <- cor(x1,x2)
    y <- cor(x1,x3)
    z <- cor(x2,x3)
    
    fun <- function(x) { 
        f <- numeric(length(x))                 	            # read as:
        f[1] <-  (1+x[1]+x[2])/sqrt(3+2*(x[1]+x[2]+x[3]))-0.8	# 0 = 2z - 2y + 3x + 4
        f[2] <-  (1+x[1]+x[3])/sqrt(3+2*(x[1]+x[2]+x[3]))-0.75	# 0 = 3z + 4y - 3x + 3
        f[3] <-  (1+x[2]+x[3])/sqrt(3+2*(x[1]+x[2]+x[3]))-0.7	# 0 = -z + 2y + 5x + 1
        f 
    } 
    startx <- c(0,0,0) # start the answer search here
    answers <- as.data.frame(nleqslv(startx,fun))  # answers["x"] = x,y,z are the solutions closest to startx if there are multiple solutions
    
    library("MASS")
    
    sig <- rbind(c(1, -0.14, 0.04),
                 c(-0.14, 1, 0.22),
                 c(0.04, 0.22, 1))
    
    colnames(sig) <- c("x1", "x2", "x3")
    rownames(sig) <- colnames(sig)
    
    X <- mvrnorm(100, rep(0, ncol(sig)), sig, empirical = TRUE)
    
    x1 <- X[, 1]
    x2 <- X[, 2]
    x3 <- X[, 3]
    
    y <- x1+x2+x3
    
    (1+cor(x1,x2)+cor(x1,x3))/(sqrt(var(x1))*sqrt(var(x1)+var(x2)+var(x3)+2*cor(x1,x2)+2*cor(x1,x3)+2*cor(x2,x3)))
    
    denom <- sqrt(choose(ncol(X), 2)+2*(cor(x1,x2)+cor(x1,x3)+cor(x2,x3)))
    (1+cor(x1,x2)+cor(x1,x3))/denom
    cor(x1, y)
    
    (1+cor(x2,x1)+cor(x2,x3))/denom
    cor(x2, y)
    
    (1+cor(x3,x1)+cor(x3,x2))/denom
    cor(x3, y)
    
    # general formula
    
    # TODO adjust outer.cor retrieval for full model
    outer.cor <- sig[lower.tri(sig)]
    
    # TODO change ncol(X)
    nmanifest <- ncol(X)
    
    denom <- sqrt(choose(nmanifest, 2)+2*(sum(outer.cor)))
    
    # should be
    # denom <- sqrt(nmanifest+2*(sum(outer.cor)))
    
    lam <- numeric(nmanifest)
    
    for(i in 1:nmanifest){
        
        lam[i] <- (1+sum(sig[, i][sig[, i]!=1]))/denom
    }
    
    x <- cor(x1,x2)
    y <- cor(x1,x3)
    z <- cor(x2,x3)
    
    0.5 = (1+x+y)/sqrt(3+2*(x+y+z))
    0.6 = (1+x+z)/sqrt(3+2*(x+y+z))
    0.7 = (1+y+z)/sqrt(3+2*(x+y+z))
}