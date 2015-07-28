retrieve_inner_coefficients <- function(path_coefficients, sm){
    
    if(!(require("nleqslv"))){
        
        stop("The package 'nleqslv' is required, type: install.packages('nleqslv')")
    }

    fun <- function(x) { 
        f <- numeric(length(x))                 	            # read as:
        f[1] <-  (1+x[1]+x[2])/sqrt(3+2*(x[1]+x[2]+x[3]))-0.8	# 0 = 2z - 2y + 3x + 4
        f[2] <-  (1+x[1]+x[3])/sqrt(3+2*(x[1]+x[2]+x[3]))-0.75	# 0 = 3z + 4y - 3x + 3
        f[3] <-  (1+x[2]+x[3])/sqrt(3+2*(x[1]+x[2]+x[3]))-0.7	# 0 = -z + 2y + 5x + 1
        f 
    } 
    startx <- c(0,0,0) # start the answer search here
    answers <- as.data.frame(nleqslv(startx,fun))  # answers["x"] = x,y,z are the solutions closest to startx if there are multiple solutions
    
}


fun <- function(x) { 
    f <- numeric(length(x))                                 
    f[1] <-  ((((x[1]+x[2]+x[3]+x[4]+x[5]+x[6])/(sqrt(3)*sqrt(5.0625)))-
                   ((x[7]+x[8]+x[9]+x[10])/(sqrt(3)*sqrt(2.56)))*
                   ((x[11]+x[12]+x[13]+x[14]+x[15]+x[16])/(sqrt(5.0625)*sqrt(2.56))))/
                  (1-((x[11]+x[12]+x[13]+x[14]+x[15]+x[16])/(sqrt(5.0625)*sqrt(2.56)))^2))-0.238157
    f[2] <-  ((((x[7]+x[8]+x[9]+x[10])/(sqrt(3)*sqrt(2.56)))-
                   ((x[1]+x[2]+x[3]+x[4]+x[5]+x[6])/(sqrt(3)*sqrt(5.0625)))*
                   ((x[11]+x[12]+x[13]+x[14]+x[15]+x[16])/(sqrt(5.0625)*sqrt(2.56))))/
                  (1-((x[11]+x[12]+x[13]+x[14]+x[15]+x[16])/(sqrt(5.0625)*sqrt(2.56)))^2))-0.2092895
    f 
} 
startx <- rep(1,16) # start the answer search here
answers <- as.data.frame(nleqslv(startx,fun))  # answers["x"] = x,y,z are the solutions closest to startx if there are multiple solutions

x <- rep(0.2,16)



((((x[1]+x[2]+x[3]+x[4]+x[5]+x[6])/(sqrt(3)*sqrt(5.0625)))-
      ((x[7]+x[8]+x[9]+x[10])/(sqrt(3)*sqrt(2.56)))*
      ((x[11]+x[12]+x[13]+x[14]+x[15]+x[16])/(sqrt(5.0625)*sqrt(2.56))))/
     (1-((x[11]+x[12]+x[13]+x[14]+x[15]+x[16])/(sqrt(5.0625)*sqrt(2.56)))^2))





(
    (
        (
            (
                x[1]+x[2]+x[3]+x[4]+x[5]+x[6]
            )
            /
            (
                sqrt(3)*sqrt(5.0625)
            )
        )
        -
        (
            (
                x[7]+x[8]+x[9]+x[10]
            )
            /
            (
                sqrt(3)*sqrt(2.56)
            )
        )
        *
        (
            (
                x[11]+x[12]+x[13]+x[14]+x[15]+x[16]
            )
            /
            (
                sqrt(5.0625)*sqrt(2.56)
            )
        )
    )
    / # großer Bruchstrich 
    (
        1-(
            (
                x[11]+x[12]+x[13]+x[14]+x[15]+x[16]
            )
            /
            (
                sqrt(5.0625)*sqrt(2.56)
            )
        )^2
    )
 )


r12 <- (0.2*4)/(sqrt(3)*sqrt(2.56))
cor(y1,y2)

r13 <- (0.2*6)/(sqrt(3)*sqrt(5.0625))
cor(y1,y3)

r23 <- (0.2*6)/(sqrt(2.56)*sqrt(5.0625))
cor(y2,y3)

(r12-r13*r23)/(1-r23^2)
(r13-r12*r23)/(1-r23^2)


fun <- function(x) { 
    f <- numeric(length(x))                                 
    f[1] <-  ((x[1]-x[2]*0.1)/(1-0.1^2))-0.3
    f[2] <-  ((x[2]-x[1]*0.1)/(1-0.1^2))-0.2
    f 
} 
startx <- rep(0,2) # start the answer search here
answers <- as.data.frame(nleqslv(startx,fun))  # answers["x"] = x,y,z are the solutions closest to startx if there are multiple solutions











