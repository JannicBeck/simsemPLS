matrix2latex <- function(matr) {
    
    printmrow <- function(x) {
        
        cat(cat(x,sep=" & "),"\\\\ \n")
    }
    
    cat("\\begin{bmatrix}","\n")
    body <- apply(matr,1,printmrow)
    cat("\\end{bmatrix}")
}