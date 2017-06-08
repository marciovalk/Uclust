objstdBn <- function(assign, varBn, md) {
    n <- length(assign)
    n1 <- sum(assign == 0)
    n2 <- n - n1
    if (n1 == 1 | n2 == 1) {
        return(Inf)
    }
    else {
        vaux1 <- which(assign == assign[1])
        vaux2 <- c(1:n)[-vaux1]
        m <- md[c(vaux1, vaux2), c(vaux1, vaux2)]
        Bns <- Bn(c(n1, n2), m)
        
        return(-Bns/sqrt(varBn[n1]))
    }
}
