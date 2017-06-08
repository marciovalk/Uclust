#' Bootstrap to find the variance of \code{Bn}.
#' 
#' @return The variance of \code{Bn}.
boot_sigma <- function(ngv, numB, md)
{
    ng1 <- ngv[1]
    ng2 <- ngv[2]
    n <- sum(ngv)
    B <- rep(0, numB)
    
    for (i in 1:numB)
    {
        vaux1 <- floor(runif(n, 1, n + 1))
        mataux <- md[vaux1, vaux1]
        ngv1 <- c(ng1, ng2)
        B[i] <- Bn(ngv1, mataux) # Bn.
    }
    
    return(var(B))
}
