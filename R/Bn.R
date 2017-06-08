#' The test statistic \code{Bn}.
#' 
#' A measure of distances between and within groups.
#' 
#' @param ngv A numeric vector \code{c(n1, n2)} where \code{n1} is the number of
#'   elements in group 1 and \code{n2} is the number of elements in group 2.
#' @param md A n x n matrix of distances where \eqn{n = n1 + n2}.
#' @return A value for the statistic \code{Bn}.
Bn <- function(ngv, md)
{
    ng <- sum(ngv)
    maux1 <- matrix(0, nrow = ng, ncol = ng)
    for (i in 1:ngv[1])
    {
        for (j in (ngv[1] + 1):ng)
        # Distance between groups.
        {
            maux1[i, j] <- md[i, j]
        }
    }
    
    maux2 <- matrix(0, ngv[1], ngv[1])
    for (i in 1:(ngv[1] - 1))
    {
        for (j in (i + 1):ngv[1])
        {
            maux2[i, j] <- md[i, j] # Distance within group 1.
        }
    }
    
    maux3 <- matrix(0, ng, ng)
    for (i in (ngv[1] + 1):(ng - 1))
    {
        for (j in (i + 1):ng)
        {
            maux3[i, j] <- md[i, j] # Distance within group 2.
        }
    }
    
    a1 <- (1 / (ngv[1] * ngv[2])) * sum(maux1)
    a2 <- (2 / (ngv[1] * (ngv[1] - 1))) * sum(maux2)
    a3 <- (2 / (ngv[2] * (ngv[2] - 1))) * sum(maux3)
    Bn <- (ngv[1] * ngv[2] / (ng * (ng - 1))) * (2 * a1 - a2 - a3)
    
    Bn
}
