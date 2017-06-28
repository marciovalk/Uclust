#' The test statistic \code{Bn}.
#' 
#' A measure of distances between and within groups.
#' 
#' @param ngv A numeric vector \code{c(n1, n2)} where \code{n1} is the number of
#'   elements in group 1 and \code{n2} is the number of elements in group 2.
#' @param distances A n x n matrix of distances where \eqn{n = n1 + n2}.
#' @return A value for the statistic \code{Bn}.
Bn <- function(ngv, distances)
{
    ng <- sum(ngv)
    
    # Total distance between groups.
    dist_between_groups <- 0.0
    for (i in 1:ngv[1])
    {
        for (j in (ngv[1] + 1):ng)
        {
            dist_between_groups <- dist_between_groups + distances[i, j]
        }
    }
    
    # Total distance within group 1.
    dist_within_group1 <- 0.0
    for (i in 1:(ngv[1] - 1))
    {
        for (j in (i + 1):ngv[1])
        {
            dist_within_group1 <- dist_within_group1 + distances[i, j]
        }
    }
    
    # Total distance within group 2.
    dist_within_group2 <- 0.0
    for (i in (ngv[1] + 1):(ng - 1))
    {
        for (j in (i + 1):ng)
        {
            dist_within_group2 <- dist_within_group2 + distances[i, j]
        }
    }
    
    a1 <- (1 / (ngv[1] * ngv[2])) * dist_between_groups
    a2 <- (2 / (ngv[1] * (ngv[1] - 1))) * dist_within_group1
    a3 <- (2 / (ngv[2] * (ngv[2] - 1))) * dist_within_group2
    Bn <- (ngv[1] * ngv[2] / (ng * (ng - 1))) * (2 * a1 - a2 - a3)
    
    return(Bn)
}
