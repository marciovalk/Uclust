#' Test for classification in one of two groups.
#' 
#' The null hypotheses is that the new data is not well classified and the 
#' alternative hypotheses is that the data is well classified.
#' 
#' The data in \code{md} must be in order: the first \code{n1} observations must
#' belong to group 1, then the new observation to be classified and the next 
#' \code{n2} observations must belong to group 2.
#' 
#' @param ngv A numeric vector \code{c(n1, n2)} where \code{n1} is the number of
#'   elements in group 1 and \code{n2} is the number of elements in group 2.
#' @param numB Numeric scalar. The number of bootstraps. It's recommended 
#'   \eqn{1000 < numB < 10000}.
#' @param md A \eqn{(n+1) x (n+1)} matrix of distances where \eqn{n = n1 + n2}.
#' @return p-value based on bootstraps.
#' @examples
#' # Example 1
#' # x is a 60 x 11 matrix.
#' x <- matrix(c(rnorm(300, 0), rnorm(360, 10)), nrow = 60)
#' # Five columns with mean zero and six with mean 10.
#' md <- as.matrix(dist(t(x)))
#' ngv <- c(5, 5)
#' # H0: column 6 is not significantly classified in group 1 (columns 1, 2, 3, 4, 5) (TRUE p-value must be >0.05)
#' Uteste_class(ngv, 1000, md)
#' 
#' # Example 2
#' # x is a 60 x 11 matrix.
#' x <- matrix(c(rnorm(360, 0), rnorm(300, 10)), nrow = 60)
#' # Six columns with mean zero and five with mean 10.
#' md <- as.matrix(dist(t(x)))
#' ngv <- c(5, 5)
#' # H0: column 6 is not significantly classified in group 1 (columns 1, 2, 3, 4, 5) (FALSE p-value must be >0.05)
#' Uteste_class(ngv, 1000, md)
Uteste_class <- function(ngv, numB, md)
{
    ng1 <- ngv[1]
    ng2 <- ngv[2]
    if (dim(md)[1] != (ng1 + ng2 + 1)) {
        print(
            "md must be (n+1 x n+1) matrix.
            If n1 is the size of group 1, n2 is the size of group 2
            and X is the element to be classified, md must be calculated
            putting the data in order that the n1 elents of group 1 go first
            than X  and than n2 elements of group 2"
        )
    }
    
    B <- rep(0, numB)
    B1_0 <- Bn(c(ng1 + 1, ng2), md)
    B2_0 <- Bn(c(ng1, 1 + ng2), md)
    D = B1_0 - B2_0
    
    for (i in 1:numB)
    {
        vaux1 <- floor(runif(ng1, 1, ng1 + 1))
        vaux2 <- floor(runif(ng2, ng1 + 1, ng2 + ng1 + 2))
        vaux3 <- floor(runif(1, ng1 + 1, ng2 + ng1 + 2))
        mataux <- md[c(vaux1, vaux3, vaux2), c(vaux1, vaux3, vaux2)]
        ngv1 <- c(ng1 + 1, ng2)
        B1 <- Bn(ngv1, mataux)
        ngv2 <- c(ng1, ng2 + 1)
        B2 <- Bn(ngv2, mataux)
        B[i] <- B1 - B2
    }
    
    mean(B > D)
}
