#' Test for classification in one of two groups.
#' 
#' The null hypotheses is that the new data is not well classified in the first group and the 
#' alternative hypotheses is that the data is well classified in the first group.
#' 
#' @param x A numeric vector to be classified.
#' @param data A data.frame. Each row must be an observation and each column a dimension.
#'   The last column must be the group factor with only two groups.
#' @param groups A factor or character vector. It must contain only two elements: the first and second group names, respectively.
#' @param numB Numeric scalar. The number of bootstraps. It's recommended 
#'   \eqn{1000 < numB < 10000}.
#' @return p-value based on bootstraps.
#' @examples
#' # Example 1
#' # Five observations from each group, G1 and G2. Each observation has 60 dimensions.
#' x <- matrix(c(rnorm(300, 0), rnorm(300, 10)), nrow = 60)
#' data <- data.frame(t(x))
#' data['group'] <- factor(rep(c('group1', 'group2'), each = 5))
#' # Test data comes from G1.
#' x <- rnorm(60, 0)
#' # The test correctly indicates that the test data should be classified in G1 (p < 0.05).
#' utest_classify(x, data, groups = c("group1", "group2"))
#' 
#' # Example 2
#' # Five observations from each group, G1 and G2. Each observation has 60 dimensions.
#' x <- matrix(c(rnorm(300, 0), rnorm(300, 10)), nrow = 60)
#' data <- data.frame(t(x))
#' data['group'] <- factor(rep(c('group1', 'group2'), each = 5))
#' # Test data comes from G2.
#' x <- rnorm(60, 10)
#' # The test correctly indicates that the test data should be classified in G2 (p > 0.05).
#' utest_classify(x, data, groups = c("group1", "group2"))
#' @export
utest_classify <- function(x, data, groups, bootstrap_iter = 1000)
{
    group_col <- ncol(data)
    n_groups <- table(data[group_col])
    ng1 <- unname(n_groups[groups[1]])
    ng2 <- unname(n_groups[groups[2]])
    
    data <- data[order(data[group_col]), -group_col]
    data <- rbind(data[1:ng1, ], x, data[(ng1+1):nrow(data), ])
    distances <- as.matrix(dist(data))
    B <- rep(0, bootstrap_iter)
    B1_0 <- Bn(c(ng1 + 1, ng2), distances)
    B2_0 <- Bn(c(ng1, 1 + ng2), distances)
    
    D = B1_0 - B2_0
    
    for (i in 1:bootstrap_iter)
    {
        vaux1 <- floor(runif(ng1, 1, ng1 + 1))
        vaux2 <- floor(runif(ng2, ng1 + 1, ng2 + ng1 + 2))
        vaux3 <- floor(runif(1, ng1 + 1, ng2 + ng1 + 2))
        mataux <- distances[c(vaux1, vaux3, vaux2), c(vaux1, vaux3, vaux2)]
        ngv1 <- c(ng1 + 1, ng2)
        B1 <- Bn(ngv1, mataux)
        ngv2 <- c(ng1, ng2 + 1)
        B2 <- Bn(ngv2, mataux)
        B[i] <- B1 - B2
    }
    
    utest_classify_obj <- list(p_value = mean(B > D),
                            groups = groups,
                            bootstrap_iter = bootstrap_iter)
    
    class(utest_classify_obj) <- "utest_classify"
    
    return(utest_classify_obj)
}

#' Simple print method for utest_classify objects.
#' @export
print.utest_classify <- function(obj, ...) {
    cat("\n")
    cat("U test for classification\n\n")
    cat("p-value:", obj$p_value, "\n")
    cat("Groups:", obj$groups, "\n")
    cat("Bootstrap iterations:", obj$bootstrap_iter, "\n")
    cat("\n")
    
    invisible(obj)
}
