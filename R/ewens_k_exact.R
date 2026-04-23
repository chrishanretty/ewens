#' Calculate expected number of classes in a sample of size n given theta
#'
#' @param n The sample size
#' @param theta The non-negative parameter governing expected sample diversity
#' 
ewens_k_exact <- function(n, theta) {
    theta * sum(1 / (theta + seq(0, n - 1)))
}
