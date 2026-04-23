#' Probability mass function for the number of classes from a Ewens distribution
#'
#' @param k An integer number of classes at which to evaluate the PMF
#' @param n A sample size not less than k
#' @param theta A non-negative parameter governing the expected sample diversity.
#' 
#' @examples
#' x <- sample(LETTERS, 120, replace = TRUE)
#' dewens_k(1, 20, theta = 1)
#'
#' @export
dewens_k <- function(k, n, theta) {
    abs(copula::Stirling1(n, k)) * (theta ^ k) /
        exp(sum(log(theta + seq_len(n) - 1)))
}
