#' Maximum likelihood estimate of theta given sample vector with class memberships
#'
#' @param x A vector containing class memberships; sample size n and number of classes k are calculated from this
#' 
#' @importFrom stats uniroot
ewens_mle <- function(x) {
    n <- length(x)
    K <- length(unique(x))
    score <- function(theta) K/theta - sum(1/(theta + seq(0, n-1)))
    
### Score is decreasing in theta, so bracket appropriately
    stats::uniroot(score, interval = c(1e-6, n))$root
}
