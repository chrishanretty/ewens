#' Probability mass function for the Ewens distribution
#'
#' @param x A vector giving class memberships of each observation in the sample
#' @param theta A non-negative parameter governing the expected sample diversity.
#' @param log if TRUE, probabilities are given as log(p)
#' @examples
#' x <- sample(LETTERS, 120, replace = TRUE)
#' dewens(x, theta = 1)
#' dewens(x, theta = 0) ## returns NaN since vector incompatible with zero diversity
#'
#' @export
dewens <- function(x, theta = 1, log = FALSE) {
    if (theta < 0) {
        stop("Parameter theta must be non-negative. ")
    }
    ### Get counts
    tab <- tabulate(factor(x))
    mvec   <- tabulate(tab, nbins = max(tab))
    n      <- length(x)
    j_seq  <- seq_along(mvec)

    log_pmf <- lfactorial(n) -
        sum(log(theta + seq_len(n) - 1)) +   # log rising factorial
        sum(mvec * log(theta)                  # theta^{a_j}
            - mvec * log(j_seq)               # j^{a_j}
            - lfactorial(mvec))               # a_j!

    if (log) {
        return(log_pmf)
    } else {
        return(exp(log_pmf))
    }
}
