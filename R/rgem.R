#' Draw from the Griffiths-Engen-McCloskey distribution
#'
#' @param alpha A parameter between zero and one
#' @param theta A parameter which must be greater than -alpha
#' @param trunc_at An integer which specifies the maximum number of components to return
#' @returns A vector of shares of length `trunc_at` which may sum to less than one
#'
#' @importFrom stats rbeta
#' @export
rgem <- function(alpha = 0, theta = 1, trunc_at = 500) {
    if (alpha < 0 | alpha > 1) {
        stop("Parameter alpha must be between zero and one")
    }
    if (theta < -alpha) {
        stop("Parameter theta must be greater than -alpha")
    }

    s <- numeric(trunc_at)
    remaining <- 1.0
    for (k in 1:trunc_at) {
        v_k <- stats::rbeta(1, 1 - alpha, theta + (k - 1) * alpha)
        s[k] <- remaining * v_k
        remaining <- remaining * (1 - v_k)
    }
    return(s)
}
