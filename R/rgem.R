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
  if (alpha < 0 || alpha > 1) stop("alpha must be in [0,1]")
  if (theta < -alpha) stop("theta must be >= -alpha")
  
  k <- seq_len(trunc_at)
  v <- stats::rbeta(trunc_at, 1 - alpha, theta + k * alpha)
  
  remaining <- cumprod(c(1, 1 - v[-trunc_at]))
  s <- remaining * v
  
  s
}
