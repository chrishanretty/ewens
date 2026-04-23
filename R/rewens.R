#' Draw from the Ewens distribution
#' 
#' @param n The sample size.
#' @param theta A non-negative parameter governing the expected sample diversity.
#' @returns A vector of length n consisting of numeric class labels.
#' @examples
#' rewens(100, 1)
#' rewens(120, 0.5)
#' rewens(10, 0)
#' @export
rewens <- function(n, theta = 1) {
    if (theta < 0) {
        stop("Parameter theta must be non-negative. ")
    }
    if (theta == 0) {
        return(rep(1, n))
    }
    
    ## Pre-allocate
    assignment <- integer(n)
    table_sizes <- integer(n) ## over-allocate
    n_tables <- 0L
    
    for (i in seq_len(n)) {
        probs <- c(table_sizes[seq_len(n_tables)], theta) / (theta + i - 1)
        choice <- sample(n_tables + 1L, 1, prob = probs)
        
        if (choice > n_tables) {
            n_tables <- n_tables + 1L
            table_sizes[n_tables] <- 1L
        } else {
            table_sizes[choice] <- table_sizes[choice] + 1L
        }
        assignment[i] <- choice
    }
    assignment
}
