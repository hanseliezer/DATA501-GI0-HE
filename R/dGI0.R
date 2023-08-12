#' Probability density function of the GI0 distribution
#'
#' This function returns the density of point \eqn{x} on a GI0 distribution with parameters \eqn{\alpha}, \eqn{\gamma} and \eqn{Looks}.
#'
#' @param p_alpha Negative value that controls roughness
#' @param p_gamma Positive value that controls scale
#' @param p_Looks Positive value that controls number of looks
#'
#' @returns Density value (numeric)
#'
#' @keywords density distribution GI0
#' @export
#' @examples
#' dGI0(5, -2, 3, 4) # -> 0.0584

dGI0 <- function(x, p_alpha, p_gamma, p_Looks) {
  if (!all(sapply(c(x, p_alpha, p_gamma, p_Looks), is.numeric))) {
    stop("All arguments must be numeric.")
  }

  if (p_alpha >= 0) {
    stop("alpha must be negative.")
  } else if (p_gamma < 0) {
    stop("gamma must be positive.")
  } else if (p_Looks < 1) {
    stop("looks must be greater than or equal to 1.")
  }

  # df will return 0 for negative inputs, so there's not really a need to split
  # x into negatives and positives, but can be included to be really explicit
  x_neg <- x[x <= 0]
  x_pos <- x[x > 0]

  return(c(
    rep(0, length(x_neg)),
    df(-p_alpha * x_pos/p_gamma, df1=2*p_Looks, df2=-2*p_alpha)
  ))

}
