#' Cumulative distribution function of the GI0 distribution
#'
#' This function returns the cumulative probability of point \eqn{x} on a GI0 distribution with parameters \eqn{\alpha}, \eqn{\gamma}
#' and \eqn{Looks}.
#'
#' @param p_alpha Required: negative value that controls roughness
#' @param p_gamma Required: positive value that controls scale
#' @param p_Looks Required: positive value greater or equal to 1 that controls number of looks
#' @param lower.tail Logical. If `TRUE`, probabilities are \eqn{P(X \leq x)}; otherwise, \eqn{P(X > x)}
#' @param log.p Logical. If `TRUE`, probabilities are stated as \eqn{\log(p)}
#'
#' @returns Distribution function value (numeric)
#'
#' @keywords cumulative distribution GI0
#' @export
#' @examples
#' pGI0(3.2, -2, 3, 7) # -> 0.7579

pGI0 <- function(x, p_alpha, p_gamma, p_Looks, lower.tail=TRUE, log.p=FALSE) {
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

  x_neg <- x[x <= 0]
  x_pos <- x[x > 0]

  return(c(
    rep(0, length(x_neg)),
    pf(-p_alpha * x_pos / p_gamma, df1=2*p_Looks, df2=-2*p_alpha,
       lower.tail=lower.tail, log.p=log.p)
  ))

}
