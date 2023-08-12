#' Quantile function of the GI0 distribution
#'
#' This function returns the quantile of point \eqn{x} on a GI0 distribution with parameters \eqn{\alpha}, \eqn{\gamma} and \eqn{Looks}.
#'
#' @param p_alpha Negative value that controls roughness
#' @param p_gamma Positive value that controls scale
#' @param p_Looks Positive value that controls number of looks
#' @param lower.tail Logical. If `TRUE`, values are \eqn{X \leq Q(p)}; otherwise, \eqn{X > Q(p)}
#' @param log.p Logical. If `TRUE`, probabilities are stated as \eqn{\log(p)}
#'
#' @returns Quantile function value (numeric)
#'
#' @keywords quantile distribution GI0
#' @export
#' @examples
#' qGI0(0.34, -2, 5, 4) # -> 0.7555

qGI0 <- function(p, p_alpha, p_gamma, p_Looks, lower.tail=TRUE, log.p=FALSE) {
  if (!all(sapply(c(p, p_alpha, p_gamma, p_Looks), is.numeric))) {
    stop("All arguments must be numeric.")
  }

  if (p_alpha >= 0) {
    stop("alpha must be negative.")
  } else if (p_gamma < 0) {
    stop("gamma must be positive.")
  } else if (p_Looks < 1) {
    stop("looks must be greater than or equal to 1.")
  }

  p_0 <- p[p < 0]
  p_0_1 <- p[p >= 0 & p < 1]
  p_1 <- p[p >= 1]

  return(c(
    rep(NA, length(p_0)),
    qf(p_0_1, df1=2*p_Looks, df2=-2*p_alpha, lower.tail=lower.tail, log.p=log.p),
    rep(NA, length(p_1))
  ))
}
