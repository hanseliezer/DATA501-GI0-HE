#' Random variate generator from the GI0 distribution
#'
#' This function returns \eqn{n} random variates from a GI0 distribution with parameters \eqn{\alpha}, \eqn{\gamma} and \eqn{Looks}.
#'
#' @param n Sample size to generate.
#' @param p_alpha Negative value that controls roughness
#' @param p_gamma Positive value that controls scale
#' @param p_Looks Positive value that controls number of looks
#' @param from.F Logical. If `TRUE`, generate from F-distribution; otherwise, generate as ratio of two Gamma variates
#'
#' @returns Vector of random variates with length \eqn{n}
#'
#' @keywords random distribution GI0
#' @export
#' @examples
#' set.seed(1967, kind="Mersenne-Twister")
#' rGI0(5, -2, 4, 5) # -> returns (0.535, 0.842 2.635 3.863 1.599)


rGI0 <- function(n, p_alpha, p_gamma, p_Looks, from.F=FALSE){
  if (!all(sapply(c(n, p_alpha, p_gamma, p_Looks), is.numeric))) {
    stop("All arguments must be numeric.")
  }

  # if user enters non-integers, just round it
  n <- round(n)
  if (n <= 0) {
    stop("n must be larger than 0.")
  } else if (p_alpha >= 0) {
    stop("alpha must be negative.")
  } else if (p_gamma < 0) {
    stop("gamma must be positive.")
  } else if (p_Looks < 1) {
    stop("looks must be greater than or equal to 1.")
  }

  ifelse(from.F==TRUE,
         return(rf(n, df1=2*p_Looks, df2=-2*p_alpha)),
         return(
           rgamma(n, shape=p_Looks, rate=p_Looks) /
             rgamma(n, shape=-p_alpha, rate=p_gamma)
         )
  )
}
