#' Joint Probability Mass Function for A Bivariate Geometric Distribution via Conditional Specification
#'
#' Computes the joint probability mass function (p.m.f.) of a Bivariate Geometric Conditional Distributions (BGCD) based on Ghosh, Marques, and Chakraborty (2023). This distribution models paired count data with geometric conditionals, incorporating dependence between variables \eqn{ X } and \eqn{ Y }.
#'
#' @param x value of \eqn{ X } that must be non-negative integer
#' @param y value of \eqn{ Y } that must be non-negative integer
#' @param q1 probability parameter for \eqn{ X }, in \eqn{(0, 1]}
#' @param q2 probability parameter for \eqn{ Y }, in \eqn{(0, 1]}
#' @param q3 dependence parameter, in \eqn{(0, 1]}
#'
#' @return The probability \eqn{ P(X = x, Y = y) } for each pair of \eqn{ x } and \eqn{ y }.
#'
#' @details
#' The joint p.m.f. of the BGCD is:
#' \deqn{
#' P(X = x, Y = y) = K(q_1, q_2, q_3) q_1^x q_2^y q_3^{xy},
#' }
#' where \eqn{ K(q_1, q_2, q_3) } is the normalizing constant computed by the function \code{normalize_constant_BGCD}.
#'
#' Note that:
#'
#' - \eqn{ q_3 < 1 } :  indicates the negative correlation between \eqn{X} and \eqn{Y}
#'
#' - \eqn{ q_3 = 1 } : indicates the independence between \eqn{X} and \eqn{Y}
#'
#'
#' @examples
#' # Compute P(X = 1, Y = 2) with q1 = 0.5, q2 = 0.6, q3 = 0.8
#' dgeomBCD(x = 1, y = 2, q1 = 0.5, q2 = 0.6, q3 = 0.8)
#'
#' # # Compute P(X = 0, Y = 4) with q1 = 0.5, q2 = 0.6, q3 = 0.8
#' dgeomBCD(x = 0, y = 4, q1 = 0.5, q2 = 0.6, q3 = 0.8)
#'
#' @seealso
#' \code{\link{pgeomBCD}} \code{\link{rgeomBCD}} \code{\link{MLEgeomBCD}}
#'
#' @references
#' Ghosh, I., Marques, F., & Chakraborty, S.(2023) A bivariate geometric distribution via conditional specification: properties and applications, Communications in Statistics - Simulation and Computation, 52:12, 5925--5945, \doi{10.1080/03610918.2021.2004419}
#'
#' @export
dgeomBCD <- function(x, y, q1, q2, q3) {
  if (!all(x == floor(x)) || any(x < 0)) {
    stop("All elements of x must be non-negative integers.")
  }
  if (!all(y == floor(y)) || any(y < 0)) {
    stop("All elements of y must be non-negative integers.")
  }
  if (!is.numeric(q1) || length(q1) != 1 || q1 < 0 || q1 > 1) {
    stop("q1 must be a numeric scalar in [0, 1].")
  }
  if (!is.numeric(q2) || length(q2) != 1 || q2 < 0 || q2 > 1) {
    stop("q2 must be a numeric scalar in [0, 1].")
  }
  if (!is.numeric(q3) || length(q3) != 1 || q3 < 0 || q3 > 1) {
    stop("q3 must be a numeric scalar in [0, 1].")
  }
  c <- normalize_constant_BGCD(q1, q2, q3)
  pmf <- c * (q1^x) * (q2^y) * (q3^(x * y))
  return(pmf)
}
