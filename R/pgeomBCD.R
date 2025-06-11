#' Cumulative Distribution Function for a Bivariate Geometric Distribution via Conditional Specification
#'
#' Computes the cumulative distribution function (c.d.f.) of a bivariate geometric conditionals distribution (BGCD) based on Ghosh, Marques, and
#' Chakraborty (2023).
#'
#' @param x value at which the c.d.f. is evaluated
#' @param y value at which the c.d.f. is evaluated
#' @param q1 probability parameter for \eqn{ X }, in (0, 1]
#' @param q2 probability parameter for \eqn{ Y }, in (0, 1]
#' @param q3 dependence parameter, in (0, 1]
#'
#' @return The probability \eqn{ P(X \leq x, Y \leq y) }.
#'
#' @examples
#' # Compute P(X ≤ 1, Y ≤ 2) with q1 = 0.5, q2 = 0.6, q3 = 0.8
#' pgeomBCD(x = 1, y = 2, q1 = 0.5, q2 = 0.6, q3 = 0.8)
#'
#' # Example with small values
#' pgeomBCD(x = 0, y = 0, q1 = 0.4, q2 = 0.3, q3 = 0.9)
#'
#' @seealso
#' \code{\link{dgeomBCD}}
#' \code{\link{rgeomBCD}}
#'
#' @references
#' Ghosh, I., Marques, F., & Chakraborty, S.  (2023) A bivariate geometric distribution via conditional specification: properties and applications, Communications in Statistics - Simulation and Computation, 52:12, 5925--5945, \doi{10.1080/03610918.2021.2004419}
#'
#' @export
pgeomBCD <- function(x, y, q1, q2, q3) {
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
  sum_cdf <- 0
  for (xx in 0:x) {
    for (yy in 0:y) {
      if(x <0 || y < 0){
        sum_cdf <- 0; break
      }
      term <- (q1^xx) * (q2^yy) * (q3^(xx * yy))
      sum_cdf <- sum_cdf + term
    }
  }
  return(sum_cdf * c)
}
