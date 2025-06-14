#' Cumulative Distribution Function for a Bivariate Poisson Distribution via Conditional Specification
#'
#' Computes the cumulative distribution function (c.d.f.) of a bivariate Poisson distribution (BPD) with conditional specification, as described by Ghosh, Marques, and Chakraborty (2021).
#'
#' @param x value at which the c.d.f. is evaluated
#' @param y value at which the c.d.f. is evaluated
#' @param lambda1 rate parameter for \eqn{ X } that must be positive
#' @param lambda2 rate parameter for \eqn{ Y } that must be positive
#' @param lambda3 dependence parameter that must be (0, 1]
#'
#' @return The probability \eqn{ P(X \leq x, Y \leq y) }.
#'
#' @examples
#' # Compute P(X \le 1, Y \le 1) with lambda1 = 0.5, lambda2 = 0.5, lambda3 = 0.5
#' ppoisBCD(x = 1, y = 1, lambda1 = 0.5, lambda2 = 0.5, lambda3 = 0.5)
#'
#' # Example with larger values
#' ppoisBCD(x = 2, y = 2, lambda1 = 1.0, lambda2 = 1.0, lambda3 = 0.8)
#'
#' @seealso
#' \code{\link{dpoisBCD}} \code{\link{rpoisBCD}}
#'
#' @references
#' Ghosh, I., Marques, F., & Chakraborty, S. (2021). A new bivariate Poisson distribution via conditional specification: properties and applications. \emph{Journal of Applied Statistics}, 48(16), 3025-3047. \doi{10.1080/02664763.2020.1793307}
#'
#' @export
ppoisBCD <- function(x, y, lambda1, lambda2, lambda3) {
  validate_lambda <- function(param, name) {
    if (!is.numeric(param) || length(param) != 1 || param <= 0) {
      stop(paste(name, "must be a positive numeric scalar"))
    }
  }
  validate_lambda(lambda1, "lambda1")
  validate_lambda(lambda2, "lambda2")
  validate_lambda(lambda3, "lambda3")
  c <- normalize_constant_BPCD(lambda1, lambda2, lambda3)
  sum_cdf <- 0
  for (xx in 0:x) {
    for (yy in 0:y) {
      if(x <0 || y < 0){
        sum_cdf <- 0; break
      }
      term <-  ((lambda1^xx) * (lambda2^yy) * (lambda3^(xx * yy)))/(factorial(xx)*factorial(yy))
      sum_cdf <- sum_cdf + term
    }
  }
  return(sum_cdf * c)
}
