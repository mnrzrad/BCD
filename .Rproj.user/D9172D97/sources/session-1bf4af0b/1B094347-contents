#' Cumulative Distribution Function for a Bivariate Binomial Distribution via Conditional Specification
#'
#' Computes the cumulative distribution function (c.d.f.) of a bivariate binomial conditionals distribution (BBCD) as defined by Ghosh, Marques, and Chakraborty (2025).
#'
#' @param x value at which the c.d.f. is evaluated
#' @param y value at which the c.d.f. is evaluated
#' @param n1 number of trials for \eqn{ X }, must be non-negative.
#' @param n2 number of trials for \eqn{ Y }, must be non-negative.
#' @param p1 base success probability for \eqn{ X }, in (0, 1).
#' @param p2 base success probability for \eqn{ Y }, in (0, 1).
#' @param lambda dependence parameter, must be positive.
#'
#' @return The probability \eqn{ P(X \leq x, Y \leq y) }.
#'
#' @examples
#' # Compute P(X ≤ 2, Y ≤ 1) with n1 = 5, n2 = 5, p1 = 0.5, p2 = 0.4, lambda = 0.5
#' pbinomBCD(x = 2, y = 5, n1 = 5, n2 = 5, p1 = 0.5, p2 = 0.4, lambda = 0.5)
#'
#' # Example with independence (lambda = 1)
#' pbinomBCD(x = 1, y = 1, n1 = 10, n2 = 10, p1 = 0.3, p2 = 0.6, lambda = 1)
#'
#' @seealso
#' \code{\link{dbinomBCD}} \code{\link{rbinomBCD}}
#'
#' @references
#' Ghosh, I., Marques, F., & Chakraborty, S. (2025). A form of bivariate binomial conditionals distributions. \emph{Communications in Statistics - Theory and Methods}m 54(2), 534--553. \doi{10.1080/03610926.2024.2315294}
#'
#'
#' @export
pbinomBCD <- function(x, y, n1, n2, p1, p2, lambda) {
  if (!is.numeric(x) || !is.numeric(y) || !is.numeric(n1) || !is.numeric(n2) ||
      !is.numeric(p1) || !is.numeric(p2) || !is.numeric(lambda)) {
    stop("All inputs must be numeric.")
  }
  if (n1 < 0 || n2 < 0) {
    stop("n1 and n2 should be positive.")
  }
  if (p1 < 0 || p1 > 1 || p2 < 0 || p2 > 1) {
    stop("p1 and p2 must be between 0 and 1.")
  }
  if (lambda <= 0) {
    stop("Lambda must be greater than zero.")
  }

    c <- normalize_constant_BBCD(n1, n2, p1, p2, lambda)
  sum_cdf <- 0
  for (xx in 0:x) {
    for (yy in 0:y) {
      if(x <0 || y < 0){
        sum_cdf <- 0; break
      }
      log_term <- log(choose(n1, xx)) + log(choose(n2, yy)) +
        xx * log(p1) + yy * log(p2) +
        (n1 - xx) * log(1 - p1) + (n2 - yy) * log(1 - p2) +
        (xx * yy) * log(lambda)
      term <- exp(log_term)
      sum_cdf <- sum_cdf + term
    }
  }
  return(c * sum_cdf)
}
