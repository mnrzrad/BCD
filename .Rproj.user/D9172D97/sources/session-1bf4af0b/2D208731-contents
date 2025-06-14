#' Joint Probability Mass Function for a Bivariate Binomial Distribution via Conditional Specification
#'
#' Computes the probability mass function (p.m.f.) of the bivariate binomial conditionals distribution (BBCD) as defined by Ghosh, Marques, and Chakraborty (2025). The distribution is characterized by conditional binomial distributions for \eqn{ X } and \eqn{ Y }.
#'
#' @param x value of \eqn{ X }, must be in \eqn{\{0, 1, ..., n_1\}}
#' @param y value of \eqn{ Y }, must be in \eqn{\{0, 1, ..., n_2\}}
#' @param n1 number of trials for \eqn{ X }, must be non-negative
#' @param n2 number of trials for \eqn{ Y }, must be non-negative
#' @param p1 base success probability for \eqn{ X }, in \eqn{(0, 1)}
#' @param p2 base success probability for \eqn{ Y }, in \eqn{(0, 1)}
#' @param lambda dependence parameter, must be positive.
#'
#' @return The probability \eqn{ P(X = x, Y = y) }.
#'
#' @details
#' The joint p.m.f. of the BBCD is
#' \deqn{
#' P(X = x, Y = y) = K_B(n_1, n_2, p_1, p_2, \lambda) \binom{n_1}{x} \binom{n_2}{y} p_1^x p_2^y (1 - p_1)^{n_1 - x} (1 - p_2)^{n_2 - y} \lambda^{xy},
#' }
#' where \eqn{ x = 0, 1, \ldots, n_1 }, \eqn{ y = 0, 1, \ldots, n_2 }, and \eqn{ K_B(n_1, n_2, p_1, p_2, \lambda) } is the normalizing constant.
#'
#'
#' @examples
#' # Compute P(X = 2, Y = 1) with n1 = 5, n2 = 5, p1 = 0.5, p2 = 0.4, lambda = 0.5
#' dbinomBCD(x = 2, y = 1, n1 = 5, n2 = 5, p1 = 0.5, p2 = 0.4, lambda = 0.5)
#'
#' # Example with independence (lambda = 1)
#' dbinomBCD(x = 2, y = 1, n1 = 5, n2 = 5, p1 = 0.5, p2 = 0.4, lambda = 1.0)
#'
#' @seealso
#' \code{\link{pbinomBCD}} \code{\link{rbinomBCD}} \code{\link{MLEbinomBCD}}
#'
#' @references
#' Ghosh, I., Marques, F., & Chakraborty, S. (2025). A form of bivariate binomial conditionals distributions. \emph{Communications in Statistics - Theory and Methods}, 54(2), 534--553. \doi{10.1080/03610926.2024.2315294}
#'
#'
#' @export
dbinomBCD <- function(x, y, n1, n2, p1, p2, lambda) {
  # Input validation
  if (!is.numeric(x) || !is.numeric(y) || !is.numeric(n1) || !is.numeric(n2) ||
      !is.numeric(p1) || !is.numeric(p2) || !is.numeric(lambda)) {
    stop("All inputs must be numeric.")
  }
  if (x < 0 || x > n1 || y < 0 || y > n2) {
    stop("x and y must be between 0 and their respective n values.")
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

  # Get normalization constant using stable method
  c <- normalize_constant_BBCD(n1, n2, p1, p2, lambda)

  # Compute in log space for numerical stability
  log_pmf <- log(c) +
    lchoose(n1, x) +
    lchoose(n2, y) +
    x * log(p1) +
    y * log(p2) +
    (n1 - x) * log(1 - p1) +
    (n2 - y) * log(1 - p2) +
    (x * y) * log(lambda)

  return(exp(log_pmf))
}
