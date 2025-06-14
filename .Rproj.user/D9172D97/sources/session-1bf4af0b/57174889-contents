#' Joint Probability Mass Function for a Bivariate Poisson Distribution via Conditional Specification
#'
#' Computes the joint probability mass function (p.m.f.) of a Bivariate Poisson Conditionals distribution (BPCD) based on Ghosh, Marques, and Chakraborty (2021).
#'
#' @param x value of \eqn{ X } that must be a non-negative integer
#' @param y value of \eqn{ Y } that must be a non-negative integer
#' @param lambda1 rate parameter for \eqn{ X } that must be positive
#' @param lambda2 rate parameter for \eqn{ Y } that must be positive
#' @param lambda3 dependence parameter that must be \eqn{(0, 1]}
#'
#' @return probability \eqn{ P(X = x, Y = y) } for each pair of \eqn{ x } and \eqn{ y }.
#'
#' @details
#' The joint p.m.f. of the BGCD is
#' \deqn{
#' P(X = x, Y = y) = K(\lambda_1, \lambda_2, \lambda_3) \frac{\lambda_1^x \lambda_2^y \lambda_3^{xy}}{x! y!},
#' }
#' where \eqn{ x, y = 0, 1, 2, \ldots }, and \eqn{ K(\lambda_1, \lambda_2, \lambda_3) } is the normalizing
#' constant computed by the function \code{normalize_constant_BPCD}.
#'
#' Key properties of the BPCD include:
#'
#' - Negative correlation for \eqn{ \lambda_3 < 1 },
#'
#' - Independence for \eqn{ \lambda_3 = 1 }.
#'
#'
#' @examples
#' # Compute P(X = 1, Y = 2) with lambda1 = 0.5, lambda2 = 0.5, lambda3 = 0.5
#' dpoisBCD(x = 1, y = 2, lambda1 = 0.5, lambda2 =  0.5, lambda3 =  0.5)
#'
#' # Compute P(X = 0, Y = 1) with lambda1 = 0.5, lambda2 = 0.5, lambda3 = 0.5
#' dpoisBCD(x = 0, y = 1, lambda1 = 0.5, lambda2 =  0.5, lambda3 =  0.5)
#'
#' @seealso
#' \code{\link{rpoisBCD}}, \code{\link{ppoisBCD}}
#'
#' @references
#' Ghosh, I., Marques, F., & Chakraborty, S. (2021). A new bivariate Poisson distribution via conditional specification: properties and applications. \emph{Journal of Applied Statistics}, 48(16), 3025-3047. \doi{10.1080/02664763.2020.1793307}
#'
#' @export
dpoisBCD <- function(x, y, lambda1, lambda2, lambda3) {
  validate_lambda <- function(param, name) {
    if (!is.numeric(param) || length(param) != 1 || param <= 0) {
      stop(paste(name, "must be a positive numeric scalar"))
    }
  }
  validate_lambda(lambda1, "lambda1")
  validate_lambda(lambda2, "lambda2")
  validate_lambda(lambda3, "lambda3")
  validate_counts <- function(vec, name) {
    if (!is.numeric(vec) || any(vec < 0) || any(vec != floor(vec))) {
      stop(paste("All", name, "must be positive integers"))
    }
  }
  validate_counts(x, "x")
  validate_counts(y, "y")
  c <- normalize_constant_BPCD(lambda1, lambda2, lambda3)
  pmf <- (lambda1^x * lambda2^y * lambda3^(x * y)) / (factorial(x) * factorial(y))
  return(c * pmf)
}
