#' Maximum Likelihood Estimation for a Bivariate Geometric Distribution via Conditional Specification
#'
#' Estimates the parameters of a bivariate geometric distribution via Conditional Specification using maximum likelihood.
#'
#' @param data data frame or matrix with two columns, representing paired observations of count variables \eqn{(X, Y)}.
#' @param initial_values numeric vector of length 3 with initial values for the parameters \code{q1}, \code{q2}, and \code{q3}. Must be strictly between 0 and 1. Default is \code{c(0.5, 0.5, 0.5)}.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{q1}}{estimated q1.}
#'   \item{\code{q2}}{estimated q2.}
#'   \item{\code{q3}}{estimated q3.}
#'   \item{\code{logLik}}{Maximum log-likelihood achieved.}
#'   \item{\code{AIC}}{Akaike Information Criterion.}
#'   \item{\code{BIC}}{Bayesian Information Criterion.}
#'   \item{\code{convergence}}{Convergence status from the optimizer (0 means successful).}}
#'
#' @details
#' The model estimates parameters from a joint distribution for \eqn{(X, Y)} with the form:
#' \deqn{
#' P(X = x, Y = y) = K(q_1, q_2, q_3) q_1^x q_2^y q_3^{xy},
#' }
#' where \eqn{ K(q_1, q_2, q_3) } is the normalizing constant.
#'
#' @examples
#' # Simulate data
#' samples <- rgeomBCD(n = 50, q1 = 0.2, q2 = 0.2, q3 = 0.5)
#' result <-MLEgeomBCD(samples)
#' print(result)
#' # For better estimation accuracy and stability, consider increasing the sample size (n = 1000)
#'
#' data(abortflights)
#' MLEgeomBCD(abortflights)
#'
#' @references
#' Ghosh, I., Marques, F., & Chakraborty, S.  (2023) A bivariate geometric distribution via conditional specification: properties and applications, Communications in Statistics - Simulation and Computation, 52:12, 5925--5945, \doi{10.1080/03610918.2021.2004419}
#'
#' @seealso
#' \code{\link{dgeomBCD}} \code{\link{pgeomBCD}} \code{\link{rgeomBCD}}
#'
#' @importFrom stats optim
#' @export
MLEgeomBCD <- function(data, initial_values = c(0.5, 0.5, 0.5)) {
  if (is.data.frame(data)) {
    X <- data[[1]]
    Y <- data[[2]]
  } else {
    X <- data[,1]
    Y <- data[,2]
  }
  loglik <- function(par) {
    q1 <- par[1]; q2 <- par[2]; q3 <- par[3]
    eps <- 1e-6
    if (any(c(q1, q2, q3) <= eps) || any(c(q1, q2, q3) >= 1 - eps)) return(-1e10)
    norm <- normalize_constant_BGCD(q1, q2, q3)
    if (!is.finite(norm) || norm <= 0) return(-Inf)

    sumx <- sum(X)
    sumy <- sum(Y)
    sumxy <- sum(X * Y)
    n <- length(X)

    ll <- sumx * log(q1) + sumy * log(q2) + sumxy * log(q3) + n * log(norm)
    return(ll)
  }

  negloglik <- function(par) -loglik(par)

  result <- optim(
    par = initial_values,
    fn = negloglik,
    method = "L-BFGS-B",
    lower = rep(0.001, 3),
    upper = rep(0.99, 3),
    control = list(maxit = 1000)
  )

  if (result$convergence != 0) stop("Convergence failed")

  est <- result$par
  n <- length(X)
  logL <- -result$value
  aic <- -2 * logL + 2 * 3
  bic <- -2 * logL + 3 * log(n)

  return(list(
    q1 = est[1],
    q2 = est[2],
    q3 = est[3],
    logLik = logL,
    AIC = aic,
    BIC = bic,
    convergence = result$convergence
  ))
}
