#' Maximum Likelihood Estimation for a Bivariate Poisson Distribution via Conditional Specification
#'
#' Estimates the parameters of a bivariate Poisson distribution via Conditional Specification using maximum likelihood.
#'
#' @param data data frame or matrix with two columns, representing paired observations of count variables \eqn{(X, Y)}.
#' @param initial_values optional named list with initial values for the parameters: \code{lambda1}, \code{lambda2}, and \code{lambda3}.
#' If not provided, the function computes heuristic starting values.
#'
#' @return A list of class \code{"MLEpoisBCD"} containing:
#' \describe{
#'   \item{\code{lambda1}}{estimated lambda1.}
#'   \item{\code{lambda2}}{estimated lambda2.}
#'   \item{\code{lambda3}}{estimated dependence parameter (must be in (0, 1]).}
#'   \item{\code{logLik}}{Maximum log-likelihood achieved.}
#'   \item{\code{AIC}}{Akaike Information Criterion.}
#'   \item{\code{BIC}}{Bayesian Information Criterion.}
#'   \item{\code{convergence}}{Convergence status from the optimizer (0 means successful).}
#' }
#'
#' @details
#' The model estimates parameters from a joint distribution for \eqn{(X, Y)} with the form:
#' \deqn{
#' P(X = x, Y = y) = K(\lambda_1, \lambda_2, \lambda_3) \frac{\lambda_1^x \lambda_2^y \lambda_3^{xy}}{x! y!},
#' }
#' where \eqn{ x, y = 0, 1, 2, \ldots }, and \eqn{ K(\lambda_1, \lambda_2, \lambda_3) } is the normalizing constant.
#'
#' @examples
#' # Simulate data
#' data <- rpoisBCD(n = 50, lambda1 = 3, lambda2 = 5, lambda3 = 1)
#' result <- MLEpoisBCD(data)
#' print(result)
#'
#' data(eplSeasonGoals)
#' MLEpoisBCD(eplSeasonGoals[["1819"]])
#'
#' data(lensfaults)
#' MLEpoisBCD(lensfaults)
#' @seealso \code{\link{dpoisBCD}} \code{\link{ppoisBCD}} \code{\link{rpoisBCD}}
#' @importFrom stats cor optim
#' @export
MLEpoisBCD <- function(data, initial_values = NULL) {
  if (is.data.frame(data)) {
    X <- data[[1]]
    Y <- data[[2]]
  } else {
    X <- data[, 1]
    Y <- data[, 2]
  }
  if (any(X < 0) || any(Y < 0) || any(X != floor(X)) || any(Y != floor(Y))) {
    stop("Data must contain non-negative integers")
  }
  if (is.null(initial_values)) {
    lambda1_init <- mean(X)
    lambda2_init <- mean(Y)
    corr <- cor(X, Y)
    corr_threshold <- 0.001
    if (abs(corr) <= corr_threshold) {
      lambda3_init <- 1.0
    } else if (corr > corr_threshold) {
      lambda3_init <- 1.0
      } else {
        lambda3_init <- 0.9 + ((corr + corr_threshold) / (1 - corr_threshold)) * 0.8
      }
    } else {
    lambda1_init <- initial_values$lambda1
    lambda2_init <- initial_values$lambda2
    lambda3_init <- initial_values$lambda3
  }
  lambda1_init <- max(0.1, lambda1_init)
  lambda2_init <- max(0.1, lambda2_init)
  lambda3_init <- min(max(0.01, lambda3_init), 1.0)

  log_likelihood <- function(params) {
    lambda1 <- params[1]
    lambda2 <- params[2]
    lambda3 <- params[3]
    if (lambda1 <= 0 || lambda2 <= 0 || lambda3 <= 0 || lambda3 > 1) {
      return(-Inf)
    }
    ll <- 0
    for (i in seq_along(X)) {
      prob <- dpoisBCD(X[i], Y[i], lambda1, lambda2, lambda3)
      if (prob < .Machine$double.eps) {
        log_c <- log(normalize_constant_BPCD(lambda1, lambda2, lambda3))
        log_pmf <- X[i] * log(lambda1) + Y[i] * log(lambda2) +
          (X[i] * Y[i]) * log(lambda3) -
          log(factorial(X[i])) - log(factorial(Y[i]))
        log_prob <- log_c + log_pmf

        if (is.finite(log_prob)) {
          ll <- ll + log_prob
        } else {
          return(-Inf)
        }
      } else {
        ll <- ll + log(prob)
      }
    }
    return(ll)
  }
  initial_params <- c(lambda1_init, lambda2_init, lambda3_init)
  lower_bounds <- c(0.0001, 0.0001, 0.0001)
  upper_bounds <- c(Inf, Inf, 1.0)
  if (lambda3_init < 0.05) {
    method <- "Nelder-Mead"
  } else {
    method <- "L-BFGS-B"
  }
  result <- tryCatch({
    optim(
      par = initial_params,
      fn = function(params) -log_likelihood(params),
      method = method,
      lower = lower_bounds,
      upper = upper_bounds,
      control = list(maxit = 10000)
    )
  }, error = function(e) {
    return(NULL)
  })
  if (is.null(result) || result$convergence != 0){
    lambda1_alt <- max(0.5, mean(X) * 0.8)
    lambda2_alt <- max(0.5, mean(Y) * 0.8)
    lambda3_alt <- 0.5
    result <- optim(
      par = c(lambda1_alt, lambda2_alt, lambda3_alt),
      fn = function(params) -log_likelihood(params),
      method = "L-BFGS-B",
      lower = lower_bounds,
      upper = upper_bounds,
      control = list(maxit = 10000)
    )
  }
  best_fit <- NULL
  best_loglik <- -Inf
  if (!is.null(result) && result$convergence == 0) {
    current_loglik <- -result$value
      if (current_loglik > best_loglik) {
        best_loglik <- current_loglik
        best_fit <- result
      }
  }
  else {
    stop("Failed to converge. Try providing different initial values.")
  }
  lambda1_est <- best_fit$par[1]
  lambda2_est <- best_fit$par[2]
  lambda3_est <- best_fit$par[3]
  n_params <- 3
  n_obs <- length(X)
  aic <- -2 * best_loglik + 2 * n_params
  bic <- -2 * best_loglik + n_params * log(n_obs)
  results <- list(
    lambda1 = lambda1_est,
    lambda2 = lambda2_est,
    lambda3 = lambda3_est,
    logLik = best_loglik,
    AIC = aic,
    BIC = bic,
    convergence = best_fit$convergence
  )
  class(results) <- "MLEpoisBCD"
  return(results)
}
