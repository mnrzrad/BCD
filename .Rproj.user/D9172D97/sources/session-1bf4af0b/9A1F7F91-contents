#' Maximum Likelihood Estimation for a Bivariate Binomial Distribution via Conditional Specification
#'
#' Estimates the parameters of a Bivariate Binomial Conditionals via Conditional Specification using maximum likelihood.
#'
#' @param data A data frame or matrix with columns `X` and `Y`
#' @param fixed_n1 known value of `n1` (NULL to estimate)
#' @param fixed_n2 known value of `n2` (NULL to estimate)
#' @param verbose logical; print progress
#'
#' #' @return A list of class \code{"MLEpoisBCD"} containing:
#' \describe{
#'   \item{\code{n1}}{estimated n1}
#'   \item{\code{n2}}{estimated n2}
#'   \item{\code{p1}}{estimated p1}
#'   \item{\code{p2}}{estimated p2}
#'   \item{\code{lambda}}{estimated lambda}
#'   \item{\code{logLik}}{Maximum log-likelihood achieved.}
#'   \item{\code{AIC}}{Akaike Information Criterion.}
#'   \item{\code{BIC}}{Bayesian Information Criterion.}
#'   \item{\code{convergence}}{Convergence status from the optimizer (0 means successful).}
#' }
#' @examples
#' data <- rbinomBCD(n = 50,n1 = 5, n2 = 3, p1 = 0.6, p2 = 0.4, lambda = 1.2)
#' MLEbinomBCD(data)
#' MLEbinomBCD(data, fixed_n1 = 5, fixed_n2 = 3)
#' @importFrom stats cor optim
#' @export
MLEbinomBCD <- function(data,
                        fixed_n1 = NULL,
                        fixed_n2 = NULL,
                        verbose = TRUE) {
  if (is.data.frame(data)) {
    X <- data[[1]]
    Y <- data[[2]]
  } else {
    X <- data[, 1]
    Y <- data[, 2]
  }
  maxX <- max(X)
  maxY <- max(Y)
  default_range_n1 <- 5
  default_range_n2 <- 5

  if (is.null(fixed_n1) || is.null(fixed_n2)) {

    cat(sprintf("By default, n1 ranges from lower_n1 = %d to upper_n1 = %d, and n2 from lower_n2 = %d to upper_n2 = %d.\n",
                maxX, maxX + default_range_n1, maxY, maxY + default_range_n2))
    ans <- readline("Do you want to change the upper_n1 and upper_n2? (y/n): ")

    if (tolower(ans) == "y") {
      upper_n1 <- as.integer(readline(sprintf("Enter upper_n1 (n1 will be searched up from %d to upper_n1): ", maxX)))
      range_n1 = upper_n1 - max(X)
      upper_n2 <- as.integer(readline(sprintf("Enter upper_n2 (n2 will be searched up from %d to upper_n2): ", maxY)))
      range_n2 = upper_n2 - max(Y)
    } else {
      range_n1 <- default_range_n1
      range_n2 <- default_range_n2
    }
  } else {
    range_n1 <- 0
    range_n2 <- 0
  }
  estimate_n1 <- is.null(fixed_n1)
  estimate_n2 <- is.null(fixed_n2)
  n1 <- if (!estimate_n1) fixed_n1 else max(X)
  n2 <- if (!estimate_n2) fixed_n2 else max(Y)
  compute_loglik <- function(n1, n2, p1, p2, lambda) {
    if (n1 < max(X) || n2 < max(Y) ||
        p1 <= 0 || p1 >= 1 ||
        p2 <= 0 || p2 >= 1 ||
        lambda <= 0) {
      return(-Inf)
    }
    total_loglik <- 0
    for (i in seq_along(X)) {
      if (X[i] > n1 || Y[i] > n2) return(-Inf)
      tryCatch({
        prob <- dbinomBCD(X[i], Y[i], n1, n2, p1, p2, lambda)
        if (is.na(prob) || prob <= 0) {
          return(-Inf)
        }
        total_loglik <- total_loglik + log(prob)
      }, error = function(e) {
        return(-Inf)
      })
    }
    if (is.na(total_loglik) || is.infinite(total_loglik)) return(-Inf)
    return(total_loglik)
  }
  n1_candidates <- if (estimate_n1) maxX:(maxX + range_n1) else fixed_n1
  n2_candidates <- if (estimate_n2) maxY:(maxY + range_n2) else fixed_n2
  best_loglik <- -Inf
  best_params <- NULL
  if (verbose) {
    cat("Starting MLE estimation for BBCD...\n")
    if (estimate_n1) cat("Estimating n1 (grid search from", min(n1_candidates), "to", max(n1_candidates), ")\n")
    if (estimate_n2) cat("Estimating n2 (grid search from", min(n2_candidates), "to", max(n2_candidates), ")\n")
  }
  total_combinations <- length(n1_candidates) * length(n2_candidates)
  current_comb <- 0
  for (n1_val in n1_candidates) {
    for (n2_val in n2_candidates) {
      current_comb <- current_comb + 1
      if (verbose && current_comb %% 2 == 0) {
        cat(sprintf("\r Trying combination %d/%d: n1=%d, n2=%d", current_comb, total_combinations, n1_val, n2_val))
        utils::flush.console()
      }
      if (max(X) > n1_val || max(Y) > n2_val) next
      p1_init <- mean(X) / n1_val
      p2_init <- mean(Y) / n2_val
      p1_init <- max(0.01, min(0.99, p1_init))
      p2_init <- max(0.01, min(0.99, p2_init))
      sample_cor <- cor(X, Y)
      if (sample_cor > 0.1) {
        lambda_candidates <- c(1.5, 2.0, 1.2)
      } else if (sample_cor < -0.1) {
        lambda_candidates <- c(0.5, 0.8, 0.3)
      } else {
        lambda_candidates <- c(1.0, 0.8, 1.2)
      }
      best_local_loglik <- -Inf
      best_local_params <- NULL
      for (lambda_init in lambda_candidates) {
        initial_params <- c(p1_init, p2_init, lambda_init)
        opt_fn <- function(params) {
          result <- tryCatch({
            -compute_loglik(n1_val, n2_val, params[1], params[2], params[3])
          }, error = function(e) {
            return(Inf)
          })
          if (is.na(result) || !is.finite(result)) return(Inf)
          return(result)
        }
        opt_result <- tryCatch({
          optim(
            par = initial_params,
            fn = opt_fn,
            method = "L-BFGS-B",
            lower = c(0.001, 0.001, 0.001),
            upper = c(0.999, 0.999, 10),
            control = list(maxit = 500, factr = 1e12)
          )
        }, error = function(e) {
          if (verbose) cat("\nOptimization error for lambda_init =", lambda_init, ":", e$message, "\n")
          return(list(par = initial_params, value = Inf, convergence = 1))
        })
        if (opt_result$convergence == 0 && abs(opt_result$par[3] - 10.0) < 0.01) {
          if (verbose) cat("\nLambda hit upper bound, trying extended range...\n")
          opt_result2 <- tryCatch({
            optim(
              par = initial_params,
              fn = opt_fn,
              method = "L-BFGS-B",
              lower = c(0.001, 0.001, 0.001),
              upper = c(0.999, 0.999, 50.0),  # Extended range
              control = list(maxit = 500, factr = 1e12)
            )
          }, error = function(e) {
            opt_result  # Return original result if extended optimization fails
          })
          # Use extended result if it's better and doesn't hit the new boundary
          if (opt_result2$convergence == 0 &&
              opt_result2$value < opt_result$value &&
              abs(opt_result2$par[3] - 50.0) > 0.01) {
            opt_result <- opt_result2
          }
        }
        if (opt_result$convergence == 0 && is.finite(opt_result$value)) {
          current_loglik <- -opt_result$value

          if (current_loglik > best_local_loglik) {
            best_local_loglik <- current_loglik
            best_local_params <- opt_result$par
          }
        }
      }
      if (!is.null(best_local_params) && best_local_loglik > best_loglik) {
        best_loglik <- best_local_loglik
        best_params <- list(
          n1 = n1_val,
          n2 = n2_val,
          p1 = best_local_params[1],
          p2 = best_local_params[2],
          lambda = best_local_params[3]
        )
        if (verbose) {
          cat(sprintf("\nImproved fit: n1=%d, n2=%d, p1=%.4f, p2=%.4f, lambda=%.4f, logLik=%.2f\n", n1_val, n2_val, best_params$p1, best_params$p2, best_params$lambda, best_loglik))
        }
      }
    }
  }
  if (verbose) cat("\n")
  if (is.null(best_params)) {
    stop("Failed to find valid parameter estimates. Try different initial values or fix n1 and n2.")
  }
  if (best_params$lambda > 5) {
    if (verbose) {
      cat("WARNING: Lambda estimate is quite large (", round(best_params$lambda, 4), ").\n")
      cat("This might indicate:\n")
      cat("1. Very strong positive correlation in the data\n")
      cat("2. Potential issues with the optimization or model specification\n")
      cat("3. The BBCD model might not be appropriate for this data\n\n")
    }
  }
  k <- 3 + estimate_n1 + estimate_n2
  n <- length(X)
  aic <- -2 * best_loglik + 2 * k
  bic <- -2 * best_loglik + k * log(n)
  result <- c(
    best_params,
    list(
      logLik = best_loglik,
      AIC = aic,
      BIC = bic,
      n_params = k,
      n_obs = n
    )
  )
  if (verbose) {
    cat("\nFinal parameter estimates:\n")
    cat(sprintf("n1 = %d\n", result$n1))
    cat(sprintf("n2 = %d\n", result$n2))
    cat(sprintf("p1 = %.4f\n", result$p1))
    cat(sprintf("p2 = %.4f\n", result$p2))
    cat(sprintf("lambda = %.4f\n", result$lambda))
    cat(sprintf("Log-likelihood: %.4f\n", result$logLik))
    cat(sprintf("AIC: %.4f\n", result$AIC))
    cat(sprintf("BIC: %.4f\n", result$BIC))
  }
  return(result)
}
