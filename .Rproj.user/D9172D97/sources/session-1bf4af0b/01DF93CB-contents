#' Random Sampling from a Bivariate Binomial Distribution via Conditional Specification
#'
#' Generates random samples from a bivariate binomial conditionals distribution (BBCD).
#'
#' @param n number of samples to generate.
#' @param n1 number of trials for \eqn{ X }, must be non-negative.
#' @param n2 number of trials for \eqn{ Y }, must be non-negative.
#' @param p1 base success probability for \eqn{ X }, in (0, 1).
#' @param p2 base success probability for \eqn{ Y }, in (0, 1).
#' @param lambda dependence parameter, must be positive.
#' @param seed seed for random number generation (default = 123).
#' @param verbose logical; if TRUE (default), prints progress updates and a summary.
#'
#' @return A data frame with columns `X` and `Y`, containing the sampled values.
#' @examples
#' samples <- rbinomBCD(n = 100, n1 = 10, n2 = 10, p1 = 0.5, p2 = 0.4, lambda = 1.2)
#' head(samples)
#'
#' @importFrom stats  cor rbinom runif dbinom
#' @export
rbinomBCD <- function(n, n1, n2, p1, p2, lambda, seed = 123, verbose = TRUE) {
  set.seed(seed)

  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != round(n)) {
    stop("n must be a positive integer.")
  }
  if (!is.numeric(n1) || length(n1) != 1 || n1 <= 0 || n1 != round(n1)) {
    stop("n1 must be a positive integer.")
  }
  if (!is.numeric(n2) || length(n2) != 1 || n2 <= 0 || n2 != round(n2)) {
    stop("n2 must be a positive integer.")
  }
  if (!is.numeric(p1) || length(p1) != 1 || p1 < 0 || p1 > 1) {
    stop("p1 must be a numeric scalar in [0, 1].")
  }
  if (!is.numeric(p2) || length(p2) != 1 || p2 < 0 || p2 > 1) {
    stop("p2 must be a numeric scalar in [0, 1].")
  }
  if (!is.numeric(lambda) || length(lambda) != 1 || lambda <= 0) {
    stop("lambda must be a positive numeric scalar.")
  }

  start_time <- Sys.time()
  total_attempts <- 0
  last_report_time <- start_time

  if (verbose) {
    cat(sprintf("Starting to generate %d samples with parameters:\n", n))
    cat(sprintf("n1 = %d, n2 = %d, p1 = %.4f, p2 = %.4f, lambda = %.4f\n", n1, n2, p1, p2, lambda))

    if (lambda > 1) {
      cat("\nWARNING: For lambda > 1, sampling may take significantly longer due
          to the nature of the distribution.\n")
      cat("Please be patient as the simulation runs...\n\n")
    }
  }

  KG <- normalize_constant_BBCD(n1, n2, p1, p2, lambda)
  samples <- matrix(NA, nrow = n, ncol = 2)
  count <- 0

  while (count < n) {
    x <- rbinom(1, n1, p1)
    y <- rbinom(1, n2, p2)
    u <- runif(1)
    p <- dbinomBCD(x, y, n1, n2, p1, p2, lambda)
    g <- dbinom(x, n1, p1) * dbinom(y, n2, p2)
    M <- KG / (p1 * p2)

    total_attempts <- total_attempts + 1

    if (g > 0 && u < p / (M * g)) {
      count <- count + 1
      samples[count, ] <- c(x, y)

      if (verbose && count %% 10 == 0) {
        curr_time <- Sys.time()
        elapsed <- difftime(curr_time, start_time, units = "secs")
        acceptance_rate <- count / total_attempts
        cat(sprintf("\r%d/%d samples (%.1f%%) | Attempts: %d | Time: %.1f sec",
                    count, n, count / n * 100, total_attempts, as.numeric(elapsed)))
        utils::flush.console()
      }
    }

    if (verbose && total_attempts %% 10000 == 0) {
      curr_time <- Sys.time()
      elapsed <- difftime(curr_time, start_time, units = "secs")
      acceptance_rate <- count / total_attempts
      cat(sprintf("\r%d/%d samples (%.1f%%) | Attempts: %d | Time: %.1f sec",
                  count, n, count / n * 100, total_attempts, as.numeric(elapsed)))
      utils::flush.console()
    }
  }

  if (verbose) {
    elapsed <- difftime(Sys.time(), start_time, units = "secs")
    acceptance_rate <- count / total_attempts
    rate_per_second <- count / as.numeric(elapsed)

    cat(sprintf("\n\nSampling completed:\n"))
    cat(sprintf("Total samples: %d\n", count))
    cat(sprintf("Total attempts: %d\n", total_attempts))
    cat(sprintf("Total time: %.2f seconds\n", as.numeric(elapsed)))

    cor_val <- cor(samples[,1], samples[,2])
    sign_txt <- ifelse(cor_val > 0.01, "positive",
                       ifelse(cor_val < -0.01, "negative", "approximately zero"))
    cat(sprintf("Sample correlation: %.4f (%s)\n", cor_val, sign_txt))
    expected_sign <- ifelse(lambda > 1, "positive", ifelse(lambda < 1, "negative", "approximately zero"))
    if (sign_txt != expected_sign) {
      cat(sprintf("Note: Sample correlation sign (%s) differs from theoretical expectation (%s) for lambda = %.2f\n", sign_txt, expected_sign, lambda))
      if (n < 500) {
        cat("This might be due to small sample size. Try increasing n.\n")
      }
      cat("Alternatively, verify that dbinomBCD and normalize_constant_BBCD are correctly implemented.\n")
    }
  }

  return(data.frame(X = samples[, 1], Y = samples[, 2]))
}
