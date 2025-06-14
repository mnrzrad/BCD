#' Random Sampling from a Bivariate Geometric Distribution via Conditional Specification
#'
#' Generates random samples from a bivariate geometric distribution (BGCD)
#'
#' @param n number of samples to generate
#' @param q1 probability parameter for \eqn{ X }, in \eqn{(0, 1]}
#' @param q2 probability parameter for \eqn{ Y }, in\eqn{(0, 1]}
#' @param q3 dependence parameter, in \eqn{(0, 1]}
#' @param seed seed for random number generation (default = 123)
#'
#' @return A data frame with two columns: `X` and `Y`, containing the sampled values.
#'
#' @examples
#' # Generate 100 samples
#' samples <- rgeomBCD(n = 100, q1 = 0.5, q2 = 0.5, q3 = 0.00001)
#' head(samples)
#' cor(samples$X, samples$Y)  # Should be negative
#'
#' @importFrom stats rgeom
#' @export
rgeomBCD <- function(n, q1, q2, q3, seed = 123) {
  set.seed(seed)

  if (!is.numeric(q1) || length(q1) != 1 || q1 < 0 || q1 > 1) {
    stop("q1 must be a numeric scalar in [0, 1].")
  }
  if (!is.numeric(q2) || length(q2) != 1 || q2 < 0 || q2 > 1) {
    stop("q2 must be a numeric scalar in [0, 1].")
  }
  if (!is.numeric(q3) || length(q3) != 1 || q3 < 0 || q3 > 1) {
    stop("q3 must be a numeric scalar in [0, 1].")
  }
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != round(n)) {
    stop("n_samples must be a positive integer.")
  }

  KG <- normalize_constant_BGCD(q1, q2, q3)
  samples <- matrix(NA, nrow = n, ncol = 2)
  count <- 0

  while (count < n) {
    x <- rgeom(1, q1)
    y <- rgeom(1, q2)
    u <- runif(1)
    p <- dgeomBCD(x, y, q1, q2, q3)
    M <- KG / (q1 * q2)
    if (u < p / (M * (1-q1)^x * q1 * (1-q2)^y * q2)) {
      count <- count + 1
      samples[count, ] <- c(x, y)
    }
  }
  return(data.frame(X = samples[, 1], Y = samples[, 2]))
}
