#' Freeman–Tukey  Test for Bivariate Distributions via Conditional Specification
#'
#' Performs a goodness-of-fit test using the Freeman–Tukey (F–T) statistic
#' for a given dataset and a specified bivariate distribution.
#'
#' @param data a dataset or matrix with two columns.
#' @param distribution a string specifying the theoretical distribution (`"BBCD"`, `"BBPD"`, or `"BBGD"`).
#' @param params a named list of parameters required by the specified distribution.
#' @param num_params an integer specifying the number of parameters that were estimated
#'
#' @return A list with components:
#' \describe{
#'   \item{observed}{Observed frequency table}
#'   \item{expected}{Expected frequency table under the specified distribution}
#'   \item{test}{Result of the Freeman–Tukey test, a list with test statistic and p-value}
#' }
#'
#' @examples
#' samples <- rbinomBCD(n = 50, n1 = 5, n2 = 2, p1 = 0.15, p2 = 0.9, lambda = 1, seed = 1)
#'params1 <- MLEbinomBCD(samples, fixed_n1 = 5, fixed_n2 = 2)
#'result_bbcd1 <- FTtest(samples, "BBCD", params1, num_params = 3)
#'result_bbcd1
#'
#'params2 <- MLEbinomBCD(samples)
#'result_bbcd2 <- FTtest(samples, "BBCD", params2, num_params = 5)
#'result_bbcd2
#'
#'samples <- rgeomBCD(n = 50, q1 = 0.5, q2 = 0.5, q3 = 0.1, seed = 123)
#'params <- MLEgeomBCD(samples)
#'result_bgcd <- FTtest(samples, "BGCD", params, num_params = 3)
#'result_bgcd
#'
#'samples <- rpoisBCD(n = 50, lambda1 = 0.5, lambda2 = 0.5, lambda3 = 0.5, seed = 8)
#'params <- MLEpoisBCD(samples)
#'result_bpcd <- FTtest(samples, "BPCD", params, num_params = 3)
#'result_bpcd
#'
#'@importFrom stats pchisq
#'@export
FTtest <- function(data, distribution, params, num_params) {
  obs_table <- create_observed_table(data)
  exp_table <- create_expected_table(data = data,
                                     distribution = distribution,
                                     params = params)

  test_result <- freeman_tukey_test(obs_table, exp_table, distribution, num_params)

  return(list(
    observed = obs_table,
    expected = exp_table,
    test = test_result
  ))
}
