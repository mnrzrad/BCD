#' Seed and Plant Count Data
#'
#' This dataset records the number of seeds sown and the number of resulting plants grown over plots of fixed area (5 square feet).
#'
#' @format A data frame with \emph{n} rows and 2 variables:
#' \describe{
#'   \item{X}{Number of seeds sown.}
#'   \item{Y}{Number of plants grown.}
#' }
#'
#' @references
#' Lakshminarayana, J., S. N. N. Pandit, and K. Srinivasa Rao. 1999. On a bivariate poisson distribution.\emph{ Communications in Statistics - Theory and Methods}, 28 (2), 267--276. \doi{10.1080/03610929908832297}
#'
#' Ghosh, I., Marques, F., & Chakraborty, S. (2025). A form of bivariate binomial conditionals distributions. \emph{Communications in Statistics - Theory and Methods}, 54(2), 534--553. \doi{10.1080/03610926.2024.2315294}
#'
#'
#' @examples
#' data(seedplant)
#' head(seedplant)
#' plot(seedplant$X, seedplant$Y,
#'      xlab = "Seeds Sown",
#'      ylab = "Plants Grown",
#'      main = "Seed vs. Plant Count per Plot")
"seedplant"
