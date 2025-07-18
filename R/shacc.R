#' Railway Shunter Accident Data (1937–1947)
#'
#' Accident records for 122 experienced railway shunters across two historical periods.
#'
#' @format A data frame with 122 rows and 2 variables:
#' \describe{
#'   \item{X}{Number of accidents during the 6-year period from 1937 to 1942.}
#'   \item{Y}{Number of accidents during the 5-year period from 1943 to 1947.}
#' }
#'
#' This dataset is useful for analyzing accident rates before and after possible policy or operational changes.
#'
#'
#' @references
#' Arbous, A. G., & Kerrich, J. E. (1951). Accident statistics and the concept of accident-proneness. \emph{Biometrics}, 7(4), 340. \doi{10.2307/3001656}
#'
#' Ghosh, I., Marques, F., & Chakraborty, S. (2025). A form of bivariate binomial conditionals distributions. \emph{Communications in Statistics - Theory and Methods}, 54(2), 534--553. \doi{10.1080/03610926.2024.2315294}
#'
#'
#' @examples
#' data(shacc)
#' head(shacc)
#' plot(shacc$X, shacc$Y, xlab = "Accidents 1937–42", ylab = "Accidents 1943–47")
"shacc"
