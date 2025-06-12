#' English Premier League Goals (2014–2019)
#'
#' A list of data frames for five consecutive seasons (2014/15 to 2018/19)
#' from the English Premier League. Each data frame contains the number of
#' full-time home (`X`) and away (`Y`) goals scored in each match of the season.
#'
#' @format A named list of 5 data frames:
#' \describe{
#'   \item{1415}{380 rows, variables: \code{X} (home goals), \code{Y} (away goals)}
#'   \item{1516}{380 rows, variables: \code{X} (home goals), \code{Y} (away goals)}
#'   \item{1617}{380 rows, variables: \code{X} (home goals), \code{Y} (away goals)}
#'   \item{1718}{380 rows, variables: \code{X} (home goals), \code{Y} (away goals)}
#'   \item{1819}{380 rows, variables: \code{X} (home goals), \code{Y} (away goals)}
#'   \item{1920}{380 rows, variables: \code{X} (home goals), \code{Y} (away goals)}
#'   \item{2021}{380 rows, variables: \code{X} (home goals), \code{Y} (away goals)}
#'   \item{2122}{380 rows, variables: \code{X} (home goals), \code{Y} (away goals)}
#'   \item{2223}{380 rows, variables: \code{X} (home goals), \code{Y} (away goals)}
#'   \item{2324}{380 rows, variables: \code{X} (home goals), \code{Y} (away goals)}
#'   \item{2525}{380 rows, variables: \code{X} (home goals), \code{Y} (away goals)}
#' }
#'
#' @details
#' Data source: English Premier League match results from \url{https://football-data.co.uk/} (formerly hosted on datahub.io).
#'
#' @usage data(eplSeasonGoals)
#'
#' @keywords datasets
#' @examples
#' data(eplSeasonGoals)
#' head(eplSeasonGoals[["1415"]])
#' head(eplSeasonGoals[["2425"]])
"eplSeasonGoals"
