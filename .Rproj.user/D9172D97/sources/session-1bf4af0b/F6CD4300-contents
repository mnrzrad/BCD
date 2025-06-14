years <- c("1415","1516","1617","1718","1819")
for (yr in years) {
  url <- sprintf("https://datahub.io/core/english-premier-league/r/season-%s.csv", yr)
  dest <- paste0("season-", yr, ".csv")
  download.file(url, dest, mode = "wb")
}

# Example in R
url <- "https://raw.githubusercontent.com/datasets/football-datasets/main/datasets/premier-league/season-2425.csv"
download.file(url, "season-2425.csv", mode = "wb")
