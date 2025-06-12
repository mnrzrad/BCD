
abortflights
file <- choose.files()
lensfaults <- read.csv(file)
usethis::use_data(lensfaults, overwrite = TRUE)

# Load necessary package
library(readr)

# Define the seasons
seasons <- c("1415", "1516", "1617", "1718", "1819", "1920", "2021", "2122", "2223", "2324", "2425")

# Initialize named list
epl_season_data <- list()

# Read and extract data for each season
for (season in seasons) {
  file <- paste0("season-", season, ".csv")

  if (file.exists(file)) {
    df <- read_csv(file, show_col_types = FALSE)

    if (all(c("FTHG", "FTAG") %in% names(df))) {
      epl_season_data[[season]] <- df[, c("FTHG", "FTAG")]
      names(epl_season_data[[season]]) <- c("X", "Y")
    } else {
      warning(paste("Skipping", file, "- FTHG/FTAG columns missing"))
    }
  } else {
    warning(paste("File not found:", file))
  }
}

eplSeasonGoals <- epl_season_data  # rename the object
save(eplSeasonGoals, file = "eplSeasonGoals.rda")


