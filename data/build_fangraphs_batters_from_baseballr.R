suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(baseballr)
})

start_season <- 2021
end_season <- 2025
output_path <- "data/fangraphs_batters_2021_2025.csv"

fg <- lapply(seq(start_season, end_season), function(season) {
  message(sprintf("Fetching FanGraphs batting leaders for %d", season))
  tryCatch(
    fg_bat_leaders(
      startseason = as.character(season),
      endseason = as.character(season),
      stats = "bat",
      pos = "all",
      lg = "all",
      qual = "0",
      ind = "1",
      pageitems = "5000"
    ),
    error = function(e) {
      message(sprintf("Failed to fetch season %d: %s", season, e$message))
      NULL
    }
  )
})

fg <- bind_rows(fg)
if (nrow(fg) == 0) {
  stop("No FanGraphs data returned for any season.")
}

needed_cols <- c(
  "Season", "PlayerName", "playerid", "Age", "position",
  "Team", "PA", "H", "R", "RBI", "HR", "SB", "AVG", "OBP", "SLG"
)

if (!"Team" %in% names(fg)) {
  fg <- fg %>% mutate(Team = dplyr::coalesce(team_name_abb, team_name))
}

missing_cols <- setdiff(needed_cols, names(fg))
if (length(missing_cols) > 0) {
  stop("Missing expected columns from FanGraphs: ", paste(missing_cols, collapse = ", "))
}

fg <- fg %>%
  mutate(
    Season = as.integer(Season),
    Age = as.numeric(Age),
    playerid = as.character(playerid),
    position = as.character(position),
    PA = as.numeric(PA)
  ) %>%
  filter(Season >= start_season, Season <= end_season)

# Keep seasons with PA >= 100
fg <- fg %>% filter(PA >= 100)

# Keep players with >=100 PA in either 2024 or 2025
keep_recent <- fg %>%
  group_by(playerid) %>%
  summarize(
    PA_2425_max = ifelse(
      any(Season %in% c(2024, 2025)),
      max(PA[Season %in% c(2024, 2025)], na.rm = TRUE),
      0
    ),
    .groups = "drop"
  ) %>%
  filter(PA_2425_max >= 100) %>%
  pull(playerid)

fg <- fg %>% filter(playerid %in% keep_recent)

out <- fg %>% select(all_of(needed_cols))

write_csv(out, output_path)
message(sprintf("Wrote %s (%d rows, %d cols)", output_path, nrow(out), ncol(out)))
