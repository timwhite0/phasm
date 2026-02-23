suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(baseballr)
})

start_season <- as.integer(Sys.getenv("FG_START_SEASON", "2018"))
end_season <- as.integer(Sys.getenv("FG_END_SEASON", "2025"))
output_path <- Sys.getenv("FG_OUTPUT_PATH", "data/fangraphs_batters_2018_2025.csv")
require_recent <- as.integer(Sys.getenv("FG_REQUIRE_RECENT", "1"))
statcast_cache_path <- Sys.getenv("FG_STATCAST_CACHE_PATH", "data/fangraphs_batters_statcast_2018_2025.csv")

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

fg_sc <- lapply(seq(start_season, end_season), function(season) {
  message(sprintf("Fetching Statcast exit-velo leaderboard for %d", season))
  tryCatch(
    statcast_leaderboards(
      leaderboard = "exit_velocity_barrels",
      year = season,
      player_type = "batter",
      min_pa = 1,
      abs = 0
    ),
    error = function(e) {
      message(sprintf("Failed to fetch Statcast season %d: %s", season, e$message))
      NULL
    }
  )
})

fg_sc <- bind_rows(fg_sc)

base_cols <- c(
  "Season", "PlayerName", "playerid", "Age", "position",
  "Team", "PA", "H", "R", "RBI", "HR", "SB", "AVG", "OBP", "SLG"
)

needed_cols <- c(
  base_cols,
  "EV", "LA", "BarrelPct", "HardHitPct", "Events"
)

if (!"Team" %in% names(fg)) {
  fg <- fg %>% mutate(Team = dplyr::coalesce(team_name_abb, team_name))
}

missing_cols <- setdiff(base_cols, names(fg))
if (length(missing_cols) > 0) {
  stop("Missing expected columns from FanGraphs: ", paste(missing_cols, collapse = ", "))
}

norm_name <- function(x) {
  tolower(gsub("[^a-z0-9]", "", x))
}

pick_stat_col <- function(df, aliases) {
  nm <- names(df)
  nrm <- norm_name(nm)
  alias_nrm <- norm_name(aliases)
  idx <- which(nrm %in% alias_nrm)
  if (length(idx) == 0) return(NULL)
  nm[[idx[[1]]]]
}

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}

if (nrow(fg_sc) == 0) {
  if (file.exists(statcast_cache_path)) {
    message(sprintf("Statcast fetch failed; using cached statcast file: %s", statcast_cache_path))
    fg_sc <- read_csv(statcast_cache_path, show_col_types = FALSE)
  } else {
    stop(
      paste0(
        "No statcast leaderboard data returned. ",
        "This is often a connectivity/API issue, not a column issue. ",
        "If needed, provide a cache file via FG_STATCAST_CACHE_PATH (default: ",
        statcast_cache_path, ")."
      )
    )
  }
}

ev_col <- pick_stat_col(fg_sc, c("EV", "AvgEV", "average_ev", "avg_exit_velocity", "avg_hit_speed"))
la_col <- pick_stat_col(fg_sc, c("LA", "AvgLA", "average_la", "avg_launch_angle", "avg_hit_angle"))
barrel_col <- pick_stat_col(fg_sc, c("Barrel%", "Barrel_pct", "barrel_percent", "barrels_per_pa", "barrelpa", "brl_percent", "brl_pa"))
hardhit_col <- pick_stat_col(fg_sc, c("HardHit%", "HardHit_pct", "hardhit_percent", "hardhit", "hard_hit_percent", "ev95percent", "ev95plus_percent"))
events_col <- pick_stat_col(fg_sc, c("Events", "BIP", "batted_ball_events", "bbe", "batted_ball_event", "attempts"))
season_col <- pick_stat_col(fg_sc, c("Season", "year"))
playerid_col <- pick_stat_col(fg_sc, c("playerid", "player_id", "id"))

if (is.null(season_col) || is.null(playerid_col) || is.null(ev_col) || is.null(la_col) || is.null(barrel_col) || is.null(hardhit_col) || is.null(events_col)) {
  stop(
    "Could not map all Statcast columns from leaderboard response. Found columns: ",
    paste(names(fg_sc), collapse = ", ")
  )
}

fg_sc_small <- fg_sc %>%
  transmute(
    Season = as.integer(.data[[season_col]]),
    mlbam_id = as.numeric(.data[[playerid_col]]),
    EV = as.numeric(.data[[ev_col]]),
    LA = as.numeric(.data[[la_col]]),
    BarrelPct = as.numeric(gsub("%", "", as.character(.data[[barrel_col]]))),
    HardHitPct = as.numeric(gsub("%", "", as.character(.data[[hardhit_col]]))),
    Events = as.numeric(.data[[events_col]])
  ) %>%
  distinct(Season, mlbam_id, .keep_all = TRUE)

if (!dir.exists(dirname(statcast_cache_path))) {
  dir.create(dirname(statcast_cache_path), recursive = TRUE, showWarnings = FALSE)
}
write_csv(fg_sc_small, statcast_cache_path)
message(sprintf("Wrote statcast cache: %s (%d rows)", statcast_cache_path, nrow(fg_sc_small)))

fg <- fg %>%
  mutate(
    Season = as.integer(Season),
    Age = as.numeric(Age),
    playerid = as.character(playerid),
    position = as.character(position),
    PA = as.numeric(PA)
  ) %>%
  filter(Season >= start_season, Season <= end_season)

fg_mlbam_col <- pick_col(fg, c("xMLBAMID", "xMLBAMID.1", "mlbam_id", "MLBAMID"))
if (!is.null(fg_mlbam_col)) {
  fg <- fg %>% mutate(mlbam_id = suppressWarnings(as.numeric(.data[[fg_mlbam_col]])))
} else {
  fg <- fg %>% mutate(mlbam_id = NA_real_)
}

if (!"mlbam_id" %in% names(fg) || all(is.na(fg$mlbam_id))) {
  warning("xMLBAMID missing in batting pull; falling back to join by playerid and season.")
  fg_sc_small <- fg_sc_small %>% rename(playerid = mlbam_id) %>% mutate(playerid = as.character(playerid))
  fg <- fg %>%
    left_join(fg_sc_small, by = c("Season", "playerid"), suffix = c("", "_sc"))
} else {
  fg <- fg %>%
    left_join(fg_sc_small, by = c("Season", "mlbam_id"), suffix = c("", "_sc"))
}

for (v in c("EV", "LA", "BarrelPct", "HardHitPct", "Events")) {
  scv <- paste0(v, "_sc")
  if (!(v %in% names(fg))) fg[[v]] <- NA_real_
  if (!(scv %in% names(fg))) fg[[scv]] <- NA_real_
  fg[[v]] <- dplyr::coalesce(fg[[scv]], suppressWarnings(as.numeric(fg[[v]])))
}

fg <- fg %>%
  select(-any_of(c("EV_sc", "LA_sc", "BarrelPct_sc", "HardHitPct_sc", "Events_sc")))

# Keep seasons with PA >= 100
fg <- fg %>% filter(PA >= 100)

if (!is.na(require_recent) && require_recent == 1) {
  # Keep hitters with >=100 PA in either 2024 or 2025
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
}

out <- fg %>% select(all_of(needed_cols))

write_csv(out, output_path)
message(sprintf("Wrote %s (%d rows, %d cols)", output_path, nrow(out), ncol(out)))
