suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(baseballr)
})

start_season <- as.integer(Sys.getenv("FG_START_SEASON", "2018"))
end_season <- as.integer(Sys.getenv("FG_END_SEASON", "2025"))
output_path <- Sys.getenv("FG_OUTPUT_PATH", "data/fangraphs_pitchers_2018_2025.csv")
require_recent <- as.integer(Sys.getenv("FG_REQUIRE_RECENT", "1"))
plv_type_override <- Sys.getenv("FG_PLV_TYPE", "")
plv_discovery_min <- as.integer(Sys.getenv("FG_PLV_DISCOVERY_MIN_TYPE", "0"))
plv_discovery_max <- as.integer(Sys.getenv("FG_PLV_DISCOVERY_MAX_TYPE", "60"))

convert_ip <- function(ip_raw) {
  ip_num <- suppressWarnings(as.numeric(ip_raw))
  whole <- floor(ip_num)
  frac <- round(ip_num - whole, 1)
  adj <- dplyr::case_when(
    is.na(frac) ~ NA_real_,
    abs(frac - 0.1) < 1e-6 ~ 1 / 3,
    abs(frac - 0.2) < 1e-6 ~ 2 / 3,
    TRUE ~ frac
  )
  whole + adj
}

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}

fetch_pitch_leaders <- function(season, type = "8") {
  fg_pitch_leaders(
    startseason = as.character(season),
    endseason = as.character(season),
    stats = "pit",
    pos = "all",
    lg = "all",
    qual = "0",
    ind = "1",
    pageitems = "5000",
    type = as.character(type)
  )
}

discover_plv_type <- function(season, type_min, type_max) {
  cand <- seq(type_min, type_max)
  for (tp in cand) {
    message(sprintf("Trying FanGraphs pitching leaderboard type=%d for Stuff+/Location+ discovery", tp))
    probe <- tryCatch(fetch_pitch_leaders(season = season, type = tp), error = function(e) NULL)
    if (is.null(probe) || nrow(probe) == 0) next
    stuff_col <- pick_col(probe, c("Stuff+", "StuffPlus", "stuff_plus", "Stuff", "Pitching+_Stuff", "plv_stuff_plus", "sp_stuff"))
    loc_col <- pick_col(probe, c("Location+", "LocationPlus", "location_plus", "Location", "Pitching+_Location", "plv_location_plus", "sp_location"))
    if (!is.null(stuff_col) && !is.null(loc_col)) {
      return(tp)
    }
  }
  NA_integer_
}

fg <- lapply(seq(start_season, end_season), function(season) {
  message(sprintf("Fetching FanGraphs pitching leaders for %d", season))
  tryCatch(
    fetch_pitch_leaders(season = season, type = 8),
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

if (!"Team" %in% names(fg)) {
  fg <- fg %>% mutate(Team = dplyr::coalesce(team_name_abb, team_name))
}

if (!"Role" %in% names(fg)) {
  if ("role" %in% names(fg)) {
    fg <- fg %>% mutate(Role = role)
  } else if ("player_role" %in% names(fg)) {
    fg <- fg %>% mutate(Role = player_role)
  } else {
    fg <- fg %>% mutate(Role = NA_character_)
  }
}

if (!"SO" %in% names(fg) && "K" %in% names(fg)) {
  fg <- fg %>% mutate(SO = K)
}

if (!"HLD" %in% names(fg) && "Holds" %in% names(fg)) {
  fg <- fg %>% mutate(HLD = Holds)
}

needed_cols <- c(
  "Season", "PlayerName", "playerid", "Age", "Role", "Team", "IP",
  "SO", "BB", "H", "ER", "SV", "HLD", "W", "QS", "G", "GS"
)

missing_cols <- setdiff(needed_cols, names(fg))
if (length(missing_cols) > 0) {
  stop("Missing expected columns from FanGraphs: ", paste(missing_cols, collapse = ", "))
}

fg <- fg %>%
  mutate(
    Season = as.integer(Season),
    Age = as.numeric(Age),
    playerid = as.character(playerid),
    Role = as.character(Role),
    G = as.numeric(G),
    GS = as.numeric(GS),
    IP = convert_ip(IP)
  ) %>%
  filter(Season >= start_season, Season <= end_season)

plv_type <- NA_integer_
if (nchar(plv_type_override) > 0) {
  plv_type <- suppressWarnings(as.integer(plv_type_override))
}
if (is.na(plv_type)) {
  plv_type <- discover_plv_type(season = end_season, type_min = plv_discovery_min, type_max = plv_discovery_max)
}

plv <- NULL
if (!is.na(plv_type)) {
  message(sprintf("Using FanGraphs leaderboard type=%d for Stuff+/Location+ pull", plv_type))
  plv <- lapply(seq(start_season, end_season), function(season) {
    message(sprintf("Fetching FanGraphs PLV leaders for %d (type=%d)", season, plv_type))
    tryCatch(fetch_pitch_leaders(season = season, type = plv_type), error = function(e) NULL)
  }) %>% bind_rows()
}

if (!is.null(plv) && nrow(plv) > 0) {
  plv_stuff_col <- pick_col(plv, c("Stuff+", "StuffPlus", "stuff_plus", "Stuff", "Pitching+_Stuff", "plv_stuff_plus", "sp_stuff"))
  plv_loc_col <- pick_col(plv, c("Location+", "LocationPlus", "location_plus", "Location", "Pitching+_Location", "plv_location_plus", "sp_location"))
  plv_bf_col <- pick_col(plv, c("BF", "TBF", "BFP", "BattersFaced", "Batters Faced", "batters_faced"))
  plv_mlbam_col <- pick_col(plv, c("xMLBAMID", "mlbam_id", "MLBAMID"))
  plv_pid_col <- pick_col(plv, c("playerid"))

  if (!is.null(plv_stuff_col) && !is.null(plv_loc_col)) {
    plv_small <- plv %>%
      transmute(
        Season = as.integer(Season),
        playerid = as.character(.data[[plv_pid_col]]),
        mlbam_id = if (!is.null(plv_mlbam_col)) suppressWarnings(as.numeric(.data[[plv_mlbam_col]])) else NA_real_,
        StuffPlus_sc = suppressWarnings(as.numeric(.data[[plv_stuff_col]])),
        LocationPlus_sc = suppressWarnings(as.numeric(.data[[plv_loc_col]])),
        BF_sc = if (!is.null(plv_bf_col)) suppressWarnings(as.numeric(.data[[plv_bf_col]])) else NA_real_
      ) %>%
      distinct(Season, playerid, .keep_all = TRUE)

    fg_mlbam_col <- pick_col(fg, c("xMLBAMID", "xMLBAMID.1", "mlbam_id", "MLBAMID"))
    if (!is.null(fg_mlbam_col)) {
      fg <- fg %>% mutate(mlbam_id = suppressWarnings(as.numeric(.data[[fg_mlbam_col]])))
    } else {
      fg <- fg %>% mutate(mlbam_id = NA_real_)
    }

    if (all(is.na(fg$mlbam_id))) {
      fg <- fg %>% left_join(plv_small, by = c("Season", "playerid"))
    } else {
      fg <- fg %>% left_join(plv_small, by = c("Season", "mlbam_id"), suffix = c("", "_pid"))
      if ("StuffPlus_sc_pid" %in% names(fg)) fg$StuffPlus_sc <- dplyr::coalesce(fg$StuffPlus_sc, fg$StuffPlus_sc_pid)
      if ("LocationPlus_sc_pid" %in% names(fg)) fg$LocationPlus_sc <- dplyr::coalesce(fg$LocationPlus_sc, fg$LocationPlus_sc_pid)
      if ("BF_sc_pid" %in% names(fg)) fg$BF_sc <- dplyr::coalesce(fg$BF_sc, fg$BF_sc_pid)
      fg <- fg %>% select(-any_of(c("playerid_pid", "StuffPlus_sc_pid", "LocationPlus_sc_pid", "BF_sc_pid")))
    }
  }
}

base_stuff_col <- pick_col(fg, c("Stuff+", "StuffPlus", "stuff_plus", "Stuff", "Pitching+_Stuff", "plv_stuff_plus", "sp_stuff"))
base_loc_col <- pick_col(fg, c("Location+", "LocationPlus", "location_plus", "Location", "Pitching+_Location", "plv_location_plus", "sp_location"))
base_bf_col <- pick_col(fg, c("BF", "TBF", "BFP", "BattersFaced", "Batters Faced", "batters_faced"))

fg <- fg %>%
  mutate(
    StuffPlus = dplyr::coalesce(
      if (!is.null(base_stuff_col)) suppressWarnings(as.numeric(.data[[base_stuff_col]])) else NA_real_,
      if ("StuffPlus_sc" %in% names(.)) suppressWarnings(as.numeric(StuffPlus_sc)) else NA_real_
    ),
    LocationPlus = dplyr::coalesce(
      if (!is.null(base_loc_col)) suppressWarnings(as.numeric(.data[[base_loc_col]])) else NA_real_,
      if ("LocationPlus_sc" %in% names(.)) suppressWarnings(as.numeric(LocationPlus_sc)) else NA_real_
    ),
    BF = dplyr::coalesce(
      if (!is.null(base_bf_col)) suppressWarnings(as.numeric(.data[[base_bf_col]])) else NA_real_,
      if ("BF_sc" %in% names(.)) suppressWarnings(as.numeric(BF_sc)) else NA_real_
    )
  ) %>%
  select(-any_of(c("StuffPlus_sc", "LocationPlus_sc", "BF_sc")))

if (all(is.na(fg$StuffPlus)) || all(is.na(fg$LocationPlus))) {
  warning(
    "StuffPlus/LocationPlus were not found from FanGraphs leaderboards. ",
    "Set FG_PLV_TYPE to the correct leaderboard type to force pull once identified."
  )
}

fg <- fg %>%
  mutate(
    Role = dplyr::case_when(
      grepl("SP|Starter", Role, ignore.case = TRUE) ~ "SP",
      grepl("RP|Reliever", Role, ignore.case = TRUE) ~ "RP",
      TRUE ~ Role
    )
  )

fg <- fg %>%
  mutate(
    Role = if_else(
      is.na(Role) | Role == "",
      if_else(!is.na(GS) & !is.na(G) & GS >= pmax(5, 0.5 * G), "SP", "RP"),
      Role
    )
  )

fg <- fg %>%
  mutate(SVHLD = SV + HLD) %>%
  group_by(Season, Team) %>%
  mutate(
    svhld_team_total = sum(SVHLD, na.rm = TRUE),
    svhld_rank = ifelse(svhld_team_total > 0, dense_rank(desc(SVHLD)), Inf),
    role_leverage = dplyr::case_when(
      grepl("^[3-9] Tms$", Team) ~ 0,
      Team == "2 Tms" ~ ifelse(svhld_team_total > 0 & svhld_rank <= 5, 1, 0),
      TRUE ~ ifelse(svhld_team_total > 0 & svhld_rank <= 3, 1, 0)
    ),
    role_leverage = ifelse(SVHLD >= 10, 1, role_leverage),
    role_leverage = ifelse(!is.na(IP) & IP > 0 & (SVHLD / IP) >= 0.3, 1, role_leverage)
  ) %>%
  ungroup()

# Keep seasons with IP >= 20
fg <- fg %>% filter(IP >= 20)

if (!is.na(require_recent) && require_recent == 1) {
  # Keep pitchers with >=20 IP in either 2024 or 2025
  keep_recent <- fg %>%
    group_by(playerid) %>%
    summarize(
      IP_2425_max = ifelse(
        any(Season %in% c(2024, 2025)),
        max(IP[Season %in% c(2024, 2025)], na.rm = TRUE),
        0
      ),
      .groups = "drop"
    ) %>%
    filter(IP_2425_max >= 20) %>%
    pull(playerid)

  fg <- fg %>% filter(playerid %in% keep_recent)
}

out <- fg %>% select(
  Season, PlayerName, playerid, Age, Role, Team, IP, G, GS,
  SO, BB, H, ER, SV, HLD, SVHLD, role_leverage, W, QS,
  StuffPlus, LocationPlus, BF
)

write_csv(out, output_path)
message(sprintf("Wrote %s (%d rows, %d cols)", output_path, nrow(out), ncol(out)))
