suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(baseballr)
})

start_season <- 2018
end_season <- 2025
output_path <- "data/fangraphs_pitchers_2018_2025.csv"

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

fg <- lapply(seq(start_season, end_season), function(season) {
  message(sprintf("Fetching FanGraphs pitching leaders for %d", season))
  tryCatch(
    fg_pitch_leaders(
      startseason = as.character(season),
      endseason = as.character(season),
      stats = "pit",
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

out <- fg %>% select(
  Season, PlayerName, playerid, Age, Role, Team, IP, G, GS,
  SO, BB, H, ER, SV, HLD, SVHLD, role_leverage, W, QS
)

write_csv(out, output_path)
message(sprintf("Wrote %s (%d rows, %d cols)", output_path, nrow(out), ncol(out)))
