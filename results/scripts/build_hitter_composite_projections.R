library(readr)
library(dplyr)

proj <- read_csv('results/projections/batters/category_projections_2026.csv', show_col_types = FALSE)
atc <- read_csv('data/atc_pa_projections_2026.csv', show_col_types = FALSE)
unc_path <- 'results/projections/batters/composite_rank_2026.csv'

need_cols <- c('playerid','PlayerName','position',
               'HR_mean','R_mean','RBI_mean','SB_mean','OBP_mean','SLG_mean')
missing <- setdiff(need_cols, names(proj))
if (length(missing) > 0) {
  stop('Missing columns in category projections: ', paste(missing, collapse=', '))
}

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}

id_col <- pick_col(atc, c("playerid", "PlayerId", "player_id"))
pa_col <- pick_col(atc, c("PA", "pa"))
team_col <- pick_col(atc, c("Team", "team"))
if (is.null(id_col) || is.null(pa_col) || is.null(team_col)) {
  stop("ATC projections must include playerid, PA, and Team.")
}

vals <- proj %>%
  mutate(playerid = as.character(playerid)) %>%
  left_join(
    atc %>% transmute(
      playerid = as.character(.data[[id_col]]),
      Team = as.character(.data[[team_col]]),
      PA_atc = as.numeric(.data[[pa_col]])
    ),
    by = "playerid",
    suffix = c("", "_atc")
  ) %>%
  mutate(PA_atc = coalesce(PA_atc, PA_atc_atc)) %>%
  select(-any_of("PA_atc_atc")) %>%
  filter(!is.na(PA_atc)) %>%
  transmute(
    playerid,
    PlayerName,
    Team,
    position,
    PA_atc,
    HR = ifelse(!is.na(HR_mean_t), HR_mean_t, HR_mean * PA_atc),
    R = ifelse(!is.na(R_mean_t), R_mean_t, R_mean * PA_atc),
    RBI = ifelse(!is.na(RBI_mean_t), RBI_mean_t, RBI_mean * PA_atc),
    SB = ifelse(!is.na(SB_mean_t), SB_mean_t, SB_mean * PA_atc),
    OBP = OBP_mean,
    SLG = SLG_mean
  )

zscore <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sdv <- sd(x, na.rm = TRUE)
  if (is.na(sdv) || sdv == 0) return(rep(NA_real_, length(x)))
  (x - mu) / sdv
}

vals_z <- vals %>%
  mutate(
    z_HR = zscore(HR),
    z_R = zscore(R),
    z_RBI = zscore(RBI),
    z_SB = zscore(SB),
    z_OBP = zscore(OBP),
    z_SLG = zscore(SLG)
  ) %>%
  mutate(composite_zscore = (z_HR + z_R + z_RBI + z_SB + z_OBP + z_SLG) / 6)

if (file.exists(unc_path)) {
  unc <- read_csv(unc_path, show_col_types = FALSE) %>%
    mutate(playerid = as.character(playerid)) %>%
    select(
      playerid,
      z_HR = z_HR_p50,
      z_R = z_R_p50,
      z_RBI = z_RBI_p50,
      z_SB = z_SB_p50,
      z_OBP = z_OBP_p50,
      z_SLG = z_SLG_p50,
      composite_zscore = composite_p50
    )
  vals_z <- vals_z %>%
    left_join(unc, by = "playerid", suffix = c("", "_unc")) %>%
    mutate(
      z_HR = coalesce(z_HR_unc, z_HR),
      z_R = coalesce(z_R_unc, z_R),
      z_RBI = coalesce(z_RBI_unc, z_RBI),
      z_SB = coalesce(z_SB_unc, z_SB),
      z_OBP = coalesce(z_OBP_unc, z_OBP),
      z_SLG = coalesce(z_SLG_unc, z_SLG),
      composite_zscore = coalesce(composite_zscore_unc, composite_zscore)
    ) %>%
    select(-ends_with("_unc"))
}

out <- vals_z %>%
  select(playerid, PlayerName, Team, position, PA_atc, composite_zscore,
         z_HR, z_R, z_RBI, z_SB, z_OBP, z_SLG)

write_csv(out, 'results/projections/batters/composite_projections_2026.csv')
cat('Wrote results/projections/batters/composite_projections_2026.csv\n')
