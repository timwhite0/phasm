library(readr)
library(dplyr)

proj <- read_csv('results/projections/category_projections_2026.csv', show_col_types = FALSE)
atc <- read_csv('data/atc_pa_projections_2026.csv', show_col_types = FALSE)

need_cols <- c('playerid','PlayerName','position',
               'HR_mean','R_mean','RBI_mean','SB_mean','OBP_mean','SLG_mean')
missing <- setdiff(need_cols, names(proj))
if (length(missing) > 0) {
  stop('Missing columns in category projections: ', paste(missing, collapse=', '))
}

if (!"PlayerId" %in% names(atc) || !"PA" %in% names(atc)) {
  stop("ATC projections must include PlayerId and PA.")
}

vals <- proj %>%
  mutate(playerid = as.character(playerid)) %>%
  left_join(
    atc %>% transmute(playerid = as.character(PlayerId), PA_atc = as.numeric(PA)),
    by = "playerid"
  ) %>%
  filter(!is.na(PA_atc)) %>%
  transmute(
    playerid,
    PlayerName,
    position,
    PA_atc,
    HR = HR_mean * PA_atc,
    R = R_mean * PA_atc,
    RBI = RBI_mean * PA_atc,
    SB = SB_mean * PA_atc,
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

out <- vals_z %>%
  select(playerid, PlayerName, position, PA_atc, composite_zscore,
         z_HR, z_R, z_RBI, z_SB, z_OBP, z_SLG)

write_csv(out, 'results/projections/composite_projections_2026.csv')
cat('Wrote results/projections/composite_projections_2026.csv\n')
