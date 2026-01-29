library(readr)
library(dplyr)

proj <- read_csv('results/projections/category_projections_2026.csv', show_col_types = FALSE)

need_cols <- c('playerid','PlayerName','position','PA_atc',
               'HR_total_mean','R_total_mean','RBI_total_mean','SB_total_mean',
               'OBP_mean','SLG_mean')
missing <- setdiff(need_cols, names(proj))
if (length(missing) > 0) {
  stop('Missing columns: ', paste(missing, collapse=', '))
}

vals <- proj %>%
  transmute(
    playerid = as.character(playerid),
    PlayerName,
    position,
    PA_atc,
    HR = HR_total_mean,
    R = R_total_mean,
    RBI = RBI_total_mean,
    SB = SB_total_mean,
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
