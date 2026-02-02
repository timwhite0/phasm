suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

proj_path <- "results/projections/pitchers/sp_category_projections_2026.csv"
atc_ip_path <- "data/atc_ip_projections_2026.csv"
out_path <- "results/projections/pitchers/sp_composite_projections_2026.csv"

proj <- read_csv(proj_path, show_col_types = FALSE) %>%
  mutate(playerid = as.character(playerid))

atc <- read_csv(atc_ip_path, show_col_types = FALSE)

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}

id_col <- pick_col(atc, c("playerid", "PlayerId", "player_id"))
ip_col <- pick_col(atc, c("IP", "ip"))
if (is.null(id_col) || is.null(ip_col)) {
  stop("Could not find playerid or IP column in ATC projections.")
}

atc <- atc %>%
  transmute(
    playerid = as.character(.data[[id_col]]),
    IP_atc = as.numeric(.data[[ip_col]])
  ) %>%
  filter(!is.na(playerid), !is.na(IP_atc))

proj <- proj %>%
  left_join(atc, by = "playerid") %>%
  filter(!is.na(IP_atc))

proj <- proj %>%
  mutate(
    ERA = ER_mean * 9,
    K9 = SO_mean * 9,
    WHIP = BB_mean + H_mean,
    Ks = SO_mean * IP_atc,
    WQS = W_mean + QS_mean
  )

zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

proj <- proj %>%
  mutate(
    z_ERA = -zscore(ERA),
    z_K9 = zscore(K9),
    z_WHIP = -zscore(WHIP),
    z_IP = zscore(IP_atc),
    z_WQS = zscore(WQS),
    composite = (z_ERA + z_K9 + z_WHIP + z_IP + z_WQS) / 5
  )

write_csv(
  proj %>%
    select(
      playerid, PlayerName, role,
      ERA, K9, WHIP, Ks, WQS, IP_atc,
      z_ERA, z_K9, z_WHIP, z_IP, z_WQS, composite
    ),
  out_path
)

cat("Wrote", out_path, "\n")
