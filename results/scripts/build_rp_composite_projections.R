suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

proj_path <- "results/projections/pitchers/rp_category_projections_2026.csv"
atc_ip_path <- "data/atc_ip_projections_2026.csv"
out_path <- "results/projections/pitchers/rp_composite_projections_2026.csv"

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
  left_join(atc, by = "playerid", suffix = c("", "_atc")) %>%
  mutate(IP_atc = coalesce(IP_atc, IP_atc_atc)) %>%
  select(-any_of("IP_atc_atc")) %>%
  filter(!is.na(IP_atc))

proj <- proj %>%
  mutate(
    ERA = ER_mean * 9,
    WHIP = BB_mean + H_mean,
    Ks = SO_mean * IP_atc,
    W = W_mean * IP_atc,
    SVH = SVHLD_mean * IP_atc
  )

zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

proj <- proj %>%
  mutate(
    z_ERA = -zscore(ERA),
    z_WHIP = -zscore(WHIP),
    z_IP = zscore(IP_atc),
    z_W = zscore(W),
    z_Ks = zscore(Ks),
    z_SVH = zscore(SVH),
    composite = (z_ERA + z_WHIP + z_IP + z_W + z_Ks + z_SVH) / 6
  )

write_csv(
  proj %>%
    select(
      playerid, PlayerName, role,
      ERA, WHIP, Ks, W, SVH, IP_atc,
      z_ERA, z_WHIP, z_IP, z_W, z_Ks, z_SVH, composite
    ),
  out_path
)

cat("Wrote", out_path, "\n")
