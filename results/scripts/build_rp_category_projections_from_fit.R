suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(rstan)
})

fit_path <- "models/rp_model_fit.rds"
prep_path <- "models/rp_model_inputs.rds"
output_projection_path <- "results/projections/pitchers/rp_category_projections_2026.csv"
atc_ip_path <- "data/atc_ip_projections_2026.csv"

count_outcomes <- c("SO", "BB", "H", "ER", "W", "SVHLD")

fit <- readRDS(fit_path)
prep <- readRDS(prep_path)

player_lookup <- prep$player_lookup %>%
  mutate(
    playerid = as.character(playerid),
    role = if_else(is.na(Role) | Role == "", "UNK", Role)
  ) %>%
  select(playerid, PlayerName, role)

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}

atc <- read_csv(atc_ip_path, show_col_types = FALSE)
id_col <- pick_col(atc, c("playerid", "PlayerId", "player_id"))
ip_col <- pick_col(atc, c("IP", "ip"))
if (is.null(id_col) || is.null(ip_col)) {
  stop("ATC IP file must include playerid and IP columns: ", atc_ip_path)
}
atc <- atc %>%
  transmute(playerid = as.character(.data[[id_col]]), IP_atc = as.numeric(.data[[ip_col]])) %>%
  filter(!is.na(playerid), !is.na(IP_atc))

eta_pred <- rstan::extract(fit, pars = "eta_pred")$eta_pred
rate_count <- exp(eta_pred)
bb_draws <- rate_count[, , match("BB", count_outcomes)]
h_draws <- rate_count[, , match("H", count_outcomes)]
whip_draws <- bb_draws + h_draws

summarize_draws <- function(draws_mat) {
  tibble(
    mean = apply(draws_mat, 2, mean),
    p05 = apply(draws_mat, 2, quantile, probs = 0.05),
    p50 = apply(draws_mat, 2, quantile, probs = 0.50),
    p95 = apply(draws_mat, 2, quantile, probs = 0.95)
  )
}

proj <- player_lookup %>%
  left_join(atc, by = "playerid")
for (k in seq_along(count_outcomes)) {
  summary_k <- summarize_draws(rate_count[, , k])
  names(summary_k) <- paste0(count_outcomes[k], "_", names(summary_k))
  proj <- bind_cols(proj, summary_k)
}

whip_summary <- summarize_draws(whip_draws)
names(whip_summary) <- paste0("WHIP_", names(whip_summary))
proj <- bind_cols(proj, whip_summary)

proj <- proj %>%
  mutate(
    ERA_mean = ER_mean * 9,
    ERA_p05 = ER_p05 * 9,
    ERA_p50 = ER_p50 * 9,
    ERA_p95 = ER_p95 * 9,
    K9_mean = SO_mean * 9,
    K9_p05 = SO_p05 * 9,
    K9_p50 = SO_p50 * 9,
    K9_p95 = SO_p95 * 9,
    BB9_mean = BB_mean * 9,
    BB9_p05 = BB_p05 * 9,
    BB9_p50 = BB_p50 * 9,
    BB9_p95 = BB_p95 * 9,
    Ks_mean = SO_mean * IP_atc,
    Ks_p05 = SO_p05 * IP_atc,
    Ks_p50 = SO_p50 * IP_atc,
    Ks_p95 = SO_p95 * IP_atc,
    W_mean_t = W_mean * IP_atc,
    W_p05_t = W_p05 * IP_atc,
    W_p50_t = W_p50 * IP_atc,
    W_p95_t = W_p95 * IP_atc,
    SVHLD_mean_t = SVHLD_mean * IP_atc,
    SVHLD_p05_t = SVHLD_p05 * IP_atc,
    SVHLD_p50_t = SVHLD_p50 * IP_atc,
    SVHLD_p95_t = SVHLD_p95 * IP_atc
  )

write_csv(proj, output_projection_path)
