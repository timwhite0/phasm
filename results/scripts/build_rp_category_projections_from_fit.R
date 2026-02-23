suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(rstan)
})

fit_path <- "models/rp_model_fit.rds"
eta_pred_path <- Sys.getenv("RP_ETA_PRED_PATH", "models/rp_eta_pred_2026.rds")
prep_path <- "models/rp_model_inputs.rds"
output_projection_path <- "results/projections/pitchers/rp_category_projections_2026.csv"
atc_ip_path <- "data/atc_ip_projections_2026.csv"
ppd_seed <- as.integer(Sys.getenv("RP_PPD_SEED", "123"))
ip_cv <- as.numeric(Sys.getenv("RP_IP_ATC_CV", "0.15"))

count_outcomes <- c("SO", "BB", "H", "ER", "W", "SVHLD")

prep <- readRDS(prep_path)
set.seed(ppd_seed)

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

sample_ip_draws <- function(ip_vec, n_draw, cv) {
  if (!is.finite(cv) || cv <= 0) {
    return(matrix(rep(ip_vec, each = n_draw), nrow = n_draw))
  }
  shape <- 1 / (cv^2)
  scale_vec <- ip_vec / shape
  matrix(
    rgamma(n_draw * length(ip_vec), shape = shape, scale = rep(scale_vec, each = n_draw)),
    nrow = n_draw
  )
}

atc <- read_csv(atc_ip_path, show_col_types = FALSE)
id_col <- pick_col(atc, c("playerid", "PlayerId", "player_id"))
ip_col <- pick_col(atc, c("IP", "ip"))
team_col <- pick_col(atc, c("Team", "team"))
if (is.null(id_col) || is.null(ip_col) || is.null(team_col)) {
  stop("ATC IP file must include playerid, IP, and Team columns: ", atc_ip_path)
}
atc <- atc %>%
  transmute(
    playerid = as.character(.data[[id_col]]),
    Team = as.character(.data[[team_col]]),
    IP_atc = as.numeric(.data[[ip_col]])
  ) %>%
  filter(!is.na(playerid), !is.na(IP_atc))

proj <- player_lookup %>%
  left_join(atc, by = "playerid") %>%
  filter(!is.na(IP_atc), IP_atc > 0)

if (nrow(proj) == 0) {
  stop("No projected players with positive IP_atc.")
}

if (file.exists(eta_pred_path)) {
  eta_obj <- readRDS(eta_pred_path)
  eta_pred <- eta_obj$eta_pred
} else {
  fit <- readRDS(fit_path)
  eta_pred <- rstan::extract(fit, pars = "eta_pred")$eta_pred
}
n_draw <- dim(eta_pred)[1]
rate_count <- exp(eta_pred)

lookup_ids <- as.character(prep$player_lookup$playerid)
keep_idx <- match(proj$playerid, lookup_ids)
if (any(is.na(keep_idx))) {
  stop("Could not align projected player ids to eta_pred indices.")
}

ip_mat <- sample_ip_draws(proj$IP_atc, n_draw, ip_cv)

draw_poisson <- function(rate_mat, exposure_mat) {
  lambda <- rate_mat * exposure_mat
  matrix(rpois(length(lambda), lambda), nrow = nrow(rate_mat), ncol = ncol(rate_mat))
}

so_count <- draw_poisson(rate_count[, keep_idx, match("SO", count_outcomes)], ip_mat)
bb_count <- draw_poisson(rate_count[, keep_idx, match("BB", count_outcomes)], ip_mat)
h_count <- draw_poisson(rate_count[, keep_idx, match("H", count_outcomes)], ip_mat)
er_count <- draw_poisson(rate_count[, keep_idx, match("ER", count_outcomes)], ip_mat)
w_count <- draw_poisson(rate_count[, keep_idx, match("W", count_outcomes)], ip_mat)
svhld_count <- draw_poisson(rate_count[, keep_idx, match("SVHLD", count_outcomes)], ip_mat)

so_rate_ppd <- so_count / ip_mat
bb_rate_ppd <- bb_count / ip_mat
h_rate_ppd <- h_count / ip_mat
er_rate_ppd <- er_count / ip_mat
w_rate_ppd <- w_count / ip_mat
svhld_rate_ppd <- svhld_count / ip_mat
whip_draws <- bb_rate_ppd + h_rate_ppd

summarize_draws <- function(draws_mat) {
  tibble(
    mean = apply(draws_mat, 2, mean),
    p05 = apply(draws_mat, 2, quantile, probs = 0.05),
    p50 = apply(draws_mat, 2, quantile, probs = 0.50),
    p95 = apply(draws_mat, 2, quantile, probs = 0.95)
  )
}

summary_map <- list(
  SO = so_rate_ppd,
  BB = bb_rate_ppd,
  H = h_rate_ppd,
  ER = er_rate_ppd,
  W = w_rate_ppd,
  SVHLD = svhld_rate_ppd
)

for (nm in names(summary_map)) {
  summary_k <- summarize_draws(summary_map[[nm]])
  names(summary_k) <- paste0(nm, "_", names(summary_k))
  proj <- bind_cols(proj, summary_k)
}

whip_summary <- summarize_draws(whip_draws)
names(whip_summary) <- paste0("WHIP_", names(whip_summary))
ks_summary <- summarize_draws(so_count)
names(ks_summary) <- paste0("Ks_", names(ks_summary))
w_total_summary <- summarize_draws(w_count)
names(w_total_summary) <- c("W_mean_t", "W_p05_t", "W_p50_t", "W_p95_t")
svhld_total_summary <- summarize_draws(svhld_count)
names(svhld_total_summary) <- c("SVHLD_mean_t", "SVHLD_p05_t", "SVHLD_p50_t", "SVHLD_p95_t")

proj <- bind_cols(proj, whip_summary, ks_summary, w_total_summary, svhld_total_summary)

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
    BB9_p95 = BB_p95 * 9
  )

write_csv(proj, output_projection_path)
