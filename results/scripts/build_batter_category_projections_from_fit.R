suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(rstan)
})

fit_path <- "models/hitter_model_fit.rds"
prep_path <- "models/hitter_model_inputs.rds"
input_path <- "data/fangraphs_batters_2018_2025.csv"
output_projection_path <- "results/projections/batters/category_projections_2026.csv"
atc_pa_path <- "data/atc_pa_projections_2026.csv"
ppd_seed <- as.integer(Sys.getenv("HITTER_PPD_SEED", "123"))
pa_cv <- as.numeric(Sys.getenv("HITTER_PA_ATC_CV", "0.10"))

count_outcomes <- c("H", "R", "RBI", "HR", "SB")
cont_outcomes <- c("AVG", "OBP", "SLG")
epsilon <- 1e-4

fit <- readRDS(fit_path)
prep <- readRDS(prep_path)
set.seed(ppd_seed)

raw <- read_csv(input_path, show_col_types = FALSE) %>%
  mutate(Season = as.integer(Season))

latest_by_player <- raw %>%
  group_by(playerid) %>%
  slice_max(order_by = Season, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(pos_raw = if_else(is.na(position) | position == "", "UNK", position))

player_lookup <- prep$player_lookup %>%
  mutate(playerid = as.character(playerid))

latest_lookup <- latest_by_player %>%
  transmute(
    playerid = as.character(playerid),
    PlayerName,
    position = pos_raw
  )

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}

atc <- read_csv(atc_pa_path, show_col_types = FALSE)
id_col <- pick_col(atc, c("playerid", "PlayerId", "player_id"))
pa_col <- pick_col(atc, c("PA", "pa"))
team_col <- pick_col(atc, c("Team", "team"))
if (is.null(id_col) || is.null(pa_col) || is.null(team_col)) {
  stop("ATC PA file must include playerid, PA, and Team columns: ", atc_pa_path)
}
atc <- atc %>%
  transmute(
    playerid = as.character(.data[[id_col]]),
    Team = as.character(.data[[team_col]]),
    PA_atc = as.numeric(.data[[pa_col]])
  ) %>%
  filter(!is.na(playerid), !is.na(PA_atc))

proj <- player_lookup %>%
  left_join(latest_lookup, by = c("playerid", "PlayerName")) %>%
  left_join(atc, by = "playerid") %>%
  mutate(position = if_else(is.na(position) | position == "", "UNK", position)) %>%
  filter(!is.na(PA_atc), PA_atc > 0)

has_phi_count <- "phi_count" %in% fit@sim$pars_oi
extract_pars <- if (has_phi_count) c("eta_pred", "phi_count") else "eta_pred"
post <- rstan::extract(fit, pars = extract_pars)
eta_pred <- post$eta_pred
phi_count <- post$phi_count
if (!has_phi_count) {
  warning("phi_count not found in fit; falling back to Poisson posterior predictive draws for counts.")
}
n_draw <- dim(eta_pred)[1]
lookup_ids <- as.character(prep$player_lookup$playerid)
keep_idx <- match(proj$playerid, lookup_ids)
if (any(is.na(keep_idx))) {
  stop("Could not align projected player ids to eta_pred indices.")
}
n_player <- length(keep_idx)

avg_pred <- 1 / (1 + exp(-eta_pred[, keep_idx, length(count_outcomes) + 1]))
obp_pred <- 1 / (1 + exp(-eta_pred[, keep_idx, length(count_outcomes) + 2]))
slg_pred <- pmax(exp(eta_pred[, keep_idx, length(count_outcomes) + 3]) - epsilon, 0)

sample_pa_draws <- function(pa_vec, n_draw, cv) {
  if (!is.finite(cv) || cv <= 0) {
    return(matrix(rep(pa_vec, each = n_draw), nrow = n_draw))
  }
  shape <- 1 / (cv^2)
  scale_vec <- pa_vec / shape
  matrix(
    rgamma(n_draw * length(pa_vec), shape = shape, scale = rep(scale_vec, each = n_draw)),
    nrow = n_draw
  )
}

pa_mat <- sample_pa_draws(proj$PA_atc, n_draw, pa_cv)
draw_counts <- function(rate_mat, exposure_mat, phi_draw = NULL) {
  lambda <- rate_mat * exposure_mat
  out <- matrix(NA_real_, nrow = nrow(rate_mat), ncol = ncol(rate_mat))
  valid <- is.finite(lambda) & lambda >= 0 & is.finite(exposure_mat) & exposure_mat > 0
  if (is.null(phi_draw)) {
    out[valid] <- rpois(sum(valid), lambda[valid])
    return(out)
  }
  phi_mat <- matrix(phi_draw, nrow = nrow(rate_mat), ncol = ncol(rate_mat))
  valid <- valid & is.finite(phi_mat) & phi_mat > 0
  out[valid] <- rnbinom(sum(valid), mu = lambda[valid], size = phi_mat[valid])
  out
}

count_rate_draws <- vector("list", length(count_outcomes))
count_total_draws <- vector("list", length(count_outcomes))
for (k in seq_along(count_outcomes)) {
  rate_k <- exp(eta_pred[, keep_idx, k])
  phi_k <- NULL
  if (!is.null(phi_count)) {
    if (is.matrix(phi_count)) {
      phi_k <- phi_count[, k]
    } else if (length(phi_count) == n_draw * length(count_outcomes)) {
      phi_k <- matrix(phi_count, nrow = n_draw, byrow = FALSE)[, k]
    }
  }
  count_k <- draw_counts(rate_k, pa_mat, phi_k)
  rate_ppd <- matrix(NA_real_, nrow = n_draw, ncol = n_player)
  valid <- is.finite(count_k) & is.finite(pa_mat) & pa_mat > 0
  rate_ppd[valid] <- count_k[valid] / pa_mat[valid]
  count_rate_draws[[k]] <- rate_ppd
  count_total_draws[[k]] <- count_k
}

summarize_draws <- function(draws_mat) {
  tibble(
    mean = apply(draws_mat, 2, mean),
    p05 = apply(draws_mat, 2, quantile, probs = 0.05),
    p50 = apply(draws_mat, 2, quantile, probs = 0.50),
    p95 = apply(draws_mat, 2, quantile, probs = 0.95)
  )
}

for (k in seq_along(count_outcomes)) {
  summary_k <- summarize_draws(count_rate_draws[[k]])
  names(summary_k) <- paste0(count_outcomes[k], "_", names(summary_k))
  proj <- bind_cols(proj, summary_k)

  summary_t <- summarize_draws(count_total_draws[[k]])
  names(summary_t) <- paste0(count_outcomes[k], "_", names(summary_t), "_t")
  proj <- bind_cols(proj, summary_t)
}

summary_avg <- summarize_draws(avg_pred)
names(summary_avg) <- paste0("AVG_", names(summary_avg))
summary_obp <- summarize_draws(obp_pred)
names(summary_obp) <- paste0("OBP_", names(summary_obp))
summary_slg <- summarize_draws(slg_pred)
names(summary_slg) <- paste0("SLG_", names(summary_slg))

proj <- bind_cols(proj, summary_avg, summary_obp, summary_slg)

write_csv(proj, output_projection_path)
