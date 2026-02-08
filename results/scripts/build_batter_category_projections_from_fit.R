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

count_outcomes <- c("H", "R", "RBI", "HR", "SB")
cont_outcomes <- c("AVG", "OBP", "SLG")
epsilon <- 1e-4

fit <- readRDS(fit_path)
prep <- readRDS(prep_path)

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
  mutate(position = if_else(is.na(position) | position == "", "UNK", position))

eta_pred <- rstan::extract(fit, pars = "eta_pred")$eta_pred

rate_count <- exp(eta_pred[, , 1:length(count_outcomes)])
avg_pred <- 1 / (1 + exp(-eta_pred[, , length(count_outcomes) + 1]))
obp_pred <- 1 / (1 + exp(-eta_pred[, , length(count_outcomes) + 2]))
slg_pred <- pmax(exp(eta_pred[, , length(count_outcomes) + 3]) - epsilon, 0)

summarize_draws <- function(draws_mat) {
  tibble(
    mean = apply(draws_mat, 2, mean),
    p05 = apply(draws_mat, 2, quantile, probs = 0.05),
    p50 = apply(draws_mat, 2, quantile, probs = 0.50),
    p95 = apply(draws_mat, 2, quantile, probs = 0.95)
  )
}

for (k in seq_along(count_outcomes)) {
  summary_k <- summarize_draws(rate_count[, , k])
  names(summary_k) <- paste0(count_outcomes[k], "_", names(summary_k))
  proj <- bind_cols(proj, summary_k)
}

summary_avg <- summarize_draws(avg_pred)
names(summary_avg) <- paste0("AVG_", names(summary_avg))
summary_obp <- summarize_draws(obp_pred)
names(summary_obp) <- paste0("OBP_", names(summary_obp))
summary_slg <- summarize_draws(slg_pred)
names(summary_slg) <- paste0("SLG_", names(summary_slg))

proj <- bind_cols(proj, summary_avg, summary_obp, summary_slg)

proj <- proj %>%
  mutate(
    H_mean_t = H_mean * PA_atc,
    H_p05_t = H_p05 * PA_atc,
    H_p50_t = H_p50 * PA_atc,
    H_p95_t = H_p95 * PA_atc,
    R_mean_t = R_mean * PA_atc,
    R_p05_t = R_p05 * PA_atc,
    R_p50_t = R_p50 * PA_atc,
    R_p95_t = R_p95 * PA_atc,
    RBI_mean_t = RBI_mean * PA_atc,
    RBI_p05_t = RBI_p05 * PA_atc,
    RBI_p50_t = RBI_p50 * PA_atc,
    RBI_p95_t = RBI_p95 * PA_atc,
    HR_mean_t = HR_mean * PA_atc,
    HR_p05_t = HR_p05 * PA_atc,
    HR_p50_t = HR_p50 * PA_atc,
    HR_p95_t = HR_p95 * PA_atc,
    SB_mean_t = SB_mean * PA_atc,
    SB_p05_t = SB_p05 * PA_atc,
    SB_p50_t = SB_p50 * PA_atc,
    SB_p95_t = SB_p95 * PA_atc
  )

write_csv(proj, output_projection_path)
