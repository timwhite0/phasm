suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(rstan)
})

fit_path <- "models/model_fit.rds"
prep_path <- "models/model_inputs.rds"
input_path <- "data/fangraphs_batters_2018_2025.csv"
output_projection_path <- "results/projections/batters/category_projections_2026.csv"

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

proj <- player_lookup %>%
  left_join(latest_lookup, by = c("playerid", "PlayerName")) %>%
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

write_csv(proj, output_projection_path)
