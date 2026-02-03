suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(rstan)
})

fit_path <- "models/rp_model_fit.rds"
prep_path <- "models/rp_model_inputs.rds"
output_projection_path <- "results/projections/pitchers/rp_category_projections_2026.csv"

count_outcomes <- c("SO", "BB", "H", "ER", "W", "SVHLD")

fit <- readRDS(fit_path)
prep <- readRDS(prep_path)

player_lookup <- prep$player_lookup %>%
  mutate(
    playerid = as.character(playerid),
    role = if_else(is.na(Role) | Role == "", "UNK", Role)
  ) %>%
  select(playerid, PlayerName, role)

eta_pred <- rstan::extract(fit, pars = "eta_pred")$eta_pred
rate_count <- exp(eta_pred)

summarize_draws <- function(draws_mat) {
  tibble(
    mean = apply(draws_mat, 2, mean),
    p05 = apply(draws_mat, 2, quantile, probs = 0.05),
    p50 = apply(draws_mat, 2, quantile, probs = 0.50),
    p95 = apply(draws_mat, 2, quantile, probs = 0.95)
  )
}

proj <- player_lookup
for (k in seq_along(count_outcomes)) {
  summary_k <- summarize_draws(rate_count[, , k])
  names(summary_k) <- paste0(count_outcomes[k], "_", names(summary_k))
  proj <- bind_cols(proj, summary_k)
}

write_csv(proj, output_projection_path)
