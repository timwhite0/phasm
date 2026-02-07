suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(rstan)
})

rstan_options(auto_write = TRUE)
cores <- 8
options(mc.cores = cores)
message(sprintf("detectCores=%d, mc.cores=%d", cores, getOption("mc.cores")))

# Config
input_path <- Sys.getenv("BATTER_INPUT_PATH", "data/fangraphs_batters_2018_2025.csv")
stan_path <- Sys.getenv("BATTER_STAN_PATH", "models/hitter_model.stan")
output_projection_path <- Sys.getenv("BATTER_OUTPUT_PROJECTION_PATH", "results/projections/batters/category_projections_2026.csv")
output_fit_path <- Sys.getenv("BATTER_OUTPUT_FIT_PATH", "models/hitter_model_fit.rds")
output_prep_path <- Sys.getenv("BATTER_OUTPUT_PREP_PATH", "models/hitter_model_inputs.rds")
batter_eb_summary_path <- Sys.getenv("BATTER_EB_SUMMARY_PATH", "results/prior_predictive/batter_prior_summary.csv")

run_fit <- TRUE
chains <- as.integer(Sys.getenv("STAN_CHAINS", "4"))
iter <- as.integer(Sys.getenv("STAN_ITER", "1000"))
warmup <- as.integer(Sys.getenv("STAN_WARMUP", "500"))
seed <- as.integer(Sys.getenv("STAN_SEED", "123"))
refresh <- as.integer(Sys.getenv("STAN_REFRESH", "100"))
subset_players <- as.integer(Sys.getenv("STAN_SUBSET_PLAYERS", "0"))

# Outcomes
count_outcomes <- c("H", "R", "RBI", "HR", "SB")
cont_outcomes <- c("AVG", "OBP", "SLG")

# Helpers
logit <- function(x) log(x / (1 - x))
inv_logit <- function(x) 1 / (1 + exp(-x))

# Load data
raw <- read_csv(input_path, show_col_types = FALSE)

# Basic cleaning
raw <- raw %>% mutate(Season = as.integer(Season))

# Optional subset for faster testing
if (!is.na(subset_players) && subset_players > 0) {
  set.seed(seed)
  all_ids <- raw %>% distinct(playerid) %>% pull(playerid)
  keep_ids <- sample(all_ids, size = min(subset_players, length(all_ids)), replace = FALSE)
  raw <- raw %>% filter(playerid %in% keep_ids)
}


# Age features (centered)
age_mean <- mean(raw$Age, na.rm = TRUE)
age_sd <- sd(raw$Age, na.rm = TRUE)
raw <- raw %>%
  mutate(
    age_c = (Age - age_mean) / age_sd,
    age2 = age_c^2
  )

# Rebuild IDs after filtering
raw <- raw %>%
  mutate(
    player_id = as.integer(factor(playerid)),
    pos_raw = if_else(is.na(position) | position == "", "UNK", position),
    pos_id = as.integer(factor(pos_raw))
  )

# Fixed effects design matrix
X <- cbind(
  intercept = 1,
  age_c = raw$age_c,
  age2 = raw$age2
)

# Random effects design matrix (intercept, age, age2) for position
Z <- cbind(
  intercept = 1,
  age_c = raw$age_c,
  age2 = raw$age2
)

# Player random effects: intercept + age only (no age2)
Z_player <- cbind(
  intercept = 1,
  age_c = raw$age_c
)

# Prior settings (defaults)
all_outcomes <- c(count_outcomes, cont_outcomes)
beta_mean <- matrix(0, nrow = ncol(X), ncol = length(all_outcomes))
beta_sd <- matrix(2.5, nrow = ncol(X), ncol = length(all_outcomes))
sigma_player_sd <- rep(1, length(all_outcomes))
sigma_pos_sd <- rep(1, length(all_outcomes))
sigma_year_sd <- rep(1, length(all_outcomes))
rho_year_mean <- rep(0, length(all_outcomes))
rho_year_sd <- rep(0.5, length(all_outcomes))
sigma_cont_sd <- rep(1, length(cont_outcomes))

if (!file.exists(batter_eb_summary_path)) {
  message("EB summary file not found; falling back to old batter priors: ", batter_eb_summary_path)
} else {
  eb_ok <- TRUE
  eb_err <- NULL
  tryCatch({
    eb <- read_csv(batter_eb_summary_path, show_col_types = FALSE)
    outcome_order <- all_outcomes
    beta_rows <- colnames(X)

    beta_eb <- eb %>%
      filter(param == "beta") %>%
      mutate(
        dim1 = factor(dim1, levels = beta_rows),
        dim2 = factor(dim2, levels = outcome_order)
      ) %>%
      arrange(dim1, dim2)
    if (nrow(beta_eb) != length(beta_rows) * length(outcome_order)) {
      stop("missing beta rows")
    }
    beta_mean <- matrix(beta_eb$mean, nrow = length(beta_rows), byrow = TRUE)
    beta_sd <- matrix(pmax(beta_eb$sd, 0.01), nrow = length(beta_rows), byrow = TRUE)

    # Convert posterior mean sigma to equivalent half-normal scale parameter.
    hn_scale <- sqrt(pi / 2)

    sigma_player_eb <- eb %>%
      filter(param == "sigma_player") %>%
      group_by(dim2) %>%
      summarize(mean = mean(mean, na.rm = TRUE), .groups = "drop") %>%
      mutate(dim2 = factor(dim2, levels = outcome_order)) %>%
      arrange(dim2)
    if (nrow(sigma_player_eb) != length(outcome_order)) {
      stop("missing sigma_player rows")
    }
    sigma_player_sd <- pmax(sigma_player_eb$mean * hn_scale, 0.01)

    sigma_pos_eb <- eb %>%
      filter(param == "sigma_pos") %>%
      group_by(dim2) %>%
      summarize(mean = mean(mean, na.rm = TRUE), .groups = "drop") %>%
      mutate(dim2 = factor(dim2, levels = outcome_order)) %>%
      arrange(dim2)
    if (nrow(sigma_pos_eb) != length(outcome_order)) {
      stop("missing sigma_pos rows")
    }
    sigma_pos_sd <- pmax(sigma_pos_eb$mean * hn_scale, 0.01)

    sigma_year_eb <- eb %>%
      filter(param == "sigma_year") %>%
      mutate(dim1 = factor(dim1, levels = outcome_order)) %>%
      arrange(dim1)
    if (nrow(sigma_year_eb) != length(outcome_order)) {
      stop("missing sigma_year rows")
    }
    sigma_year_sd <- pmax(sigma_year_eb$mean * hn_scale, 0.01)

    rho_year_eb <- eb %>%
      filter(param == "rho_year") %>%
      mutate(dim1 = factor(dim1, levels = outcome_order)) %>%
      arrange(dim1)
    if (nrow(rho_year_eb) != length(outcome_order)) {
      stop("missing rho_year rows")
    }
    rho_year_mean <- rho_year_eb$mean
    rho_year_sd <- pmax(rho_year_eb$sd, 0.05)

    sigma_cont_eb <- eb %>%
      filter(param == "sigma_cont") %>%
      mutate(dim1 = factor(dim1, levels = cont_outcomes)) %>%
      arrange(dim1)
    if (nrow(sigma_cont_eb) != length(cont_outcomes)) {
      stop("missing sigma_cont rows")
    }
    sigma_cont_sd <- pmax(sigma_cont_eb$mean * hn_scale, 0.01)
  }, error = function(e) {
    eb_ok <<- FALSE
    eb_err <<- conditionMessage(e)
  })

  if (eb_ok) {
    message("Using EB priors from: ", batter_eb_summary_path)
  } else {
    message("EB summary is invalid; falling back to old batter priors: ", batter_eb_summary_path, " (", eb_err, ")")
  }
}

# Outcomes
count_mat <- raw %>%
  select(all_of(count_outcomes)) %>%
  mutate(across(everything(), ~as.integer(round(.x)))) %>%
  as.matrix()

# Continuous outcomes with transforms
cont_df <- raw %>% select(all_of(cont_outcomes))

epsilon <- 1e-4
avg_t <- logit(pmin(pmax(cont_df$AVG, epsilon), 1 - epsilon))
obp_t <- logit(pmin(pmax(cont_df$OBP, epsilon), 1 - epsilon))
slg_t <- log(pmax(cont_df$SLG, 0) + epsilon)

y_cont <- cbind(avg_t, obp_t, slg_t)

# Year index
years <- sort(unique(raw$Season))
year_id <- match(raw$Season, years)

# Offset (rate per PA)
offset_log_pa <- log(pmax(raw$PA, 1))

# Build prediction set for 2026 using most recent season per player
latest_by_player <- raw %>%
  group_by(playerid) %>%
  slice_max(order_by = Season, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    Season = 2026L,
    Age = Age + 1
  )

latest_by_player <- latest_by_player %>%
  mutate(
    age_c = (Age - age_mean) / age_sd,
    age2 = age_c^2,
    pos_raw = if_else(is.na(position) | position == "", "UNK", position),
    pos_id = as.integer(factor(pos_raw, levels = levels(factor(raw$pos_raw))))
  )

X_pred <- cbind(
  intercept = 1,
  age_c = latest_by_player$age_c,
  age2 = latest_by_player$age2
)

Z_pred <- cbind(
  intercept = 1,
  age_c = latest_by_player$age_c,
  age2 = latest_by_player$age2
)

Z_player_pred <- cbind(
  intercept = 1,
  age_c = latest_by_player$age_c
)

# Align matrix columns between training and prediction
X_pred <- X_pred[, colnames(X), drop = FALSE]

# Create prediction IDs
player_id_pred <- as.integer(factor(latest_by_player$playerid, levels = levels(factor(raw$playerid))))
pos_id_pred <- latest_by_player$pos_id
year_id_pred <- rep(length(years) + 1, nrow(latest_by_player))
offset_log_pa_pred <- log(pmax(latest_by_player$PA, 1))

# Stan data
stan_data <- list(
  N = nrow(raw),
  K = length(c(count_outcomes, cont_outcomes)),
  K_count = length(count_outcomes),
  K_cont = length(cont_outcomes),
  P = ncol(X),
  R = ncol(Z),
  R_player = ncol(Z_player),
  X = X,
  Z = Z,
  Z_player = Z_player,
  beta_mean = beta_mean,
  beta_sd = beta_sd,
  sigma_player_sd = sigma_player_sd,
  sigma_pos_sd = sigma_pos_sd,
  sigma_year_sd = sigma_year_sd,
  rho_year_mean = rho_year_mean,
  rho_year_sd = rho_year_sd,
  sigma_cont_sd = sigma_cont_sd,
  J_player = length(unique(raw$player_id)),
  J_pos = length(unique(raw$pos_id)),
  J_year = length(years),
  player_id = raw$player_id,
  pos_id = raw$pos_id,
  year_id = year_id,
  y_count = count_mat,
  offset_log_pa = offset_log_pa,
  y_cont = y_cont,
  N_pred = nrow(latest_by_player),
  X_pred = X_pred,
  Z_pred = Z_pred,
  Z_player_pred = Z_player_pred,
  player_id_pred = player_id_pred,
  pos_id_pred = pos_id_pred,
  year_id_pred = year_id_pred,
  offset_log_pa_pred = offset_log_pa_pred
)

saveRDS(
  list(
    stan_data = stan_data,
    years = years,
    age_mean = age_mean,
    age_sd = age_sd,
    player_lookup = latest_by_player %>% select(playerid, PlayerName)
  ),
  output_prep_path
)

if (run_fit) {
  fit <- stan(
    file = stan_path,
    data = stan_data,
    chains = chains,
    iter = iter,
    warmup = warmup,
    seed = seed,
    refresh = refresh,
    control = list(adapt_delta = 0.9, max_treedepth = 12)
  )
  saveRDS(fit, output_fit_path)
} else {
  fit <- readRDS(output_fit_path)
}

# Extract predictions
eta_pred <- rstan::extract(fit, pars = "eta_pred")$eta_pred

# Summarize predictions per player
# Outcomes order: H, R, RBI, HR, SB, AVG, OBP, SLG
# Count outcomes: exp(eta) gives per-PA rate
rate_count <- exp(eta_pred[, , 1:length(count_outcomes)])
avg_pred <- inv_logit(eta_pred[, , length(count_outcomes) + 1])
obp_pred <- inv_logit(eta_pred[, , length(count_outcomes) + 2])
slg_pred <- pmax(exp(eta_pred[, , length(count_outcomes) + 3]) - epsilon, 0)

summarize_draws <- function(draws_mat) {
  tibble(
    mean = apply(draws_mat, 2, mean),
    p05 = apply(draws_mat, 2, quantile, probs = 0.05),
    p50 = apply(draws_mat, 2, quantile, probs = 0.50),
    p95 = apply(draws_mat, 2, quantile, probs = 0.95)
  )
}

proj <- latest_by_player %>%
  transmute(
    playerid,
    PlayerName,
    position = pos_raw
  ) %>%
  distinct()

# Add count outcomes
for (k in seq_along(count_outcomes)) {
  summary_k <- summarize_draws(rate_count[, , k])
  names(summary_k) <- paste0(count_outcomes[k], "_", names(summary_k))
  proj <- bind_cols(proj, summary_k)
}

# Add continuous outcomes
summary_avg <- summarize_draws(avg_pred)
names(summary_avg) <- paste0("AVG_", names(summary_avg))
summary_obp <- summarize_draws(obp_pred)
names(summary_obp) <- paste0("OBP_", names(summary_obp))
summary_slg <- summarize_draws(slg_pred)
names(summary_slg) <- paste0("SLG_", names(summary_slg))

proj <- bind_cols(proj, summary_avg, summary_obp, summary_slg)

write_csv(proj, output_projection_path)
