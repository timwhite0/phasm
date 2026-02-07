suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(rstan)
})

rstan_options(auto_write = TRUE)
cores <- 8
options(mc.cores = cores)
message(sprintf("detectCores=%d, mc.cores=%d", cores, getOption("mc.cores")))

# Config
input_path <- "data/fangraphs_pitchers_2013_2017.csv"
atc_ip_path <- "data/atc_ip_projections_2026.csv"
stan_path <- "models/rp_model.stan"
output_fit_path <- "models/rp_eb_2013_2017_fit.rds"
output_summary_path <- "results/prior_predictive/rp_prior_summary.csv"

chains <- 2
iter <- 2500
warmup <- 500
seed <- 42
refresh <- 10
subset_players <- 0
use_existing_fit <- as.integer(Sys.getenv("USE_EXISTING_FIT", "0"))

# Outcomes per IP
count_outcomes <- c("SO", "BB", "H", "ER", "W", "SVHLD")
zip_outcomes <- character(0)
svhld_idx <- match("SVHLD", count_outcomes)

# Load data
raw <- read_csv(input_path, show_col_types = FALSE)

# Basic cleaning
raw <- raw %>%
  mutate(Season = as.integer(Season)) %>%
  filter(Season >= 2013, Season <= 2017) %>%
  mutate(SVHLD = SV + HLD)

# Keep pitchers whose most recent season is RP
latest_role <- raw %>%
  group_by(playerid) %>%
  slice_max(order_by = Season, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(playerid, latest_role = Role)

raw <- raw %>%
  left_join(latest_role, by = "playerid") %>%
  filter(latest_role == "RP") %>%
  filter(Role == "RP") %>%
  select(-latest_role)

# Filter out projected starters (ATC GS >= 1) before fitting RP model
atc <- read_csv(atc_ip_path, show_col_types = FALSE)
id_col <- if ("playerid" %in% names(atc)) "playerid" else if ("PlayerId" %in% names(atc)) "PlayerId" else if ("player_id" %in% names(atc)) "player_id" else NA_character_
gs_col <- if ("GS" %in% names(atc)) "GS" else if ("gs" %in% names(atc)) "gs" else NA_character_
if (is.na(id_col) || is.na(gs_col)) {
  stop("ATC IP file must include playerid and GS columns: ", atc_ip_path)
}
starter_ids <- atc %>%
  transmute(playerid = as.character(.data[[id_col]]), GS = as.numeric(.data[[gs_col]])) %>%
  filter(!is.na(playerid), !is.na(GS), GS >= 1) %>%
  pull(playerid) %>%
  unique()
if (length(starter_ids) > 0) {
  raw <- raw %>% filter(!playerid %in% starter_ids)
}

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
    role_raw = if_else(is.na(Role) | Role == "", "UNK", Role)
  )

# Fixed effects design matrix
X <- cbind(
  intercept = 1,
  age_c = raw$age_c,
  age2 = raw$age2
)

# Player random effects: intercept + age only (no age2)
Z_player <- cbind(
  intercept = 1,
  age_c = raw$age_c
)

# Prior settings (outcome-specific defaults)
beta_mean <- matrix(0, nrow = ncol(X), ncol = length(count_outcomes))
beta_sd <- matrix(2.5, nrow = ncol(X), ncol = length(count_outcomes))
sigma_player_sd <- rep(1, length(count_outcomes))
sigma_year_sd <- rep(1, length(count_outcomes))

# Tighten SVHLD priors
if (!is.na(svhld_idx)) {
  beta_mean[1, svhld_idx] <- log(0.20)
  beta_sd[, svhld_idx] <- 0.5
  sigma_player_sd[svhld_idx] <- 0.2
  sigma_year_sd[svhld_idx] <- 0.05
}

# Outcomes
count_mat <- raw %>%
  select(all_of(count_outcomes)) %>%
  mutate(across(everything(), ~as.integer(round(.x)))) %>%
  as.matrix()

# Year index
years <- sort(unique(raw$Season))
year_id <- match(raw$Season, years)

# Offset (rate per IP)
offset_log_ip <- log(pmax(raw$IP, 1))

# Minimal prediction set (use latest season per player, no closer list)
latest_by_player <- raw %>%
  group_by(playerid) %>%
  slice_max(order_by = Season, n = 1, with_ties = FALSE) %>%
  ungroup()

latest_by_player <- latest_by_player %>%
  mutate(
    age_c = (Age - age_mean) / age_sd,
    age2 = age_c^2
  )

X_pred <- cbind(
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
year_id_pred <- rep(length(years), nrow(latest_by_player))
offset_log_ip_pred <- log(pmax(latest_by_player$IP, 1))

stan_data <- list(
  N = nrow(raw),
  K = length(count_outcomes),
  K_count = length(count_outcomes),
  P = ncol(X),
  R_player = ncol(Z_player),
  X = X,
  Z_player = Z_player,
  K_zip = length(zip_outcomes),
  zip_idx = match(zip_outcomes, count_outcomes),
  k_svhld = match("SVHLD", count_outcomes),
  role_leverage = as.numeric(raw$role_leverage),
  beta_mean = beta_mean,
  beta_sd = beta_sd,
  sigma_player_sd = sigma_player_sd,
  sigma_year_sd = sigma_year_sd,
  beta_zip_sd = rep(1.0, length(zip_outcomes)),
  J_player = length(unique(raw$player_id)),
  J_year = length(years),
  player_id = raw$player_id,
  year_id = year_id,
  y_count = count_mat,
  offset_log_ip = offset_log_ip,
  N_pred = nrow(latest_by_player),
  X_pred = X_pred,
  Z_player_pred = Z_player_pred,
  player_id_pred = player_id_pred,
  year_id_pred = year_id_pred,
  offset_log_ip_pred = offset_log_ip_pred,
  role_leverage_pred = as.numeric(latest_by_player$role_leverage)
)

if (!is.na(use_existing_fit) && use_existing_fit == 1 && file.exists(output_fit_path)) {
  fit <- readRDS(output_fit_path)
} else {
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
}

summarize_vec <- function(x) {
  c(
    mean = mean(x),
    sd = sd(x),
    p05 = unname(quantile(x, 0.05)),
    p50 = unname(quantile(x, 0.50)),
    p95 = unname(quantile(x, 0.95))
  )
}

summarize_array <- function(arr, dim1_labels, dim2_labels, param) {
  out <- list()
  idx <- 1
  for (i in seq_along(dim1_labels)) {
    for (j in seq_along(dim2_labels)) {
      stats <- as.list(summarize_vec(arr[, i, j]))
      out[[idx]] <- tibble(
        param = param,
        dim1 = dim1_labels[i],
        dim2 = dim2_labels[j],
        !!!stats
      )
      idx <- idx + 1
    }
  }
  bind_rows(out)
}

summarize_vector_param <- function(arr, labels, param) {
  out <- list()
  for (i in seq_along(labels)) {
    stats <- as.list(summarize_vec(arr[, i]))
    out[[i]] <- tibble(
      param = param,
      dim1 = labels[i],
      dim2 = NA_character_,
      !!!stats
    )
  }
  bind_rows(out)
}

draws <- rstan::extract(
  fit,
  pars = c("beta", "sigma_player", "sigma_year", "rho_year", "beta_role_svhld")
)

beta_labels <- colnames(X)
outcome_labels <- count_outcomes
player_re_labels <- colnames(Z_player)

summary_tbl <- bind_rows(
  summarize_array(draws$beta, beta_labels, outcome_labels, "beta"),
  summarize_array(draws$sigma_player, player_re_labels, outcome_labels, "sigma_player"),
  summarize_vector_param(draws$sigma_year, outcome_labels, "sigma_year"),
  summarize_vector_param(draws$rho_year, outcome_labels, "rho_year"),
  tibble(
    param = "beta_role_svhld",
    dim1 = NA_character_,
    dim2 = NA_character_,
    !!!as.list(summarize_vec(draws$beta_role_svhld))
  )
)

if (!dir.exists("results/prior_predictive")) {
  dir.create("results/prior_predictive", recursive = TRUE)
}

write_csv(summary_tbl, output_summary_path)
message(sprintf("Wrote %s", output_summary_path))
