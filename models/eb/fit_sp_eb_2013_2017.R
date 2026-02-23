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
stan_path <- "models/sp_model.stan"
output_fit_path <- "models/sp_eb_2013_2017_fit.rds"
output_summary_path <- "results/prior_predictive/sp_prior_summary.csv"

chains <- 2
iter <- 2500
warmup <- 500
seed <- 42
refresh <- 10
subset_players <- 0
use_existing_fit <- as.integer(Sys.getenv("USE_EXISTING_FIT", "0"))

# Outcomes per IP
count_outcomes <- c("SO", "BB", "H", "ER", "W", "QS")
zip_outcomes <- character(0)
plv_covars <- c("StuffPlus", "LocationPlus", "BF")

# Load data
raw <- read_csv(input_path, show_col_types = FALSE)

for (v in plv_covars) {
  if (!v %in% names(raw)) raw[[v]] <- NA_real_
}

# Basic cleaning
raw <- raw %>%
  mutate(Season = as.integer(Season)) %>%
  filter(Season >= 2013, Season <= 2017)

# Keep pitchers whose most recent season is SP
latest_role <- raw %>%
  group_by(playerid) %>%
  slice_max(order_by = Season, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(playerid, latest_role = Role)

raw <- raw %>%
  left_join(latest_role, by = "playerid") %>%
  filter(latest_role == "SP") %>%
  filter(Role == "SP") %>%
  select(-latest_role)

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

raw <- raw %>%
  mutate(
    StuffPlus = suppressWarnings(as.numeric(StuffPlus)),
    LocationPlus = suppressWarnings(as.numeric(LocationPlus)),
    BF = suppressWarnings(as.numeric(BF))
  )

for (v in c("StuffPlus", "LocationPlus")) {
  mu <- mean(raw[[v]], na.rm = TRUE)
  sdv <- sd(raw[[v]], na.rm = TRUE)
  if (is.na(mu)) mu <- 0
  if (is.na(sdv) || sdv == 0) sdv <- 1
  raw[[paste0(v, "_z")]] <- (dplyr::coalesce(raw[[v]], mu) - mu) / sdv
}

raw <- raw %>%
  mutate(
    has_plv = as.integer(Season >= 2020 & !is.na(StuffPlus) & !is.na(LocationPlus)),
    plv_exposure = dplyr::coalesce(BF, 3 * IP, 1),
    plv_exposure = pmax(plv_exposure, 1)
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

# Prior settings (legacy baseline)
beta_mean <- matrix(0, nrow = ncol(X), ncol = length(count_outcomes))
beta_sd <- matrix(2.5, nrow = ncol(X), ncol = length(count_outcomes))
sigma_player_sd <- rep(1, length(count_outcomes))
sigma_year_sd <- rep(1, length(count_outcomes))
rho_year_mean <- rep(0, length(count_outcomes))
rho_year_sd <- rep(0.5, length(count_outcomes))

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

# Minimal prediction set (latest season per player)
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
  beta_mean = beta_mean,
  beta_sd = beta_sd,
  sigma_player_sd = sigma_player_sd,
  sigma_year_sd = sigma_year_sd,
  beta_zip_sd = rep(1.0, length(zip_outcomes)),
  rho_year_mean = rho_year_mean,
  rho_year_sd = rho_year_sd,
  J_player = length(unique(raw$player_id)),
  J_year = length(years),
  player_id = raw$player_id,
  year_id = year_id,
  y_count = count_mat,
  offset_log_ip = offset_log_ip,
  stuff_obs_z = raw$StuffPlus_z,
  location_obs_z = raw$LocationPlus_z,
  plv_exposure = raw$plv_exposure,
  has_plv = raw$has_plv,
  N_pred = nrow(latest_by_player),
  X_pred = X_pred,
  Z_player_pred = Z_player_pred,
  player_id_pred = player_id_pred,
  year_id_pred = year_id_pred,
  offset_log_ip_pred = offset_log_ip_pred
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
  pars = c("beta", "sigma_player", "sigma_year", "rho_year")
)

beta_labels <- colnames(X)
outcome_labels <- count_outcomes
player_re_labels <- colnames(Z_player)

summary_tbl <- bind_rows(
  summarize_array(draws$beta, beta_labels, outcome_labels, "beta"),
  summarize_array(draws$sigma_player, player_re_labels, outcome_labels, "sigma_player"),
  summarize_vector_param(draws$sigma_year, outcome_labels, "sigma_year"),
  summarize_vector_param(draws$rho_year, outcome_labels, "rho_year")
)

if (!dir.exists("results/prior_predictive")) {
  dir.create("results/prior_predictive", recursive = TRUE)
}

write_csv(summary_tbl, output_summary_path)
message(sprintf("Wrote %s", output_summary_path))
