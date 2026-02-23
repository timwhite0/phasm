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
input_path <- "data/fangraphs_batters_2015_2017.csv"
stan_path <- "models/hitter_model.stan"
output_fit_path <- "models/batter_eb_2015_2017_fit.rds"
output_summary_path <- "results/prior_predictive/batter_prior_summary.csv"

chains <- 2
iter <- 2500
warmup <- 500
seed <- 42
refresh <- 10
subset_players <- 0
use_existing_fit <- as.integer(Sys.getenv("USE_EXISTING_FIT", "0"))
stan_init <- Sys.getenv("STAN_INIT", "")

# Outcomes
count_outcomes <- c("H", "R", "RBI", "HR", "SB")
cont_outcomes <- c("AVG", "OBP", "SLG")
all_outcomes <- c(count_outcomes, cont_outcomes)
statcast_covars <- c("EV", "LA", "Events", "BarrelPct", "HardHitPct")

# Helpers
logit <- function(x) log(x / (1 - x))
inv_logit <- function(x) 1 / (1 + exp(-x))

# Load data
raw <- read_csv(input_path, show_col_types = FALSE)

# Basic cleaning
raw <- raw %>%
  mutate(Season = as.integer(Season)) %>%
  filter(Season >= 2015, Season <= 2017)

missing_statcast <- setdiff(statcast_covars, names(raw))
if (length(missing_statcast) > 0) {
  stop(
    "Missing Statcast covariates in EB hitter input: ",
    paste(missing_statcast, collapse = ", ")
  )
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

sc_mean <- setNames(numeric(2), c("EV", "LA"))
sc_sd <- setNames(numeric(2), c("EV", "LA"))
for (v in c("EV", "LA")) {
  raw[[v]] <- suppressWarnings(as.numeric(raw[[v]]))
  mu <- mean(raw[[v]], na.rm = TRUE)
  sdv <- sd(raw[[v]], na.rm = TRUE)
  if (is.na(mu)) mu <- 0
  if (is.na(sdv) || sdv == 0) sdv <- 1
  sc_mean[[v]] <- mu
  sc_sd[[v]] <- sdv
  raw[[v]] <- dplyr::coalesce(raw[[v]], mu)
  raw[[paste0(v, "_z")]] <- (raw[[v]] - mu) / sdv
}

raw <- raw %>%
  mutate(
    Events = suppressWarnings(as.numeric(Events)),
    Events = if_else(is.na(Events) | Events < 1, 1, Events)
  )

bbe_mean <- setNames(numeric(2), c("BarrelPct", "HardHitPct"))
for (v in c("BarrelPct", "HardHitPct")) {
  raw[[v]] <- suppressWarnings(as.numeric(raw[[v]]))
  prop <- raw[[v]]
  prop <- ifelse(is.na(prop), NA_real_, ifelse(prop > 1, prop / 100, prop))
  prop <- pmin(pmax(prop, 1e-4), 1 - 1e-4)
  mu <- mean(prop, na.rm = TRUE)
  if (is.na(mu)) mu <- 0.5
  bbe_mean[[v]] <- mu
  prop <- dplyr::coalesce(prop, mu)
  raw[[paste0(v, "_logit")]] <- logit(prop)
}

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

# Prior settings (legacy baseline)
beta_mean <- matrix(0, nrow = ncol(X), ncol = length(all_outcomes))
beta_sd <- matrix(2.5, nrow = ncol(X), ncol = length(all_outcomes))
sigma_player_sd <- rep(1, length(all_outcomes))
sigma_pos_sd <- rep(1, length(all_outcomes))
sigma_year_sd <- rep(1, length(all_outcomes))
rho_year_mean <- rep(0, length(all_outcomes))
rho_year_sd <- rep(0.5, length(all_outcomes))
sigma_cont_sd <- rep(1, length(cont_outcomes))

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

# Minimal prediction set (latest season per player)
latest_by_player <- raw %>%
  group_by(playerid) %>%
  slice_max(order_by = Season, n = 1, with_ties = FALSE) %>%
  ungroup()

latest_by_player <- latest_by_player %>%
  mutate(
    age_c = (Age - age_mean) / age_sd,
    age2 = age_c^2,
    pos_raw = if_else(is.na(position) | position == "", "UNK", position),
    pos_id = as.integer(factor(pos_raw, levels = levels(factor(raw$pos_raw)))),
    EV = dplyr::coalesce(as.numeric(EV), sc_mean[["EV"]]),
    LA = dplyr::coalesce(as.numeric(LA), sc_mean[["LA"]]),
    EV_z = (EV - sc_mean[["EV"]]) / sc_sd[["EV"]],
    LA_z = (LA - sc_mean[["LA"]]) / sc_sd[["LA"]],
    BarrelPct = suppressWarnings(as.numeric(BarrelPct)),
    HardHitPct = suppressWarnings(as.numeric(HardHitPct)),
    BarrelPct = if_else(BarrelPct > 1, BarrelPct / 100, BarrelPct),
    HardHitPct = if_else(HardHitPct > 1, HardHitPct / 100, HardHitPct),
    BarrelPct = dplyr::coalesce(BarrelPct, bbe_mean[["BarrelPct"]]),
    HardHitPct = dplyr::coalesce(HardHitPct, bbe_mean[["HardHitPct"]]),
    BarrelPct = pmin(pmax(BarrelPct, 1e-4), 1 - 1e-4),
    HardHitPct = pmin(pmax(HardHitPct, 1e-4), 1 - 1e-4),
    BarrelPct_logit = logit(BarrelPct),
    HardHitPct_logit = logit(HardHitPct)
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
year_id_pred <- rep(length(years), nrow(latest_by_player))
offset_log_pa_pred <- log(pmax(latest_by_player$PA, 1))

# Stan data
stan_data <- list(
  N = nrow(raw),
  K = length(all_outcomes),
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
  ev_obs_z = raw$EV_z,
  la_obs_z = raw$LA_z,
  barrel_obs_logit = raw$BarrelPct_logit,
  hardhit_obs_logit = raw$HardHitPct_logit,
  events_bb = raw$Events,
  N_pred = nrow(latest_by_player),
  X_pred = X_pred,
  Z_pred = Z_pred,
  Z_player_pred = Z_player_pred,
  player_id_pred = player_id_pred,
  pos_id_pred = pos_id_pred,
  year_id_pred = year_id_pred,
  offset_log_pa_pred = offset_log_pa_pred
)

if (!is.na(use_existing_fit) && use_existing_fit == 1 && file.exists(output_fit_path)) {
  fit <- readRDS(output_fit_path)
} else {
  init_arg <- "random"
  if (identical(stan_init, "0")) {
    init_arg <- 0
    message("Using Stan init = 0")
  }

  fit <- stan(
    file = stan_path,
    data = stan_data,
    chains = chains,
    iter = iter,
    warmup = warmup,
    seed = seed,
    refresh = refresh,
    init = init_arg,
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

summarize_scalar_param <- function(arr, label, param) {
  stats <- as.list(summarize_vec(arr))
  tibble(
    param = param,
    dim1 = label,
    dim2 = NA_character_,
    !!!stats
  )
}

draws <- rstan::extract(
  fit,
  pars = c(
    "beta", "sigma_player", "sigma_pos", "sigma_year", "rho_year", "sigma_cont",
    "beta_ev_lat", "beta_la_lat", "sigma_ev_obs", "sigma_la_obs",
    "beta_barrel_lat", "beta_hardhit_lat", "sigma_barrel_obs", "sigma_hardhit_obs",
    "sigma_player_statcast", "sigma_player_bbe",
    "beta_ev_out", "beta_la_out", "beta_barrel_out", "beta_hardhit_out"
  )
)

beta_labels <- colnames(X)
outcome_labels <- all_outcomes
player_re_labels <- colnames(Z_player)
pos_re_labels <- colnames(Z)

summary_tbl <- bind_rows(
  summarize_array(draws$beta, beta_labels, outcome_labels, "beta"),
  summarize_array(draws$sigma_player, player_re_labels, outcome_labels, "sigma_player"),
  summarize_array(draws$sigma_pos, pos_re_labels, outcome_labels, "sigma_pos"),
  summarize_vector_param(draws$sigma_year, outcome_labels, "sigma_year"),
  summarize_vector_param(draws$rho_year, outcome_labels, "rho_year"),
  summarize_vector_param(draws$sigma_cont, cont_outcomes, "sigma_cont"),
  summarize_vector_param(draws$beta_ev_lat, c("intercept", "age_c", "age2"), "beta_ev_lat"),
  summarize_vector_param(draws$beta_la_lat, c("intercept", "age_c", "age2"), "beta_la_lat"),
  summarize_vector_param(draws$beta_barrel_lat, c("intercept", "age_c", "age2"), "beta_barrel_lat"),
  summarize_vector_param(draws$beta_hardhit_lat, c("intercept", "age_c", "age2"), "beta_hardhit_lat"),
  summarize_scalar_param(draws$sigma_ev_obs, "sigma_ev_obs", "sigma_ev_obs"),
  summarize_scalar_param(draws$sigma_la_obs, "sigma_la_obs", "sigma_la_obs"),
  summarize_scalar_param(draws$sigma_barrel_obs, "sigma_barrel_obs", "sigma_barrel_obs"),
  summarize_scalar_param(draws$sigma_hardhit_obs, "sigma_hardhit_obs", "sigma_hardhit_obs"),
  summarize_vector_param(draws$sigma_player_statcast, c("ev_intercept", "ev_age", "la_intercept", "la_age"), "sigma_player_statcast"),
  summarize_vector_param(draws$sigma_player_bbe, c("barrel_intercept", "barrel_age", "hardhit_intercept", "hardhit_age"), "sigma_player_bbe"),
  summarize_vector_param(draws$beta_ev_out, c("H", "R", "RBI", "HR", "AVG", "OBP", "SLG"), "beta_ev_out"),
  summarize_vector_param(draws$beta_la_out, c("H", "R", "RBI", "HR", "AVG", "OBP", "SLG"), "beta_la_out"),
  summarize_vector_param(draws$beta_barrel_out, c("H", "R", "RBI", "HR", "AVG", "OBP", "SLG"), "beta_barrel_out"),
  summarize_vector_param(draws$beta_hardhit_out, c("H", "R", "RBI", "HR", "AVG", "OBP", "SLG"), "beta_hardhit_out")
)

if (!dir.exists("results/prior_predictive")) {
  dir.create("results/prior_predictive", recursive = TRUE)
}

write_csv(summary_tbl, output_summary_path)
message(sprintf("Wrote %s", output_summary_path))
