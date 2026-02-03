suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(rstan)
  library(rvest)
})

rstan_options(auto_write = TRUE)
cores <- 8
options(mc.cores = cores)
message(sprintf("detectCores=%d, mc.cores=%d", cores, getOption("mc.cores")))

# Config
input_path <- "data/fangraphs_pitchers_2018_2025.csv"
atc_ip_path <- "data/atc_ip_projections_2026.csv"
closer_depth_chart_url <- "https://www.fangraphs.com/roster-resource/closer-depth-chart"
closer_cache_path <- "data/closer_depth_chart_2026.csv"
stan_path <- "models/rp_model.stan"
output_projection_path <- "results/projections/pitchers/rp_category_projections_2026.csv"
output_fit_path <- "models/rp_model_fit.rds"
output_prep_path <- "models/rp_model_inputs.rds"

run_fit <- TRUE
chains <- as.integer(Sys.getenv("STAN_CHAINS", "4"))
iter <- as.integer(Sys.getenv("STAN_ITER", "1000"))
warmup <- as.integer(Sys.getenv("STAN_WARMUP", "500"))
seed <- as.integer(Sys.getenv("STAN_SEED", "123"))
refresh <- as.integer(Sys.getenv("STAN_REFRESH", "100"))
subset_players <- as.integer(Sys.getenv("STAN_SUBSET_PLAYERS", "0"))

# Outcomes per IP
count_outcomes <- c("SO", "BB", "H", "ER", "W", "SVHLD")
zip_outcomes <- character(0)
svhld_idx <- match("SVHLD", count_outcomes)

# Load data
raw <- read_csv(input_path, show_col_types = FALSE)

# Basic cleaning
raw <- raw %>%
  mutate(Season = as.integer(Season)) %>%
  filter(Season >= 2018, Season <= 2025) %>%
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

normalize_name <- function(x) {
  x2 <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  tolower(gsub("[^a-z0-9]", "", x2))
}

fetch_closer_depth_chart <- function(url) {
  doc <- read_html(url)
  tables <- html_table(doc, fill = TRUE)
  out <- vector("list", length(tables))
  for (i in seq_along(tables)) {
    tbl <- tables[[i]]
    names(tbl) <- tolower(gsub("\\s+", "_", names(tbl)))
    if (!all(c("team", "player", "projected_role") %in% names(tbl))) {
      out[[i]] <- NULL
      next
    }
    out[[i]] <- tbl %>%
      transmute(
        Team = .data$team,
        PlayerName = .data$player,
        ProjectedRole = .data$projected_role
      )
  }
  bind_rows(out) %>%
    filter(!is.na(PlayerName), PlayerName != "PLAYER") %>%
    mutate(
      PlayerName = trimws(PlayerName),
      ProjectedRole = trimws(ProjectedRole)
    )
}

closer_roles <- c("Closer", "Co-Closer", "Closer Committee", "Setup Man")
if (file.exists(closer_cache_path)) {
  closer_list <- read_csv(closer_cache_path, show_col_types = FALSE) %>%
    rename_with(~ gsub("\\.", "_", .x))
  # Normalize expected columns from the Fangraphs table export
  if ("PROJECTED_ROLE" %in% names(closer_list)) {
    closer_list <- closer_list %>% rename(ProjectedRole = PROJECTED_ROLE)
  }
  if ("PLAYER" %in% names(closer_list)) {
    closer_list <- closer_list %>% rename(PlayerName = PLAYER)
  }
  if ("TEAM" %in% names(closer_list)) {
    closer_list <- closer_list %>% rename(Team = TEAM)
  }
  # Drop division headers and duplicates
  if (all(c("Team", "PlayerName", "ProjectedRole") %in% names(closer_list))) {
    closer_list <- closer_list %>%
      filter(!is.na(PlayerName), PlayerName != "", PlayerName != "PLAYER") %>%
      filter(!(Team %in% c("AL East", "AL Central", "AL West", "NL East", "NL Central", "NL West"))) %>%
      distinct(Team, PlayerName, ProjectedRole, .keep_all = TRUE)
  }
} else {
  stop(paste0(
    "Missing cached closer list at ", closer_cache_path,
    ". Create it locally (online) and rerun."
  ))
}

closer_list <- closer_list %>%
  filter(ProjectedRole %in% closer_roles) %>%
  mutate(name_key = normalize_name(PlayerName)) %>%
  distinct(name_key)

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
    role_raw = if_else(is.na(Role) | Role == "", "UNK", Role)
  )

latest_by_player <- latest_by_player %>%
  mutate(
    name_key = normalize_name(PlayerName),
    role_leverage = ifelse(name_key %in% closer_list$name_key, 1, 0)
  ) %>%
  select(-name_key)

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
year_id_pred <- rep(length(years) + 1, nrow(latest_by_player))
offset_log_ip_pred <- log(pmax(latest_by_player$IP, 1))

# Stan data
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

saveRDS(
  list(
    stan_data = stan_data,
    years = years,
    age_mean = age_mean,
    age_sd = age_sd,
    player_lookup = latest_by_player %>% select(playerid, PlayerName, Role)
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
if (length(rstan::get_sampler_params(fit, inc_warmup = FALSE)) == 0) {
  stop("Stan fit contains no samples; aborting projection step.")
}

eta_pred <- rstan::extract(fit, pars = "eta_pred")$eta_pred
beta_zip <- NULL
if (length(zip_outcomes) > 0) {
  beta_zip <- rstan::extract(fit, pars = "beta_zip")$beta_zip
}

# Summarize predictions per player
rate_count <- exp(eta_pred)

if (!is.null(beta_zip)) {
  pi_list <- vector("list", length(zip_outcomes))
  for (j in seq_along(zip_outcomes)) {
    beta_k <- beta_zip[, , j]
    lin_pred <- X_pred %*% t(beta_k)
    pi_list[[j]] <- t(1 / (1 + exp(-lin_pred)))
  }
  for (j in seq_along(zip_outcomes)) {
    k <- match(zip_outcomes[j], count_outcomes)
    rate_count[, , k] <- rate_count[, , k] * (1 - pi_list[[j]])
  }
}

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
    role = role_raw
  ) %>%
  distinct()

# Add outcomes
for (k in seq_along(count_outcomes)) {
  summary_k <- summarize_draws(rate_count[, , k])
  names(summary_k) <- paste0(count_outcomes[k], "_", names(summary_k))
  proj <- bind_cols(proj, summary_k)
}

write_csv(proj, output_projection_path)
