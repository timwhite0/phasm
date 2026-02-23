suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(rstan)
})

fit_path <- Sys.getenv("RP_OUTPUT_FIT_PATH", "models/rp_model_fit.rds")
prep_path <- Sys.getenv("RP_OUTPUT_PREP_PATH", "models/rp_model_inputs.rds")
closer_cache_path <- Sys.getenv("RP_CLOSER_CACHE_PATH", "data/closer_depth_chart_2026.csv")
output_path <- Sys.getenv("RP_ETA_PRED_PATH", "models/rp_eta_pred_2026.rds")

closer_roles <- c("Closer", "Co-Closer", "Closer Committee", "Setup Man")

normalize_name <- function(x) {
  x2 <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  tolower(gsub("[^a-z0-9]", "", x2))
}

if (!file.exists(fit_path)) stop("Missing fit: ", fit_path)
if (!file.exists(prep_path)) stop("Missing prep: ", prep_path)
if (!file.exists(closer_cache_path)) stop("Missing closer cache: ", closer_cache_path)

fit <- readRDS(fit_path)
prep <- readRDS(prep_path)

if (is.null(prep$stan_data$role_leverage_pred)) {
  stop("Prep object missing role_leverage_pred; cannot refresh RP eta_pred.")
}

closer <- read_csv(closer_cache_path, show_col_types = FALSE) %>%
  rename_with(~ gsub("[^A-Za-z0-9]+", "_", .x))

if ("PROJECTED_ROLE" %in% names(closer)) closer <- closer %>% rename(ProjectedRole = PROJECTED_ROLE)
if ("PLAYER" %in% names(closer)) closer <- closer %>% rename(PlayerName = PLAYER)
if ("TEAM" %in% names(closer)) closer <- closer %>% rename(Team = TEAM)

need_cols <- c("Team", "PlayerName", "ProjectedRole")
if (!all(need_cols %in% names(closer))) {
  stop("Closer cache is missing required columns: Team, PlayerName, ProjectedRole")
}

closer_keys <- closer %>%
  filter(!is.na(PlayerName), PlayerName != "", PlayerName != "PLAYER") %>%
  filter(!(Team %in% c("AL East", "AL Central", "AL West", "NL East", "NL Central", "NL West"))) %>%
  distinct(Team, PlayerName, ProjectedRole, .keep_all = TRUE) %>%
  filter(ProjectedRole %in% closer_roles) %>%
  mutate(name_key = normalize_name(PlayerName)) %>%
  distinct(name_key) %>%
  pull(name_key)

lookup <- prep$player_lookup %>%
  mutate(name_key = normalize_name(PlayerName))

old_role <- as.numeric(prep$stan_data$role_leverage_pred)
new_role <- as.numeric(lookup$name_key %in% closer_keys)
delta <- new_role - old_role

eta_pred <- rstan::extract(fit, pars = "eta_pred")$eta_pred
beta_role <- rstan::extract(fit, pars = "beta_role_svhld")$beta_role_svhld

k_svhld <- as.integer(prep$stan_data$k_svhld)
if (!is.finite(k_svhld) || k_svhld < 1 || k_svhld > dim(eta_pred)[3]) {
  stop("Invalid SVHLD index in prep$stan_data$k_svhld.")
}

if (any(delta != 0)) {
  eta_pred[, , k_svhld] <- eta_pred[, , k_svhld] +
    matrix(beta_role, nrow = length(beta_role), ncol = length(delta)) *
      matrix(delta, nrow = length(beta_role), ncol = length(delta), byrow = TRUE)
}

saveRDS(
  list(
    eta_pred = eta_pred,
    player_lookup = prep$player_lookup,
    role_leverage_pred_old = old_role,
    role_leverage_pred_new = new_role,
    changed_n = sum(delta != 0),
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ),
  output_path
)

cat("Wrote ", output_path, "\n", sep = "")
cat("Role leverage changes: ", sum(delta != 0), " players\n", sep = "")
