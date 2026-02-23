library(dplyr)
library(readr)
suppressPackageStartupMessages({
  library(ggplot2)
})

fit_path <- 'models/rp_model_fit.rds'
eta_pred_path <- Sys.getenv("RP_ETA_PRED_PATH", "models/rp_eta_pred_2026.rds")
prep_path <- 'models/rp_model_inputs.rds'
input_path <- 'data/fangraphs_pitchers_2018_2025.csv'
atc_ip_path <- "data/atc_ip_projections_2026.csv"
results_dir <- 'results/plots/trends/pitchers/relievers'
ppd_seed <- as.integer(Sys.getenv("RP_PPD_SEED", "123"))

if (!dir.exists('results')) dir.create('results')
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

prep <- readRDS(prep_path)
set.seed(ppd_seed)

cat_defs <- list(
  W = 'W_mean',
  SVHLD = 'SVHLD_mean'
)

fit <- readRDS(fit_path)
raw <- read_csv(input_path, show_col_types = FALSE) %>%
  mutate(Season = as.integer(Season)) %>%
  filter(Season >= 2018, Season <= 2025) %>%
  filter(Role == "RP") %>%
  mutate(SVHLD = SV + HLD)
atc <- read_csv(atc_ip_path, show_col_types = FALSE)
if (!"role_leverage" %in% names(raw)) {
  stop("role_leverage column missing from input data")
}

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}

id_col <- pick_col(atc, c("playerid", "PlayerId", "player_id"))
ip_col <- pick_col(atc, c("IP", "ip"))
if (is.null(id_col) || is.null(ip_col)) {
  stop("Could not find playerid or IP column in ATC projections.")
}

atc <- atc %>%
  transmute(
    playerid = as.character(.data[[id_col]]),
    IP_atc = as.numeric(.data[[ip_col]])
  ) %>%
  filter(!is.na(playerid), !is.na(IP_atc), IP_atc > 0)

age_mean <- mean(raw$Age, na.rm = TRUE)
age_sd <- sd(raw$Age, na.rm = TRUE)
raw <- raw %>%
  mutate(
    age_c = (Age - age_mean) / age_sd,
    age2 = age_c^2,
    player_id = match(as.integer(playerid), prep$player_lookup$playerid),
    year_id = match(Season, prep$years)
  )
raw <- raw %>% filter(!is.na(player_id), !is.na(year_id), !is.na(IP), IP > 0)

post <- rstan::extract(fit)
if (file.exists(eta_pred_path)) {
  eta_obj <- readRDS(eta_pred_path)
  eta_pred <- eta_obj$eta_pred
} else {
  eta_pred <- post$eta_pred
}
beta <- post$beta
beta_role_svhld <- post$beta_role_svhld
beta_zip <- NULL
u_role <- NULL
u_player <- post$u_player
year_effect <- post$year_effect
n_iter <- dim(beta)[1]
K <- dim(beta)[3]
J_player <- dim(u_player)[3]
k_svhld <- 6

get_or_default <- function(x, default) {
  if (is.null(x)) default else x
}

beta_stuff_lat <- get_or_default(post$beta_stuff_lat, matrix(0, nrow = n_iter, ncol = 3))
beta_location_lat <- get_or_default(post$beta_location_lat, matrix(0, nrow = n_iter, ncol = 3))
beta_stuff_out <- get_or_default(post$beta_stuff_out, matrix(0, nrow = n_iter, ncol = K))
beta_location_out <- get_or_default(post$beta_location_out, matrix(0, nrow = n_iter, ncol = K))
u_player_plv <- get_or_default(post$u_player_plv, array(0, dim = c(n_iter, J_player, 4)))

summarize_draws <- function(x) {
  c(
    mean = mean(x, na.rm = TRUE),
    p05 = as.numeric(quantile(x, 0.05, na.rm = TRUE)),
    p50 = as.numeric(quantile(x, 0.5, na.rm = TRUE)),
    p95 = as.numeric(quantile(x, 0.95, na.rm = TRUE))
  )
}

summarize_matrix <- function(draws_3d) {
  t(apply(draws_3d, 2, summarize_draws))
}

proj_all <- prep$player_lookup %>%
  mutate(playerid = as.character(playerid)) %>%
  left_join(atc, by = "playerid")

keep_idx <- which(!is.na(proj_all$IP_atc) & proj_all$IP_atc > 0)
if (length(keep_idx) == 0) {
  stop("No ATC IP matches found for 2026 projections.")
}

proj <- proj_all[keep_idx, ]

rate_pred <- exp(eta_pred)
ip_pred <- matrix(rep(proj$IP_atc, each = n_iter), nrow = n_iter)
w_count_pred <- matrix(rpois(length(ip_pred), rate_pred[, keep_idx, 5] * ip_pred), nrow = n_iter)
svhld_count_pred <- matrix(rpois(length(ip_pred), rate_pred[, keep_idx, 6] * ip_pred), nrow = n_iter)

proj <- bind_cols(
  proj,
  setNames(as.data.frame(summarize_matrix(w_count_pred / ip_pred)), c("W_mean", "W_p05", "W_p50", "W_p95")),
  setNames(as.data.frame(summarize_matrix(svhld_count_pred / ip_pred)), c("SVHLD_mean", "SVHLD_p05", "SVHLD_p50", "SVHLD_p95"))
)

cat_top <- list()
for (cat in names(cat_defs)) {
  col <- cat_defs[[cat]]
  if (!col %in% names(proj)) next
  cat_top[[cat]] <- proj %>%
    filter(!is.na(.data[[col]])) %>%
    {if (cat %in% c("H", "BB", "ER")) arrange(., .data[[col]]) else arrange(., desc(.data[[col]]))} %>%
    slice(1:100) %>%
    pull(playerid)
}

outcomes <- names(cat_defs)

for (o in outcomes) {
  ids <- cat_top[[o]]
  if (is.null(ids)) next

  subset <- raw %>% filter(as.character(playerid) %in% ids)
  if (nrow(subset) == 0) next

  X <- cbind(
    intercept = 1,
    age_c = subset$age_c,
    age2 = subset$age2
  )
  Z_player <- cbind(
    intercept = 1,
    age_c = subset$age_c
  )

  subset <- subset %>%
    mutate(
      player_id = match(as.integer(playerid), prep$player_lookup$playerid),
      year_id = match(Season, prep$years)
    )

  if (nrow(subset) == 0) next

  player_id <- subset$player_id
  year_id <- subset$year_id
  role_leverage <- subset$role_leverage

  n_rows <- nrow(subset)
  summaries <- vector('list', n_rows)

  for (i in seq_len(n_rows)) {
    x_i <- X[i, ]
    zpl_i <- Z_player[i, ]
    pid <- player_id[i]
    yid <- year_id[i]

    eta <- matrix(0, nrow = n_iter, ncol = K)
    for (k in 1:K) {
      eta[, k] <- beta[, 1, k] * x_i[1] + beta[, 2, k] * x_i[2] + beta[, 3, k] * x_i[3]
    }
    if (!is.null(u_role)) {
      for (r in 1:3) {
        for (k in 1:K) {
          eta[, k] <- eta[, k] + zr_i[r] * u_role[, r, role, k]
        }
      }
    }
    for (r in 1:2) {
      for (k in 1:K) {
        eta[, k] <- eta[, k] + zpl_i[r] * u_player[, r, pid, k]
      }
    }
    for (k in 1:K) {
      eta[, k] <- eta[, k] + year_effect[, k, yid]
    }

    stuff_lat <- beta_stuff_lat[, 1] + beta_stuff_lat[, 2] * x_i[2] + beta_stuff_lat[, 3] * x_i[3] +
      u_player_plv[, pid, 1] + x_i[2] * u_player_plv[, pid, 2]
    location_lat <- beta_location_lat[, 1] + beta_location_lat[, 2] * x_i[2] + beta_location_lat[, 3] * x_i[3] +
      u_player_plv[, pid, 3] + x_i[2] * u_player_plv[, pid, 4]
    for (k in 1:K) {
      eta[, k] <- eta[, k] + beta_stuff_out[, k] * stuff_lat + beta_location_out[, k] * location_lat
    }

    eta[, k_svhld] <- eta[, k_svhld] + role_leverage[i] * beta_role_svhld

    rate_count <- exp(eta)
    ip_hist <- subset$IP[i]
    w_rate <- rpois(n_iter, rate_count[, 5] * ip_hist) / ip_hist
    svhld_rate <- rpois(n_iter, rate_count[, 6] * ip_hist) / ip_hist

    summaries[[i]] <- list(
      W = summarize_draws(w_rate),
      SVHLD = summarize_draws(svhld_rate)
    )
  }

  plot_rows <- list()
  for (i in seq_len(n_rows)) {
    obs <- subset[i, ]
    sum_o <- summaries[[i]][[o]]
    obs_val <- obs[[o]] / obs$IP

    plot_rows[[length(plot_rows) + 1]] <- data.frame(
      playerid = as.character(obs$playerid),
      PlayerName = obs$PlayerName,
      Season = obs$Season,
      outcome = o,
      observed = obs_val,
      fitted_mean = sum_o['mean'],
      fitted_p05 = sum_o['p05'],
      fitted_p95 = sum_o['p95'],
      type = 'fit',
      stringsAsFactors = FALSE
    )
  }

  plot_df <- bind_rows(plot_rows) %>% arrange(PlayerName, Season)

  mean_col <- paste0(o, '_mean')
  p05_col <- paste0(o, '_p05')
  p95_col <- paste0(o, '_p95')
  if (mean_col %in% names(proj)) {
    proj_df <- proj %>%
      filter(playerid %in% ids) %>%
      transmute(
        playerid,
        PlayerName,
        Season = 2026L,
        outcome = o,
        observed = NA_real_,
        fitted_mean = .data[[mean_col]],
        fitted_p05 = .data[[p05_col]],
        fitted_p95 = .data[[p95_col]],
        type = 'projection'
      )
    plot_df <- bind_rows(plot_df, proj_df)
  }

  order_df <- plot_df %>%
    filter(Season == 2026L)
  if (o %in% c("H", "BB", "ER")) {
    order_df <- order_df %>% arrange(fitted_mean)
  } else {
    order_df <- order_df %>% arrange(desc(fitted_mean))
  }
  order_df <- order_df %>% distinct(PlayerName)
  plot_df$PlayerName <- factor(plot_df$PlayerName, levels = order_df$PlayerName)

  write_csv(plot_df, file.path(results_dir, paste0('rp_trends_top100_', o, '_data.csv')))

  p <- ggplot(plot_df, aes(x = Season, group = PlayerName)) +
    geom_linerange(aes(ymin = fitted_p05, ymax = fitted_p95, color = type), linewidth = 0.6, alpha = 0.7, na.rm = TRUE) +
    geom_line(data = plot_df %>% filter(type == 'fit'), aes(y = fitted_mean), color = 'gold3', linewidth = 0.7) +
    geom_point(data = plot_df %>% filter(type == 'fit'), aes(y = fitted_mean), color = 'gold3', size = 1.6) +
    geom_point(data = plot_df %>% filter(type == 'projection'), aes(y = fitted_mean), color = 'steelblue3', size = 1.8, shape = 17) +
    geom_point(aes(y = observed), color = 'black', size = 1.4, na.rm = TRUE) +
    facet_wrap(~ PlayerName, scales = 'fixed') +
    theme_minimal(base_size = 10) +
    scale_x_continuous(breaks = 2018:2026) +
    scale_color_manual(values = c(fit = 'gold3', projection = 'steelblue3')) +
    labs(
      title = paste0(o, ' per IP: observed (black), fitted (gold), 2026 projection (blue)'),
      y = paste0(o, ' per IP'),
      x = 'Season'
    ) +
    theme(
      legend.position = 'none',
      strip.text = element_text(face = 'bold'),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave(filename = file.path(results_dir, paste0('rp_trends_top100_', o, '.pdf')),
         plot = p, width = 18, height = 12)
}

cat('Wrote plots to', results_dir, '\n')
