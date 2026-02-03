library(dplyr)
library(readr)
suppressPackageStartupMessages({
  library(ggplot2)
})

fit_path <- 'models/rp_model_fit.rds'
prep_path <- 'models/rp_model_inputs.rds'
input_path <- 'data/fangraphs_pitchers_2018_2025.csv'
results_dir <- 'results/plots/fitted_outcome_curves/pitchers/relievers'

if (!dir.exists('results')) dir.create('results')
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

prep <- readRDS(prep_path)

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
if (!"role_leverage" %in% names(raw)) {
  stop("role_leverage column missing from input data")
}

age_mean <- mean(raw$Age, na.rm = TRUE)
age_sd <- sd(raw$Age, na.rm = TRUE)
raw <- raw %>%
  mutate(
    age_c = (Age - age_mean) / age_sd,
    age2 = age_c^2,
    player_id = as.integer(factor(playerid))
  )

years <- prep$years

post <- rstan::extract(fit)
eta_pred <- post$eta_pred
beta <- post$beta
beta_role_svhld <- post$beta_role_svhld
beta_zip <- NULL
u_role <- NULL
u_player <- post$u_player
year_effect <- post$year_effect
n_iter <- dim(beta)[1]
K <- dim(beta)[3]
k_svhld <- 6

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

proj <- prep$player_lookup %>%
  mutate(playerid = as.character(playerid)) %>%
  distinct()

rate_pred <- exp(eta_pred)
proj <- bind_cols(
  proj,
  setNames(as.data.frame(summarize_matrix(rate_pred[, , 1])), c("SO_mean", "SO_p05", "SO_p50", "SO_p95")),
  setNames(as.data.frame(summarize_matrix(rate_pred[, , 2])), c("BB_mean", "BB_p05", "BB_p50", "BB_p95")),
  setNames(as.data.frame(summarize_matrix(rate_pred[, , 3])), c("H_mean", "H_p05", "H_p50", "H_p95")),
  setNames(as.data.frame(summarize_matrix(rate_pred[, , 4])), c("ER_mean", "ER_p05", "ER_p50", "ER_p95")),
  setNames(as.data.frame(summarize_matrix(rate_pred[, , 5])), c("W_mean", "W_p05", "W_p50", "W_p95")),
  setNames(as.data.frame(summarize_matrix(rate_pred[, , 6])), c("SVHLD_mean", "SVHLD_p05", "SVHLD_p50", "SVHLD_p95"))
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
      year_id = match(Season, years)
    ) %>%
    filter(!is.na(player_id), !is.na(year_id))

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
    eta[, k_svhld] <- eta[, k_svhld] + role_leverage[i] * beta_role_svhld

    rate_count <- exp(eta)

    summaries[[i]] <- list(
      SO = summarize_draws(rate_count[, 1]),
      BB = summarize_draws(rate_count[, 2]),
      H = summarize_draws(rate_count[, 3]),
      ER = summarize_draws(rate_count[, 4]),
      W = summarize_draws(rate_count[, 5]),
      SVHLD = summarize_draws(rate_count[, 6])
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

  write_csv(plot_df, file.path(results_dir, paste0('rp_latent_fit_top100_', o, '_data.csv')))

  p <- ggplot(plot_df, aes(x = Season, group = PlayerName)) +
    geom_linerange(aes(ymin = fitted_p05, ymax = fitted_p95, color = type), linewidth = 0.6, alpha = 0.7, na.rm = TRUE) +
    geom_line(data = plot_df %>% filter(type == 'fit'), aes(y = fitted_mean), color = 'goldenrod', linewidth = 0.7) +
    geom_point(data = plot_df %>% filter(type == 'fit'), aes(y = fitted_mean), color = 'goldenrod', size = 1.6) +
    geom_point(data = plot_df %>% filter(type == 'projection'), aes(y = fitted_mean), color = 'dodgerblue', size = 1.8, shape = 17) +
    geom_point(aes(y = observed), color = 'black', size = 1.4, na.rm = TRUE) +
    facet_wrap(~ PlayerName, scales = 'fixed') +
    theme_minimal(base_size = 10) +
    scale_x_continuous(breaks = 2018:2026) +
    scale_color_manual(values = c(fit = 'goldenrod', projection = 'dodgerblue')) +
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

  ggsave(filename = file.path(results_dir, paste0('rp_latent_fit_top100_', o, '.pdf')),
         plot = p, width = 18, height = 12)
}

cat('Wrote plots to', results_dir, '\n')
