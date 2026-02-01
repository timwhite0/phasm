library(dplyr)
library(readr)
suppressPackageStartupMessages({
  library(ggplot2)
})

fit_path <- 'models/model_fit.rds'
input_path <- 'data/fangraphs_batters_2018_2025.csv'
proj_path <- 'results/projections/category_projections_2026.csv'
results_dir <- 'results/plots'

if (!dir.exists('results')) dir.create('results')
if (!dir.exists(results_dir)) dir.create(results_dir)

proj <- read_csv(proj_path, show_col_types = FALSE) %>%
  mutate(playerid = as.character(playerid))

cat_defs <- list(
  H = 'H_mean',
  R = 'R_total_mean',
  RBI = 'RBI_total_mean',
  HR = 'HR_total_mean',
  SB = 'SB_total_mean',
  AVG = 'AVG_mean',
  OBP = 'OBP_mean',
  SLG = 'SLG_mean'
)

cat_top <- list()
for (cat in names(cat_defs)) {
  col <- cat_defs[[cat]]
  if (!col %in% names(proj)) next
  cat_top[[cat]] <- proj %>%
    filter(!is.na(.data[[col]])) %>%
    arrange(desc(.data[[col]])) %>%
    slice(1:100) %>%
    pull(playerid)
}

fit <- readRDS(fit_path)
raw <- read_csv(input_path, show_col_types = FALSE) %>%
  mutate(Season = as.integer(Season))

age_mean <- mean(raw$Age, na.rm = TRUE)
age_sd <- sd(raw$Age, na.rm = TRUE)
raw <- raw %>%
  mutate(
    age_c = (Age - age_mean) / age_sd,
    age2 = age_c^2,
    player_id = as.integer(factor(playerid)),
    pos_raw = if_else(is.na(position) | position == "", "UNK", position),
    pos_id = as.integer(factor(pos_raw))
  )

years <- sort(unique(raw$Season))

inv_logit <- function(x) 1 / (1 + exp(-x))

post <- rstan::extract(fit)
beta <- post$beta
u_pos <- post$u_pos
u_player <- post$u_player
year_effect <- post$year_effect
n_iter <- dim(beta)[1]
K <- dim(beta)[3]

summarize_draws <- function(x) {
  c(
    mean = mean(x, na.rm = TRUE),
    p10 = as.numeric(quantile(x, 0.1, na.rm = TRUE)),
    p90 = as.numeric(quantile(x, 0.9, na.rm = TRUE))
  )
}

outcomes <- c('H','R','RBI','HR','SB','AVG','OBP','SLG')

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
  Z_pos <- cbind(
    intercept = 1,
    age_c = subset$age_c,
    age2 = subset$age2
  )
  Z_player <- cbind(
    intercept = 1,
    age_c = subset$age_c
  )

  player_id <- subset$player_id
  pos_id <- subset$pos_id
  year_id <- match(subset$Season, years)

  n_rows <- nrow(subset)
  summaries <- vector('list', n_rows)

  for (i in seq_len(n_rows)) {
    x_i <- X[i, ]
    zp_i <- Z_pos[i, ]
    zpl_i <- Z_player[i, ]
    pid <- player_id[i]
    pos <- pos_id[i]
    yid <- year_id[i]

    eta <- matrix(0, nrow = n_iter, ncol = K)
    for (k in 1:K) {
      eta[, k] <- beta[, 1, k] * x_i[1] + beta[, 2, k] * x_i[2] + beta[, 3, k] * x_i[3]
    }
    for (r in 1:3) {
      for (k in 1:K) {
        eta[, k] <- eta[, k] + zp_i[r] * u_pos[, r, pos, k]
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

    rate_count <- exp(eta[, 1:5])
    avg_pred <- inv_logit(eta[, 6])
    obp_pred <- inv_logit(eta[, 7])
    slg_pred <- pmax(exp(eta[, 8]) - 1e-4, 0)

    summaries[[i]] <- list(
      H = summarize_draws(rate_count[,1]),
      R = summarize_draws(rate_count[,2]),
      RBI = summarize_draws(rate_count[,3]),
      HR = summarize_draws(rate_count[,4]),
      SB = summarize_draws(rate_count[,5]),
      AVG = summarize_draws(avg_pred),
      OBP = summarize_draws(obp_pred),
      SLG = summarize_draws(slg_pred)
    )
  }

  plot_rows <- list()
  for (i in seq_len(n_rows)) {
    obs <- subset[i, ]
    sum_o <- summaries[[i]][[o]]
    obs_val <- if (o %in% c('H','R','RBI','HR','SB')) {
      obs[[o]] / obs$PA
    } else {
      obs[[o]]
    }

    plot_rows[[length(plot_rows) + 1]] <- data.frame(
      playerid = as.character(obs$playerid),
      PlayerName = obs$PlayerName,
      Season = obs$Season,
      outcome = o,
      observed = obs_val,
      fitted_mean = sum_o['mean'],
      fitted_p10 = sum_o['p10'],
      fitted_p90 = sum_o['p90'],
      type = 'fit',
      stringsAsFactors = FALSE
    )
  }

  plot_df <- bind_rows(plot_rows) %>% arrange(PlayerName, Season)

  mean_col <- paste0(o, '_mean')
  p10_col <- paste0(o, '_p10')
  p90_col <- paste0(o, '_p90')
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
        fitted_p10 = .data[[p10_col]],
        fitted_p90 = .data[[p90_col]],
        type = 'projection'
      )
    plot_df <- bind_rows(plot_df, proj_df)
  }

  order_df <- plot_df %>%
    filter(Season == 2026L) %>%
    arrange(desc(fitted_mean)) %>%
    distinct(PlayerName)
  plot_df$PlayerName <- factor(plot_df$PlayerName, levels = order_df$PlayerName)

  write_csv(plot_df, file.path(results_dir, paste0('latent_fit_top100_', o, '_data.csv')))

  p <- ggplot(plot_df, aes(x = Season, group = PlayerName)) +
    geom_linerange(aes(ymin = fitted_p10, ymax = fitted_p90, color = type), linewidth = 0.6, alpha = 0.7, na.rm = TRUE) +
    geom_line(data = plot_df %>% filter(type == 'fit'), aes(y = fitted_mean), color = '#1f77b4', linewidth = 0.7) +
    geom_point(data = plot_df %>% filter(type == 'fit'), aes(y = fitted_mean), color = '#1f77b4', size = 1.6) +
    geom_point(data = plot_df %>% filter(type == 'projection'), aes(y = fitted_mean), color = '#2ca02c', size = 1.8, shape = 17) +
    geom_point(aes(y = observed), color = '#d62728', size = 1.4, na.rm = TRUE) +
    facet_wrap(~ PlayerName, scales = 'fixed') +
    theme_minimal(base_size = 10) +
    scale_x_continuous(breaks = 2021:2026) +
    scale_color_manual(values = c(fit = '#1f77b4', projection = '#2ca02c')) +
    labs(
      title = paste0(o, ': observed (red), fitted (blue), 2026 proj (green)'),
      y = if (o %in% c('H','R','RBI','HR','SB')) paste0(o, ' per PA') else o,
      x = 'Season'
    ) +
    theme(legend.position = 'none')

  ggsave(filename = file.path(results_dir, paste0('latent_fit_top100_', o, '.pdf')),
         plot = p, width = 18, height = 12)
}

cat('Wrote plots to', results_dir, '\n')
