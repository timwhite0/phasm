library(dplyr)
library(readr)
suppressPackageStartupMessages({
  library(ggplot2)
})

fit_path <- 'models/model_fit.rds'
prep_path <- 'models/model_inputs.rds'
input_path <- 'data/fangraphs_batters_2018_2025.csv'
results_dir <- 'results/plots/latent_fits'

if (!dir.exists('results')) dir.create('results')
if (!dir.exists(results_dir)) dir.create(results_dir)

prep <- readRDS(prep_path)

cat_defs <- list(
  H = 'H_mean',
  R = 'R_mean',
  RBI = 'RBI_mean',
  HR = 'HR_mean',
  SB = 'SB_mean',
  AVG = 'AVG_mean',
  OBP = 'OBP_mean',
  SLG = 'SLG_mean'
)

fit <- readRDS(fit_path)
post <- rstan::extract(fit)
eta_pred <- post$eta_pred

# Build projection summaries from the posterior draws (90% intervals)
inv_logit <- function(x) 1 / (1 + exp(-x))
epsilon <- 1e-4
rate_count <- exp(eta_pred[, , 1:5])
avg_pred <- inv_logit(eta_pred[, , 6])
obp_pred <- inv_logit(eta_pred[, , 7])
slg_pred <- pmax(exp(eta_pred[, , 8]) - epsilon, 0)

summarize_draws <- function(draws_mat) {
  c(
    mean = mean(draws_mat, na.rm = TRUE),
    p05 = as.numeric(quantile(draws_mat, 0.05, na.rm = TRUE)),
    p50 = as.numeric(quantile(draws_mat, 0.5, na.rm = TRUE)),
    p95 = as.numeric(quantile(draws_mat, 0.95, na.rm = TRUE))
  )
}

summarize_matrix <- function(draws_3d) {
  t(apply(draws_3d, 2, summarize_draws))
}

proj <- prep$player_lookup %>%
  mutate(playerid = as.character(playerid)) %>%
  distinct()

proj <- bind_cols(
  proj,
  setNames(as.data.frame(summarize_matrix(rate_count[, , 1])), c("H_mean", "H_p05", "H_p50", "H_p95")),
  setNames(as.data.frame(summarize_matrix(rate_count[, , 2])), c("R_mean", "R_p05", "R_p50", "R_p95")),
  setNames(as.data.frame(summarize_matrix(rate_count[, , 3])), c("RBI_mean", "RBI_p05", "RBI_p50", "RBI_p95")),
  setNames(as.data.frame(summarize_matrix(rate_count[, , 4])), c("HR_mean", "HR_p05", "HR_p50", "HR_p95")),
  setNames(as.data.frame(summarize_matrix(rate_count[, , 5])), c("SB_mean", "SB_p05", "SB_p50", "SB_p95")),
  setNames(as.data.frame(summarize_matrix(avg_pred)), c("AVG_mean", "AVG_p05", "AVG_p50", "AVG_p95")),
  setNames(as.data.frame(summarize_matrix(obp_pred)), c("OBP_mean", "OBP_p05", "OBP_p50", "OBP_p95")),
  setNames(as.data.frame(summarize_matrix(slg_pred)), c("SLG_mean", "SLG_p05", "SLG_p50", "SLG_p95"))
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

beta <- post$beta
u_pos <- post$u_pos
u_player <- post$u_player
year_effect <- post$year_effect
n_iter <- dim(beta)[1]
K <- dim(beta)[3]

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
    filter(Season == 2026L) %>%
    arrange(desc(fitted_mean)) %>%
    distinct(PlayerName)
  plot_df$PlayerName <- factor(plot_df$PlayerName, levels = order_df$PlayerName)

  write_csv(plot_df, file.path(results_dir, paste0('latent_fit_top100_', o, '_data.csv')))

  p <- ggplot(plot_df, aes(x = Season, group = PlayerName)) +
    geom_linerange(aes(ymin = fitted_p05, ymax = fitted_p95, color = type), linewidth = 0.6, alpha = 0.7, na.rm = TRUE) +
    geom_line(data = plot_df %>% filter(type == 'fit'), aes(y = fitted_mean), color = 'goldenrod', linewidth = 0.7) +
    geom_point(data = plot_df %>% filter(type == 'fit'), aes(y = fitted_mean), color = 'goldenrod', size = 1.6) +
    geom_point(data = plot_df %>% filter(type == 'projection'), aes(y = fitted_mean), color = 'dodgerblue', size = 1.8, shape = 17) +
    geom_point(aes(y = observed), color = 'black', size = 1.4, na.rm = TRUE) +
    facet_wrap(~ PlayerName, scales = 'fixed') +
    theme_minimal(base_size = 10) +
    scale_x_continuous(breaks = 2021:2026) +
    scale_color_manual(values = c(fit = 'goldenrod', projection = 'dodgerblue')) +
    labs(
      title = paste0(o, ': observed (red), fitted (blue), 2026 proj (green)'),
      y = if (o %in% c('H','R','RBI','HR','SB')) paste0(o, ' per PA') else o,
      x = 'Season'
    ) +
    theme(
      legend.position = 'none',
      strip.text = element_text(face = 'bold')
    )

  ggsave(filename = file.path(results_dir, paste0('latent_fit_top100_', o, '.pdf')),
         plot = p, width = 18, height = 12)
}

cat('Wrote plots to', results_dir, '\n')
