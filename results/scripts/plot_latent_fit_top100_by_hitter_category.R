library(dplyr)
library(readr)
suppressPackageStartupMessages({
  library(ggplot2)
})

fit_path <- 'models/hitter_model_fit.rds'
prep_path <- 'models/hitter_model_inputs.rds'
input_path <- 'data/fangraphs_batters_2018_2025.csv'
atc_pa_path <- 'data/atc_pa_projections_2026.csv'
results_dir <- 'results/plots/trends/batters'
pa_cv <- as.numeric(Sys.getenv("HITTER_PA_ATC_CV", "0.10"))

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
sigma_cont <- post$sigma_cont
n_iter <- dim(eta_pred)[1]

# Build projection summaries from posterior predictive draws (90% intervals)
inv_logit <- function(x) 1 / (1 + exp(-x))
epsilon <- 1e-4

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}

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

atc <- read_csv(atc_pa_path, show_col_types = FALSE)
id_col <- pick_col(atc, c("playerid", "PlayerId", "player_id"))
pa_col <- pick_col(atc, c("PA", "pa"))
if (is.null(id_col) || is.null(pa_col)) {
  stop("Could not find playerid or PA column in ATC projections.")
}
atc <- atc %>%
  transmute(
    playerid = as.character(.data[[id_col]]),
    PA_atc = as.numeric(.data[[pa_col]])
  ) %>%
  filter(!is.na(playerid), !is.na(PA_atc), PA_atc > 0)
proj <- proj %>%
  left_join(atc, by = "playerid") %>%
  filter(!is.na(PA_atc), PA_atc > 0)
lookup_ids <- as.character(prep$player_lookup$playerid)
keep_idx <- match(proj$playerid, lookup_ids)
if (any(is.na(keep_idx))) {
  stop("Could not align projected player ids to eta_pred indices.")
}
eta_pred_proj <- eta_pred[, keep_idx, , drop = FALSE]

sample_pa_draws <- function(pa_vec, n_draw, cv) {
  if (!is.finite(cv) || cv <= 0) {
    return(matrix(rep(pa_vec, each = n_draw), nrow = n_draw))
  }
  shape <- 1 / (cv^2)
  scale_vec <- pa_vec / shape
  matrix(
    rgamma(n_draw * length(pa_vec), shape = shape, scale = rep(scale_vec, each = n_draw)),
    nrow = n_draw
  )
}

pa_mat_pred <- sample_pa_draws(proj$PA_atc, n_iter, pa_cv)
count_rate_ppd <- vector("list", 5)
for (k in 1:5) {
  rate_k <- exp(eta_pred_proj[, , k])
  lambda <- rate_k * pa_mat_pred
  count_draw <- matrix(NA_real_, nrow = n_iter, ncol = ncol(pa_mat_pred))
  valid <- is.finite(lambda) & lambda >= 0 & is.finite(pa_mat_pred) & pa_mat_pred > 0
  count_draw[valid] <- rpois(sum(valid), lambda[valid])
  out <- matrix(NA_real_, nrow = n_iter, ncol = ncol(pa_mat_pred))
  out[valid] <- count_draw[valid] / pa_mat_pred[valid]
  count_rate_ppd[[k]] <- out
}

avg_lat_ppd <- matrix(
  rnorm(
    length(eta_pred_proj[, , 6]),
    mean = as.vector(eta_pred_proj[, , 6]),
    sd = as.vector(matrix(sigma_cont[, 1], nrow = n_iter, ncol = ncol(eta_pred_proj[, , 6])))
  ),
  nrow = n_iter
)
obp_lat_ppd <- matrix(
  rnorm(
    length(eta_pred_proj[, , 7]),
    mean = as.vector(eta_pred_proj[, , 7]),
    sd = as.vector(matrix(sigma_cont[, 2], nrow = n_iter, ncol = ncol(eta_pred_proj[, , 7])))
  ),
  nrow = n_iter
)
slg_lat_ppd <- matrix(
  rnorm(
    length(eta_pred_proj[, , 8]),
    mean = as.vector(eta_pred_proj[, , 8]),
    sd = as.vector(matrix(sigma_cont[, 3], nrow = n_iter, ncol = ncol(eta_pred_proj[, , 8])))
  ),
  nrow = n_iter
)
avg_pred <- inv_logit(avg_lat_ppd)
obp_pred <- inv_logit(obp_lat_ppd)
slg_pred <- pmax(exp(slg_lat_ppd) - epsilon, 0)

proj <- bind_cols(
  proj,
  setNames(as.data.frame(summarize_matrix(count_rate_ppd[[1]])), c("H_mean", "H_p05", "H_p50", "H_p95")),
  setNames(as.data.frame(summarize_matrix(count_rate_ppd[[2]])), c("R_mean", "R_p05", "R_p50", "R_p95")),
  setNames(as.data.frame(summarize_matrix(count_rate_ppd[[3]])), c("RBI_mean", "RBI_p05", "RBI_p50", "RBI_p95")),
  setNames(as.data.frame(summarize_matrix(count_rate_ppd[[4]])), c("HR_mean", "HR_p05", "HR_p50", "HR_p95")),
  setNames(as.data.frame(summarize_matrix(count_rate_ppd[[5]])), c("SB_mean", "SB_p05", "SB_p50", "SB_p95")),
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

statcast_covars <- c("EV", "LA", "BarrelPct", "HardHitPct")
missing_statcast <- setdiff(statcast_covars, names(raw))
if (length(missing_statcast) > 0) {
  stop(
    "Missing Statcast covariates in hitter input for plotting: ",
    paste(missing_statcast, collapse = ", ")
  )
}

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

for (v in statcast_covars) {
  raw[[v]] <- suppressWarnings(as.numeric(raw[[v]]))
  mu <- mean(raw[[v]], na.rm = TRUE)
  sdv <- sd(raw[[v]], na.rm = TRUE)
  if (is.na(mu)) mu <- 0
  if (is.na(sdv) || sdv == 0) sdv <- 1
  raw[[v]] <- dplyr::coalesce(raw[[v]], mu)
  raw[[paste0(v, "_z")]] <- (raw[[v]] - mu) / sdv
}

years <- sort(unique(raw$Season))

beta <- post$beta
u_pos <- post$u_pos
u_player <- post$u_player
year_effect <- post$year_effect
K <- dim(beta)[3]
J_player <- dim(u_player)[3]

get_or_default <- function(x, default) {
  if (is.null(x)) return(default)
  x
}

beta_ev_lat <- get_or_default(post$beta_ev_lat, matrix(0, nrow = n_iter, ncol = 3))
beta_la_lat <- get_or_default(post$beta_la_lat, matrix(0, nrow = n_iter, ncol = 3))
beta_barrel_lat <- get_or_default(post$beta_barrel_lat, matrix(0, nrow = n_iter, ncol = 3))
beta_hardhit_lat <- get_or_default(post$beta_hardhit_lat, matrix(0, nrow = n_iter, ncol = 3))
beta_ev_out <- get_or_default(post$beta_ev_out, matrix(0, nrow = n_iter, ncol = 7))
beta_la_out <- get_or_default(post$beta_la_out, matrix(0, nrow = n_iter, ncol = 7))
beta_barrel_out <- get_or_default(post$beta_barrel_out, matrix(0, nrow = n_iter, ncol = 7))
beta_hardhit_out <- get_or_default(post$beta_hardhit_out, matrix(0, nrow = n_iter, ncol = 7))
u_player_statcast <- get_or_default(post$u_player_statcast, array(0, dim = c(n_iter, J_player, 4)))
u_player_bbe <- get_or_default(post$u_player_bbe, array(0, dim = c(n_iter, J_player, 4)))

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
      for (p in seq_len(ncol(X))) {
        eta[, k] <- eta[, k] + beta[, p, k] * x_i[p]
      }
    }
    for (r in seq_len(ncol(Z_pos))) {
      for (k in 1:K) {
        eta[, k] <- eta[, k] + zp_i[r] * u_pos[, r, pos, k]
      }
    }
    for (r in seq_len(ncol(Z_player))) {
      for (k in 1:K) {
        eta[, k] <- eta[, k] + zpl_i[r] * u_player[, r, pid, k]
      }
    }
    for (k in 1:K) {
      eta[, k] <- eta[, k] + year_effect[, k, yid]
    }

    ev_lat <- beta_ev_lat[, 1] + beta_ev_lat[, 2] * x_i[2] + beta_ev_lat[, 3] * x_i[3] +
      u_player_statcast[, pid, 1] + x_i[2] * u_player_statcast[, pid, 2]
    la_lat <- beta_la_lat[, 1] + beta_la_lat[, 2] * x_i[2] + beta_la_lat[, 3] * x_i[3] +
      u_player_statcast[, pid, 3] + x_i[2] * u_player_statcast[, pid, 4]
    barrel_lat <- beta_barrel_lat[, 1] + beta_barrel_lat[, 2] * x_i[2] + beta_barrel_lat[, 3] * x_i[3] +
      u_player_bbe[, pid, 1] + x_i[2] * u_player_bbe[, pid, 2]
    hardhit_lat <- beta_hardhit_lat[, 1] + beta_hardhit_lat[, 2] * x_i[2] + beta_hardhit_lat[, 3] * x_i[3] +
      u_player_bbe[, pid, 3] + x_i[2] * u_player_bbe[, pid, 4]

    for (k in c(1, 2, 3, 4, 6, 7, 8)) {
      idx <- if (k < 5) k else (k - 1)
      eta[, k] <- eta[, k] + beta_ev_out[, idx] * ev_lat +
        beta_la_out[, idx] * la_lat +
        beta_barrel_out[, idx] * barrel_lat +
        beta_hardhit_out[, idx] * hardhit_lat
    }

    rate_count <- exp(eta[, 1:5])
    pa_hist <- subset$PA[i]
    count_rate_hist <- matrix(NA_real_, nrow = n_iter, ncol = 5)
    if (!is.na(pa_hist) && pa_hist > 0) {
      for (k in 1:5) {
        count_rate_hist[, k] <- rpois(n_iter, rate_count[, k] * pa_hist) / pa_hist
      }
    }
    avg_pred <- inv_logit(rnorm(n_iter, eta[, 6], sigma_cont[, 1]))
    obp_pred <- inv_logit(rnorm(n_iter, eta[, 7], sigma_cont[, 2]))
    slg_pred <- pmax(exp(rnorm(n_iter, eta[, 8], sigma_cont[, 3])) - 1e-4, 0)

    summaries[[i]] <- list(
      H = summarize_draws(count_rate_hist[,1]),
      R = summarize_draws(count_rate_hist[,2]),
      RBI = summarize_draws(count_rate_hist[,3]),
      HR = summarize_draws(count_rate_hist[,4]),
      SB = summarize_draws(count_rate_hist[,5]),
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

  write_csv(plot_df, file.path(results_dir, paste0('trends_top100_', o, '_data.csv')))

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
      title = paste0(
        if (o %in% c('H','R','RBI','HR','SB')) paste0(o, ' (per PA)') else o,
        ': observed (black), fitted (gold), 2026 projection (blue)'
      ),
      y = if (o %in% c('H','R','RBI','HR','SB')) paste0(o, ' per PA') else o,
      x = 'Season'
    ) +
    theme(
      legend.position = 'none',
      strip.text = element_text(face = 'bold'),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave(filename = file.path(results_dir, paste0('trends_top100_', o, '.pdf')),
         plot = p, width = 18, height = 12)
}

cat('Wrote plots to', results_dir, '\n')
