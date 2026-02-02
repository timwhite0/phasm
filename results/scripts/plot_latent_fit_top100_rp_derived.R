suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})

fit_path <- "models/rp_model_fit.rds"
prep_path <- "models/rp_model_inputs.rds"
input_path <- "data/fangraphs_pitchers_2018_2025.csv"
atc_ip_path <- "data/atc_ip_projections_2026.csv"
results_dir <- "results/plots/fitted_outcome_curves/pitchers/relievers"

if (!dir.exists("results")) dir.create("results")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

fit <- readRDS(fit_path)
prep <- readRDS(prep_path)
raw <- read_csv(input_path, show_col_types = FALSE) %>%
  mutate(Season = as.integer(Season)) %>%
  filter(Season >= 2018, Season <= 2025) %>%
  filter(Role == "RP")

atc <- read_csv(atc_ip_path, show_col_types = FALSE)

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
  filter(!is.na(playerid), !is.na(IP_atc))

age_mean <- mean(raw$Age, na.rm = TRUE)
age_sd <- sd(raw$Age, na.rm = TRUE)
raw <- raw %>%
  mutate(
    age_c = (Age - age_mean) / age_sd,
    age2 = age_c^2,
    player_id = match(as.integer(playerid), prep$player_lookup$playerid),
    year_id = match(Season, prep$years)
  ) %>%
  filter(!is.na(player_id), !is.na(year_id)) %>%
  mutate(row_id = row_number())

post <- rstan::extract(fit)
beta <- post$beta
u_player <- post$u_player
year_effect <- post$year_effect
n_iter <- dim(beta)[1]
K <- dim(beta)[3]

summarize_draws <- function(x) {
  c(
    mean = mean(x, na.rm = TRUE),
    p05 = as.numeric(quantile(x, 0.05, na.rm = TRUE)),
    p50 = as.numeric(quantile(x, 0.5, na.rm = TRUE)),
    p95 = as.numeric(quantile(x, 0.95, na.rm = TRUE))
  )
}

X_obs <- cbind(
  intercept = 1,
  age_c = raw$age_c,
  age2 = raw$age2
)
Z_player_obs <- cbind(
  intercept = 1,
  age_c = raw$age_c
)

player_id <- raw$player_id
year_id <- raw$year_id

eta_obs <- function(i) {
  x_i <- X_obs[i, ]
  zpl_i <- Z_player_obs[i, ]
  pid <- player_id[i]
  yid <- year_id[i]

  eta <- matrix(0, nrow = n_iter, ncol = K)
  for (k in 1:K) {
    eta[, k] <- beta[, 1, k] * x_i[1] + beta[, 2, k] * x_i[2] + beta[, 3, k] * x_i[3]
  }
  for (r in 1:2) {
    for (k in 1:K) {
      eta[, k] <- eta[, k] + zpl_i[r] * u_player[, r, pid, k]
    }
  }
  for (k in 1:K) {
    eta[, k] <- eta[, k] + year_effect[, k, yid]
  }
  eta
}

proj_all <- prep$player_lookup %>%
  mutate(playerid = as.character(playerid)) %>%
  left_join(atc, by = "playerid")

keep_idx <- which(!is.na(proj_all$IP_atc))
if (length(keep_idx) == 0) {
  stop("No ATC IP matches found for 2026 projections.")
}

proj <- proj_all[keep_idx, ]

eta_pred <- post$eta_pred
rate_pred <- exp(eta_pred[, keep_idx, , drop = FALSE])

metric_draws <- list(
  ERA = rate_pred[, , 4] * 9,
  K9 = rate_pred[, , 1] * 9,
  WHIP = rate_pred[, , 2] + rate_pred[, , 3]
)

proj_summaries <- lapply(metric_draws, function(mat) {
  t(apply(mat, 2, summarize_draws))
})

proj_metrics <- proj %>%
  select(playerid, PlayerName, role = Role) %>%
  distinct()

add_metric_cols <- function(df, metric, mat) {
  cols <- setNames(as.data.frame(mat), paste0(metric, "_", c("mean", "p05", "p50", "p95")))
  bind_cols(df, cols)
}

for (metric in names(proj_summaries)) {
  proj_metrics <- add_metric_cols(proj_metrics, metric, proj_summaries[[metric]])
}

metrics <- c("ERA", "K9", "WHIP")

top_players <- list()
for (metric in metrics) {
  mean_col <- paste0(metric, "_mean")
  if (metric %in% c("ERA", "WHIP")) {
    ids <- proj_metrics %>%
      arrange(.data[[mean_col]]) %>%
      slice(1:100) %>%
      pull(playerid)
  } else {
    ids <- proj_metrics %>%
      arrange(desc(.data[[mean_col]])) %>%
      slice(1:100) %>%
      pull(playerid)
  }
  top_players[[metric]] <- ids
}

for (metric in metrics) {
  ids <- top_players[[metric]]
  subset <- raw %>% filter(as.character(playerid) %in% ids)
  if (nrow(subset) == 0) next

  summaries <- vector("list", nrow(subset))
  for (i in seq_len(nrow(subset))) {
    eta <- eta_obs(subset$row_id[i])
    rate <- exp(eta)
    ip <- subset$IP[i]

    draws <- switch(
      metric,
      ERA = rate[, 4] * 9,
      K9 = rate[, 1] * 9,
      WHIP = rate[, 2] + rate[, 3]
    )
    summaries[[i]] <- summarize_draws(draws)
  }

  plot_rows <- list()
  for (i in seq_len(nrow(subset))) {
    obs <- subset[i, ]
    obs_val <- switch(
      metric,
      ERA = (obs$ER / obs$IP) * 9,
      K9 = (obs$SO / obs$IP) * 9,
      WHIP = (obs$BB + obs$H) / obs$IP
    )
    sum_o <- summaries[[i]]
    plot_rows[[length(plot_rows) + 1]] <- data.frame(
      playerid = as.character(obs$playerid),
      PlayerName = obs$PlayerName,
      Season = obs$Season,
      metric = metric,
      observed = obs_val,
      fitted_mean = sum_o["mean"],
      fitted_p05 = sum_o["p05"],
      fitted_p95 = sum_o["p95"],
      type = "fit",
      stringsAsFactors = FALSE
    )
  }

  plot_df <- bind_rows(plot_rows) %>% arrange(PlayerName, Season)

  mean_col <- paste0(metric, "_mean")
  p05_col <- paste0(metric, "_p05")
  p95_col <- paste0(metric, "_p95")
  proj_df <- proj_metrics %>%
    filter(playerid %in% ids) %>%
    transmute(
      playerid,
      PlayerName,
      Season = 2026L,
      metric = metric,
      observed = NA_real_,
      fitted_mean = .data[[mean_col]],
      fitted_p05 = .data[[p05_col]],
      fitted_p95 = .data[[p95_col]],
      type = "projection"
    )

  plot_df <- bind_rows(plot_df, proj_df)

  order_df <- plot_df %>%
    filter(Season == 2026L)
  if (metric %in% c("ERA", "WHIP")) {
    order_df <- order_df %>% arrange(fitted_mean)
  } else {
    order_df <- order_df %>% arrange(desc(fitted_mean))
  }
  order_df <- order_df %>% distinct(PlayerName)
  plot_df$PlayerName <- factor(plot_df$PlayerName, levels = order_df$PlayerName)

  write_csv(plot_df, file.path(results_dir, paste0("rp_latent_fit_derived_", metric, "_data.csv")))

  p <- ggplot(plot_df, aes(x = Season, group = PlayerName)) +
    geom_linerange(aes(ymin = fitted_p05, ymax = fitted_p95, color = type), linewidth = 0.6, alpha = 0.7, na.rm = TRUE) +
    geom_line(data = plot_df %>% filter(type == "fit"), aes(y = fitted_mean), color = "goldenrod", linewidth = 0.7) +
    geom_point(data = plot_df %>% filter(type == "fit"), aes(y = fitted_mean), color = "goldenrod", size = 1.6) +
    geom_point(data = plot_df %>% filter(type == "projection"), aes(y = fitted_mean), color = "dodgerblue", size = 1.8, shape = 17) +
    geom_point(aes(y = observed), color = "black", size = 1.4, na.rm = TRUE) +
    facet_wrap(~ PlayerName, scales = "fixed") +
    theme_minimal(base_size = 10) +
    scale_x_continuous(breaks = 2018:2026) +
    scale_color_manual(values = c(fit = "goldenrod", projection = "dodgerblue")) +
    labs(
      title = paste0(metric, ": observed (black), fitted (gold), 2026 projection (blue)"),
      y = metric,
      x = "Season"
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave(filename = file.path(results_dir, paste0("rp_latent_fit_derived_", metric, ".pdf")),
         plot = p, width = 18, height = 12)
}

cat("Wrote derived pitcher latent fit plots to", results_dir, "\n")
