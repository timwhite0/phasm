suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(forcats)
  library(stringr)
})

fit_path <- "models/rp_model_fit.rds"
prep_path <- "models/rp_model_inputs.rds"
atc_ip_path <- "data/atc_ip_projections_2026.csv"
results_dir <- "results/plots/interval_projections/pitchers/relievers"

if (!dir.exists("results")) dir.create("results")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

fit <- readRDS(fit_path)
prep <- readRDS(prep_path)

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

proj_all <- prep$player_lookup %>%
  mutate(playerid = as.character(playerid)) %>%
  left_join(atc, by = "playerid")

keep_idx <- which(!is.na(proj_all$IP_atc))
if (length(keep_idx) == 0) {
  stop("No ATC IP matches found for 2026 projections.")
}

proj <- proj_all[keep_idx, ]

post <- rstan::extract(fit)
eta_pred <- post$eta_pred
n_iter <- dim(eta_pred)[1]

rate_pred <- exp(eta_pred[, keep_idx, , drop = FALSE])

ip_mat <- matrix(rep(proj$IP_atc, each = n_iter), nrow = n_iter)

metric_draws <- list(
  ERA = rate_pred[, , 4] * 9,
  K9 = rate_pred[, , 1] * 9,
  BB9 = rate_pred[, , 2] * 9,
  WHIP = rate_pred[, , 2] + rate_pred[, , 3],
  Ks = rate_pred[, , 1] * ip_mat,
  W = rate_pred[, , 5] * ip_mat,
  SVHLD = rate_pred[, , 6] * ip_mat
)

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

proj_metrics <- proj %>%
  select(playerid, PlayerName, role = Role) %>%
  distinct()

for (metric in names(metric_draws)) {
  mat <- summarize_matrix(metric_draws[[metric]])
  cols <- setNames(as.data.frame(mat), paste0(metric, "_", c("mean", "p05", "p50", "p95")))
  proj_metrics <- bind_cols(proj_metrics, cols)
}

metrics <- c("ERA", "WHIP", "K9", "BB9", "Ks", "W", "SVHLD")

plot_role <- function(role_name) {
  role_df <- proj_metrics %>%
    filter(role == role_name)
  if (nrow(role_df) == 0) return(NULL)

  plot_df <- list()
  for (metric in metrics) {
    mean_col <- paste0(metric, "_mean")
    p05_col <- paste0(metric, "_p05")
    p95_col <- paste0(metric, "_p95")

    metric_df <- role_df %>%
      select(playerid, PlayerName, all_of(mean_col), all_of(p05_col), all_of(p95_col)) %>%
      rename(mean = all_of(mean_col), p05 = all_of(p05_col), p95 = all_of(p95_col)) %>%
      mutate(metric = metric)

    metric_df <- if (metric %in% c("ERA", "WHIP", "BB9")) {
      metric_df %>% arrange(mean) %>% slice(1:30)
    } else {
      metric_df %>% arrange(desc(mean)) %>% slice(1:30)
    }

    desc_flag <- metric %in% c("ERA", "WHIP", "BB9")
    metric_df <- metric_df %>%
      mutate(
        PlayerName_plot = paste(PlayerName, metric, sep = "___"),
        PlayerName_plot = fct_reorder(PlayerName_plot, mean, .desc = desc_flag)
      )

    plot_df[[metric]] <- metric_df
  }

  plot_df <- bind_rows(plot_df)
  if (nrow(plot_df) == 0) return(NULL)

  p <- ggplot(plot_df, aes(x = PlayerName_plot, y = mean)) +
    geom_linerange(aes(ymin = p05, ymax = p95), color = "gray45", linewidth = 0.6) +
    geom_point(color = "dodgerblue", size = 1.6) +
    coord_flip() +
    facet_wrap(~ metric, scales = "free") +
    scale_x_discrete(labels = function(x) str_replace(x, "___.*", ""), drop = TRUE) +
    theme_minimal(base_size = 11) +
    labs(
      title = paste0("2026 projections by metric: ", role_name),
      x = NULL,
      y = NULL
    ) +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  out_role <- str_replace_all(role_name, "[^A-Za-z0-9]+", "_")
  out_path <- file.path(results_dir, paste0("rp_intervals_2026_", out_role, ".pdf"))
  ggsave(out_path, plot = p, width = 12, height = 14)
}

roles <- sort(unique(proj_metrics$role))
for (role_name in roles) {
  plot_role(role_name)
}

cat("Wrote pitcher role interval plots to", results_dir, "\n")
