suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(rstan)
})

fit_path <- "models/rp_model_fit.rds"
eta_pred_path <- Sys.getenv("RP_ETA_PRED_PATH", "models/rp_eta_pred_2026.rds")
prep_path <- "models/rp_model_inputs.rds"
category_path <- "results/projections/pitchers/rp_category_projections_2026.csv"
out_csv <- "results/projections/pitchers/rp_composite_rank_2026.csv"
out_md <- "results/projections/pitchers/top50_rp_composite_by_role.md"

draw_cap <- as.integer(Sys.getenv("UNCERTAINTY_DRAW_CAP", "0"))
set_seed <- as.integer(Sys.getenv("UNCERTAINTY_SEED", "123"))
ip_cv <- as.numeric(Sys.getenv("RP_IP_ATC_CV", "0.15"))

prep <- readRDS(prep_path)
proj <- read_csv(category_path, show_col_types = FALSE) %>%
  mutate(playerid = as.character(playerid), role = if_else(is.na(role) | role == "", "UNK", role))

keep <- which(!is.na(proj$IP_atc) & proj$IP_atc > 0)
if (length(keep) == 0) {
  stop("No players with non-missing positive IP_atc in RP projections.")
}

sample_ip_draws <- function(ip_vec, n_draw, cv) {
  if (!is.finite(cv) || cv <= 0) {
    return(matrix(rep(ip_vec, each = n_draw), nrow = n_draw))
  }
  shape <- 1 / (cv^2)
  scale_vec <- ip_vec / shape
  matrix(
    rgamma(n_draw * length(ip_vec), shape = shape, scale = rep(scale_vec, each = n_draw)),
    nrow = n_draw
  )
}

if (file.exists(eta_pred_path)) {
  eta_obj <- readRDS(eta_pred_path)
  eta_pred <- eta_obj$eta_pred
} else {
  fit <- readRDS(fit_path)
  eta_pred <- rstan::extract(fit, pars = "eta_pred")$eta_pred
}
n_draw <- dim(eta_pred)[1]

if (!is.na(draw_cap) && draw_cap > 0 && draw_cap < n_draw) {
  set.seed(set_seed)
  draw_idx <- sort(sample.int(n_draw, draw_cap, replace = FALSE))
  eta_pred <- eta_pred[draw_idx, , , drop = FALSE]
  n_draw <- dim(eta_pred)[1]
}

lookup_ids <- as.character(prep$player_lookup$playerid)
eta_idx <- match(proj$playerid[keep], lookup_ids)
if (any(is.na(eta_idx))) {
  stop("Could not align RP projection player ids to eta_pred indices.")
}

eta <- eta_pred[, eta_idx, , drop = FALSE]
drop3 <- function(x) {
  dim(x) <- dim(x)[1:2]
  x
}

set.seed(set_seed)
ip_mat <- sample_ip_draws(proj$IP_atc[keep], n_draw, ip_cv)

so_rate <- exp(drop3(eta[, , 1, drop = FALSE]))
bb_rate <- exp(drop3(eta[, , 2, drop = FALSE]))
h_rate <- exp(drop3(eta[, , 3, drop = FALSE]))
er_rate <- exp(drop3(eta[, , 4, drop = FALSE]))
w_rate <- exp(drop3(eta[, , 5, drop = FALSE]))
svhld_rate <- exp(drop3(eta[, , 6, drop = FALSE]))

draw_poisson <- function(rate_mat, exposure_mat) {
  lambda <- rate_mat * exposure_mat
  matrix(rpois(length(lambda), lambda), nrow = nrow(rate_mat), ncol = ncol(rate_mat))
}

so_count <- draw_poisson(so_rate, ip_mat)
bb_count <- draw_poisson(bb_rate, ip_mat)
h_count <- draw_poisson(h_rate, ip_mat)
er_count <- draw_poisson(er_rate, ip_mat)
w_count <- draw_poisson(w_rate, ip_mat)
svhld_count <- draw_poisson(svhld_rate, ip_mat)

ERA <- (er_count / ip_mat) * 9
WHIP <- (bb_count + h_count) / ip_mat
Ks <- so_count
W <- w_count
SVH <- svhld_count
IP <- ip_mat

zscore_rows <- function(mat) {
  mu <- rowMeans(mat, na.rm = TRUE)
  sdv <- apply(mat, 1, sd, na.rm = TRUE)
  sdv[is.na(sdv) | sdv == 0] <- NA_real_
  out <- sweep(mat, 1, mu, "-")
  out <- sweep(out, 1, sdv, "/")
  out
}

z_ERA <- -zscore_rows(ERA)
z_WHIP <- -zscore_rows(WHIP)
z_IP <- zscore_rows(IP)
z_W <- zscore_rows(W)
z_Ks <- zscore_rows(Ks)
z_SVH <- zscore_rows(SVH)

comp_draw <- (z_ERA + z_WHIP + z_IP + z_W + z_Ks + z_SVH) / 6

rank_overall <- t(apply(comp_draw, 1, function(x) rank(-x, ties.method = "average")))
role_vec <- proj$role[keep]
role_levels <- sort(unique(role_vec))
rank_role <- matrix(NA_real_, nrow = n_draw, ncol = ncol(comp_draw))
for (r in role_levels) {
  idx <- which(role_vec == r)
  rank_role[, idx] <- t(apply(comp_draw[, idx, drop = FALSE], 1, function(x) rank(-x, ties.method = "average")))
}

summ <- tibble(
  playerid = proj$playerid[keep],
  PlayerName = proj$PlayerName[keep],
  Team = proj$Team[keep],
  role = role_vec,
  IP_atc = proj$IP_atc[keep],
  z_ERA_p50 = apply(z_ERA, 2, quantile, probs = 0.50, na.rm = TRUE),
  z_WHIP_p50 = apply(z_WHIP, 2, quantile, probs = 0.50, na.rm = TRUE),
  z_IP_p50 = apply(z_IP, 2, quantile, probs = 0.50, na.rm = TRUE),
  z_W_p50 = apply(z_W, 2, quantile, probs = 0.50, na.rm = TRUE),
  z_Ks_p50 = apply(z_Ks, 2, quantile, probs = 0.50, na.rm = TRUE),
  z_SVH_p50 = apply(z_SVH, 2, quantile, probs = 0.50, na.rm = TRUE),
  composite_mean = colMeans(comp_draw, na.rm = TRUE),
  composite_p05 = apply(comp_draw, 2, quantile, probs = 0.05, na.rm = TRUE),
  composite_p50 = apply(comp_draw, 2, quantile, probs = 0.50, na.rm = TRUE),
  composite_p95 = apply(comp_draw, 2, quantile, probs = 0.95, na.rm = TRUE),
  expected_rank_overall = colMeans(rank_overall, na.rm = TRUE),
  p_top20_overall = colMeans(rank_overall <= 20, na.rm = TRUE),
  p_top50_overall = colMeans(rank_overall <= 50, na.rm = TRUE),
  expected_rank_role = colMeans(rank_role, na.rm = TRUE),
  p_top20_role = colMeans(rank_role <= 20, na.rm = TRUE),
  p_top50_role = colMeans(rank_role <= 50, na.rm = TRUE)
)

write_csv(summ, out_csv)

fmt <- function(x, d = 3) ifelse(is.na(x), "NA", sprintf(paste0("%.", d, "f"), x))
make_table <- function(df) {
  header <- paste(
    "| Rank | Player | Team | Exp rank (role) | P(top20 role) | P(top50 role) | Comp p50 | Comp 90% CI |",
    "|---:|---|---|---:|---:|---:|---:|---|",
    sep = "\n"
  )
  rows <- paste0(
    "| ", seq_len(nrow(df)),
    " | ", df$PlayerName,
    " | ", ifelse(is.na(df$Team), "", df$Team),
    " | ", fmt(df$expected_rank_role, 1),
    " | ", fmt(df$p_top20_role, 3),
    " | ", fmt(df$p_top50_role, 3),
    " | ", fmt(df$composite_p50, 3),
    " | [", fmt(df$composite_p05, 3), ", ", fmt(df$composite_p95, 3), "] |"
  )
  paste0(header, "\n", paste(rows, collapse = "\n"), "\n")
}

lines <- c(
  "# Top 50 RP composite rankings by role (2026)",
  "",
  "## Method",
  "- Rank pitchers within each posterior predictive draw using draw-level RP composite z-scores.",
  "- Composite uses ERA (flipped), WHIP (flipped), IP, W, Ks, and SV+HLD.",
  paste0("- IP is treated as uncertain: Gamma(mean = IP_atc, CV = ", format(ip_cv, trim = TRUE), ")."),
  paste0("- Summaries below use ", n_draw, " posterior draws."),
  "",
  "## Top 50 by role",
  ""
)

for (r in role_levels) {
  top_r <- summ %>%
    filter(role == r) %>%
    arrange(expected_rank_role) %>%
    slice_head(n = 50)
  lines <- c(lines, paste0("### ", r), "", make_table(top_r), "")
}

writeLines(lines, out_md)
cat("Wrote ", out_csv, "\n", sep = "")
cat("Wrote ", out_md, "\n", sep = "")
