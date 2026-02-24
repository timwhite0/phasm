suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(rstan)
  library(stringr)
})

fit_path <- "models/hitter_model_fit.rds"
prep_path <- "models/hitter_model_inputs.rds"
category_path <- "results/projections/batters/category_projections_2026.csv"
out_csv <- "results/projections/batters/composite_rank_2026.csv"
out_md <- "results/projections/batters/top20_hitter_composite_by_position.md"

# Optional draw cap for faster reruns on large fits.
draw_cap <- as.integer(Sys.getenv("UNCERTAINTY_DRAW_CAP", "0"))
set_seed <- as.integer(Sys.getenv("UNCERTAINTY_SEED", "123"))
pa_cv <- as.numeric(Sys.getenv("HITTER_PA_ATC_CV", "0.10"))

fit <- readRDS(fit_path)
prep <- readRDS(prep_path)
cat_proj <- read_csv(category_path, show_col_types = FALSE) %>%
  mutate(playerid = as.character(playerid))

lookup <- prep$player_lookup %>%
  mutate(playerid = as.character(playerid)) %>%
  select(playerid, PlayerName)

meta <- lookup %>%
  left_join(
    cat_proj %>% select(playerid, Team, position, PA_atc),
    by = "playerid"
  ) %>%
  mutate(
    position = if_else(is.na(position) | position == "", "UNK", position),
    pos1 = str_trim(str_replace(position, "/.*", ""))
  )

keep <- which(!is.na(meta$PA_atc) & meta$PA_atc > 0)
if (length(keep) == 0) {
  stop("No players with non-missing positive PA_atc in category projections.")
}

has_phi_count <- "phi_count" %in% fit@sim$pars_oi
extract_pars <- if (has_phi_count) c("eta_pred", "phi_count") else "eta_pred"
post <- rstan::extract(fit, pars = extract_pars)
eta_pred <- post$eta_pred
phi_count <- post$phi_count
if (!has_phi_count) {
  warning("phi_count not found in fit; falling back to Poisson posterior predictive draws for counts.")
}
n_draw <- dim(eta_pred)[1]

if (!is.na(draw_cap) && draw_cap > 0 && draw_cap < n_draw) {
  set.seed(set_seed)
  draw_idx <- sort(sample.int(n_draw, draw_cap, replace = FALSE))
  eta_pred <- eta_pred[draw_idx, , , drop = FALSE]
  if (!is.null(phi_count)) {
    if (is.matrix(phi_count)) {
      phi_count <- phi_count[draw_idx, , drop = FALSE]
    } else if (length(phi_count) == n_draw * 5) {
      phi_count <- matrix(phi_count, nrow = n_draw, byrow = FALSE)[draw_idx, , drop = FALSE]
    }
  }
  n_draw <- dim(eta_pred)[1]
}

drop3 <- function(x) {
  dim(x) <- dim(x)[1:2]
  x
}
eta <- eta_pred[, keep, , drop = FALSE]
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
draw_counts <- function(rate_mat, exposure_mat, phi_draw = NULL) {
  lambda <- rate_mat * exposure_mat
  out <- matrix(NA_real_, nrow = nrow(rate_mat), ncol = ncol(rate_mat))
  valid <- is.finite(lambda) & lambda >= 0 & is.finite(exposure_mat) & exposure_mat > 0
  if (is.null(phi_draw)) {
    out[valid] <- rpois(sum(valid), lambda[valid])
    return(out)
  }
  phi_mat <- matrix(phi_draw, nrow = nrow(rate_mat), ncol = ncol(rate_mat))
  valid <- valid & is.finite(phi_mat) & phi_mat > 0
  out[valid] <- rnbinom(sum(valid), mu = lambda[valid], size = phi_mat[valid])
  out
}
set.seed(set_seed)
pa <- sample_pa_draws(meta$PA_atc[keep], n_draw, pa_cv)

inv_logit <- function(x) 1 / (1 + exp(-x))

get_phi <- function(k) {
  if (is.null(phi_count)) return(NULL)
  if (is.matrix(phi_count)) return(phi_count[, k])
  if (length(phi_count) == n_draw * 5) return(matrix(phi_count, nrow = n_draw, byrow = FALSE)[, k])
  NULL
}
HR <- draw_counts(exp(drop3(eta[, , 4, drop = FALSE])), pa, get_phi(4))
R <- draw_counts(exp(drop3(eta[, , 2, drop = FALSE])), pa, get_phi(2))
RBI <- draw_counts(exp(drop3(eta[, , 3, drop = FALSE])), pa, get_phi(3))
SB <- draw_counts(exp(drop3(eta[, , 5, drop = FALSE])), pa, get_phi(5))
OBP <- inv_logit(drop3(eta[, , 7, drop = FALSE]))
SLG <- pmax(exp(drop3(eta[, , 8, drop = FALSE])) - 1e-4, 0)

zscore_rows <- function(mat) {
  mu <- rowMeans(mat, na.rm = TRUE)
  sdv <- apply(mat, 1, sd, na.rm = TRUE)
  sdv[is.na(sdv) | sdv == 0] <- NA_real_
  out <- sweep(mat, 1, mu, "-")
  out <- sweep(out, 1, sdv, "/")
  out
}

z_HR <- zscore_rows(HR)
z_R <- zscore_rows(R)
z_RBI <- zscore_rows(RBI)
z_SB <- zscore_rows(SB)
z_OBP <- zscore_rows(OBP)
z_SLG <- zscore_rows(SLG)

comp_draw <- (z_HR + z_R + z_RBI + z_SB + z_OBP + z_SLG) / 6

rank_overall <- t(apply(comp_draw, 1, function(x) rank(-x, ties.method = "average")))

pos_vec <- meta$pos1[keep]
pos_levels <- sort(unique(pos_vec))
rank_pos <- matrix(NA_real_, nrow = n_draw, ncol = ncol(comp_draw))
for (p in pos_levels) {
  idx <- which(pos_vec == p)
  rank_pos[, idx] <- t(apply(comp_draw[, idx, drop = FALSE], 1, function(x) rank(-x, ties.method = "average")))
}

summ <- tibble(
  playerid = meta$playerid[keep],
  PlayerName = meta$PlayerName[keep],
  Team = meta$Team[keep],
  position = meta$position[keep],
  pos1 = pos_vec,
  PA_atc = meta$PA_atc[keep],
  z_HR_p50 = apply(z_HR, 2, quantile, probs = 0.50, na.rm = TRUE),
  z_R_p50 = apply(z_R, 2, quantile, probs = 0.50, na.rm = TRUE),
  z_RBI_p50 = apply(z_RBI, 2, quantile, probs = 0.50, na.rm = TRUE),
  z_SB_p50 = apply(z_SB, 2, quantile, probs = 0.50, na.rm = TRUE),
  z_OBP_p50 = apply(z_OBP, 2, quantile, probs = 0.50, na.rm = TRUE),
  z_SLG_p50 = apply(z_SLG, 2, quantile, probs = 0.50, na.rm = TRUE),
  composite_mean = colMeans(comp_draw, na.rm = TRUE),
  composite_p05 = apply(comp_draw, 2, quantile, probs = 0.05, na.rm = TRUE),
  composite_p50 = apply(comp_draw, 2, quantile, probs = 0.50, na.rm = TRUE),
  composite_p95 = apply(comp_draw, 2, quantile, probs = 0.95, na.rm = TRUE),
  expected_rank_overall = colMeans(rank_overall, na.rm = TRUE),
  p_top20_overall = colMeans(rank_overall <= 20, na.rm = TRUE),
  p_top50_overall = colMeans(rank_overall <= 50, na.rm = TRUE),
  expected_rank_pos = colMeans(rank_pos, na.rm = TRUE),
  p_top12_pos = colMeans(rank_pos <= 12, na.rm = TRUE),
  p_top20_pos = colMeans(rank_pos <= 20, na.rm = TRUE)
)

write_csv(summ, out_csv)

fmt <- function(x, d = 3) ifelse(is.na(x), "NA", sprintf(paste0("%.", d, "f"), x))

make_table <- function(df) {
  header <- paste(
    "| Rank | Player | Team | Exp rank (pos) | P(top12 pos) | P(top20 pos) | Comp p50 | Comp 90% CI |",
    "|---:|---|---|---:|---:|---:|---:|---|",
    sep = "\n"
  )
  rows <- paste0(
    "| ", seq_len(nrow(df)),
    " | ", df$PlayerName,
    " | ", ifelse(is.na(df$Team), "NA", df$Team),
    " | ", fmt(df$expected_rank_pos, 1),
    " | ", fmt(df$p_top12_pos, 3),
    " | ", fmt(df$p_top20_pos, 3),
    " | ", fmt(df$composite_p50, 3),
    " | [", fmt(df$composite_p05, 3), ", ", fmt(df$composite_p95, 3), "] |"
  )
  paste0(header, "\n", paste(rows, collapse = "\n"), "\n")
}

make_overall_table <- function(df) {
  header <- paste(
    "| Rank | Player | Team | Position | Exp rank (overall) | P(top20 overall) | P(top50 overall) | Comp p50 | Comp 90% CI |",
    "|---:|---|---|---|---:|---:|---:|---:|---|",
    sep = "\n"
  )
  rows <- paste0(
    "| ", seq_len(nrow(df)),
    " | ", df$PlayerName,
    " | ", ifelse(is.na(df$Team), "NA", df$Team),
    " | ", df$position,
    " | ", fmt(df$expected_rank_overall, 1),
    " | ", fmt(df$p_top20_overall, 3),
    " | ", fmt(df$p_top50_overall, 3),
    " | ", fmt(df$composite_p50, 3),
    " | [", fmt(df$composite_p05, 3), ", ", fmt(df$composite_p95, 3), "] |"
  )
  paste0(header, "\n", paste(rows, collapse = "\n"), "\n")
}

lines <- c(
  "# Top 20 hitter composite rankings by position (2026)",
  "",
  "## Method",
  "- Rank players within each posterior draw using draw-level composite z-scores.",
  "- Composite uses HR, R, RBI, SB totals (scaled by ATC PA), plus OBP and SLG.",
  paste0("- PA is treated as uncertain: Gamma(mean = PA_atc, CV = ", format(pa_cv, trim = TRUE), ")."),
  paste0("- Summaries below use ", n_draw, " posterior draws."),
  "",
  "## Top 20 overall",
  ""
)

top_overall <- summ %>%
  arrange(expected_rank_overall) %>%
  slice_head(n = 20)

lines <- c(
  lines,
  make_overall_table(top_overall),
  "",
  "## Top 20 by first-listed position",
  ""
)

for (p in pos_levels) {
  top_p <- summ %>%
    filter(pos1 == p) %>%
    arrange(expected_rank_pos) %>%
    slice_head(n = 20)
  lines <- c(lines, paste0("### ", p), "", make_table(top_p), "")
}

writeLines(lines, out_md)
cat("Wrote ", out_csv, "\n", sep = "")
cat("Wrote ", out_md, "\n", sep = "")
