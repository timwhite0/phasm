library(dplyr)
library(readr)
suppressPackageStartupMessages({
  library(ggplot2)
  library(forcats)
  library(stringr)
})

proj_path <- "results/projections/category_projections_2026.csv"
pa_path <- "data/atc_pa_projections_2026.csv"
results_dir <- "results/plots/position_intervals"

if (!dir.exists("results")) dir.create("results")
if (!dir.exists(results_dir)) dir.create(results_dir)

proj <- read_csv(proj_path, show_col_types = FALSE) %>%
  mutate(playerid = as.character(playerid))

pa <- read_csv(pa_path, show_col_types = FALSE)

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}

id_col <- pick_col(pa, c("playerid", "PlayerId", "player_id"))
pa_col <- pick_col(pa, c("PA", "pa"))
if (is.null(id_col) || is.null(pa_col)) {
  stop("Could not find playerid or PA column in ATC projections.")
}

# Normalize PA columns
pa <- pa %>%
  mutate(
    playerid = as.character(.data[[id_col]]),
    PA = as.numeric(.data[[pa_col]])
  ) %>%
  select(playerid, PA) %>%
  filter(!is.na(playerid), !is.na(PA))

proj <- proj %>%
  left_join(pa, by = "playerid")

count_cats <- c("H", "R", "RBI", "SB", "HR")
rate_cats <- c("AVG", "OBP", "SLG")

build_category_df <- function(cat) {
  mean_col <- paste0(cat, "_mean")
  p10_col <- paste0(cat, "_p10")
  p90_col <- paste0(cat, "_p90")

  if (!all(c(mean_col, p10_col, p90_col) %in% names(proj))) {
    return(NULL)
  }

  out <- proj %>%
    select(playerid, PlayerName, position, PA, all_of(mean_col), all_of(p10_col), all_of(p90_col)) %>%
    rename(mean = all_of(mean_col), p10 = all_of(p10_col), p90 = all_of(p90_col)) %>%
    mutate(category = cat)

  if (cat %in% count_cats) {
    out <- out %>%
      mutate(
        mean = mean * PA,
        p10 = p10 * PA,
        p90 = p90 * PA
      )
  }

  out
}

cat_frames <- lapply(c(count_cats, rate_cats), build_category_df)
cat_df <- bind_rows(cat_frames) %>%
  filter(!is.na(position), !is.na(mean), !is.na(p10), !is.na(p90)) %>%
  mutate(
    position = str_trim(str_replace(position, "/.*", "")),
    position = as.character(position)
  )

if (nrow(cat_df) == 0) {
  stop("No projection data available after processing.")
}

plot_position <- function(pos) {
  pos_df <- cat_df %>%
    filter(position == pos) %>%
    group_by(category) %>%
    arrange(desc(mean), .by_group = TRUE) %>%
    slice(1:30) %>%
    ungroup()

  if (nrow(pos_df) == 0) return(NULL)

  pos_df <- pos_df %>%
    group_by(category) %>%
    mutate(
      PlayerName_plot = paste(PlayerName, category, sep = "___"),
      PlayerName_plot = fct_reorder(PlayerName_plot, mean)
    ) %>%
    ungroup()

  p <- ggplot(pos_df, aes(x = PlayerName_plot, y = mean)) +
    geom_linerange(aes(ymin = p10, ymax = p90), color = "gray45", linewidth = 0.6) +
    geom_point(color = "dodgerblue", size = 1.6) +
    coord_flip() +
    facet_wrap(~ category, scales = "free") +
    scale_x_discrete(labels = function(x) str_replace(x, "___.*", ""), drop = TRUE) +
    theme_minimal(base_size = 11) +
    labs(
      title = paste0("2026 projections by category: ", pos),
      x = NULL,
      y = NULL
    ) +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  # Add per-facet y labels using a small annotation in the strip area via subtitle
  p
}

positions <- sort(unique(cat_df$position))

for (pos in positions) {
  p <- plot_position(pos)
  if (is.null(p)) next
  file_pos <- str_replace_all(pos, "[^A-Za-z0-9]+", "_")
  out_path <- file.path(results_dir, paste0("projection_intervals_2026_", file_pos, ".pdf"))
  ggsave(out_path, plot = p, width = 12, height = 14)
}

cat("Wrote position plots to", results_dir, "\n")
