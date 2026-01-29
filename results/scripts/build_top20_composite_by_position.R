suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

input_path <- "results/projections/composite_projections_2026.csv"
output_path <- "results/projections/top20_composite_by_position.md"

comp <- read_csv(input_path, show_col_types = FALSE) %>%
  mutate(
    position = ifelse(is.na(position) | position == "", "UNK", position),
    position_primary = str_split(position, "/", simplify = TRUE)[, 1]
  )

fmt <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", x))

make_table <- function(df, include_position = FALSE) {
  df <- df %>%
    mutate(
      composite_z = fmt(composite_zscore),
      z_HR = fmt(z_HR),
      z_R = fmt(z_R),
      z_RBI = fmt(z_RBI),
      z_SB = fmt(z_SB),
      z_OBP = fmt(z_OBP),
      z_SLG = fmt(z_SLG)
    )
  if (include_position) {
    header <- paste(
      "| Rank | Player | Position | Composite z | z_HR | z_R | z_RBI | z_SB | z_OBP | z_SLG |",
      "|---:|---|---|---:|---:|---:|---:|---:|---:|---:|",
      sep = "\n"
    )
    rows <- paste0(
      "| ", seq_len(nrow(df)), " | ", df$PlayerName, " | ", df$position_primary,
      " | ", df$composite_z, " | ", df$z_HR, " | ", df$z_R, " | ", df$z_RBI,
      " | ", df$z_SB, " | ", df$z_OBP, " | ", df$z_SLG, " |"
    )
  } else {
    header <- paste(
      "| Rank | Player | Composite z | z_HR | z_R | z_RBI | z_SB | z_OBP | z_SLG |",
      "|---:|---|---:|---:|---:|---:|---:|---:|---:|",
      sep = "\n"
    )
    rows <- paste0(
      "| ", seq_len(nrow(df)), " | ", df$PlayerName,
      " | ", df$composite_z, " | ", df$z_HR, " | ", df$z_R, " | ", df$z_RBI,
      " | ", df$z_SB, " | ", df$z_OBP, " | ", df$z_SLG, " |"
    )
  }
  paste0(header, "\n", paste(rows, collapse = "\n"), "\n")
}

overall <- comp %>%
  arrange(desc(composite_zscore)) %>%
  slice_head(n = 20)

positions <- c("C", "1B", "2B", "3B", "SS", "OF", "DH")

lines <- c(
  "# Top 20 Composite Z-Scores by Position (2026)",
  "",
  "## Methodology",
  "- Composite scores come from `results/projections/composite_projections_2026.csv`.",
  "- The composite is an equal-weight average of z-scores for: HR, R, RBI, SB totals (using ATC PA), plus OBP and SLG rates.",
  "- Z-scores are computed from **posterior means** of each category.",
  "- Positions use the primary position string; for multi-position players, the first position is used.",
  "",
  "## Top 20 Overall",
  "",
  make_table(overall, include_position = TRUE),
  "## Top 20 by Position",
  ""
)

for (pos in positions) {
  df_pos <- comp %>%
    filter(position_primary == pos) %>%
    arrange(desc(composite_zscore)) %>%
    slice_head(n = 20)
  lines <- c(lines, paste0("### ", pos), "", make_table(df_pos, include_position = FALSE), "")
}

writeLines(lines, output_path)
cat(sprintf("Wrote %s\n", output_path))
