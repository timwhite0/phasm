suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

input_path <- "results/projections/pitchers/pitcher_composite_projections_2026.csv"
output_path <- "results/projections/pitchers/top50_pitcher_composite_by_role.md"

comp <- read_csv(input_path, show_col_types = FALSE) %>%
  mutate(role = ifelse(is.na(role) | role == "", "UNK", role))

fmt <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", x))

make_table <- function(df, include_role = FALSE) {
  df <- df %>%
    mutate(
      composite_z = fmt(composite),
      z_ERA = fmt(z_ERA),
      z_K9 = fmt(z_K9),
      z_WHIP = fmt(z_WHIP),
      z_IP = fmt(z_IP),
      z_WQS = fmt(z_WQS)
    )
  if (include_role) {
    header <- paste(
      "| Rank | Player | Role | Composite z | z_ERA | z_K9 | z_WHIP | z_IP | z_WQS |",
      "|---:|---|---|---:|---:|---:|---:|---:|---:|",
      sep = "\n"
    )
    rows <- paste0(
      "| ", seq_len(nrow(df)), " | ", df$PlayerName, " | ", df$role,
      " | ", df$composite_z, " | ", df$z_ERA, " | ", df$z_K9,
      " | ", df$z_WHIP, " | ", df$z_IP, " | ", df$z_WQS, " |"
    )
  } else {
    header <- paste(
      "| Rank | Player | Composite z | z_ERA | z_K9 | z_WHIP | z_IP | z_WQS |",
      "|---:|---|---:|---:|---:|---:|---:|---:|",
      sep = "\n"
    )
    rows <- paste0(
      "| ", seq_len(nrow(df)), " | ", df$PlayerName,
      " | ", df$composite_z, " | ", df$z_ERA, " | ", df$z_K9,
      " | ", df$z_WHIP, " | ", df$z_IP, " | ", df$z_WQS, " |"
    )
  }
  paste0(header, "\n", paste(rows, collapse = "\n"), "\n")
}

roles <- sort(unique(comp$role))

lines <- c(
  "# Top 50 Pitcher Composite Z-Scores by Role (2026)",
  "",
  "## Methodology",
  "- Composite scores come from `results/projections/pitchers/pitcher_composite_projections_2026.csv`.",
  "- The composite is an equal-weight average of z-scores for: ERA (sign flipped), K/9, WHIP (sign flipped), W+QS, and ATC-projected IP.",
  "- Z-scores are computed from **posterior means** of each metric.",
  "",
  "## Top 50 by Role",
  ""
)

for (role_name in roles) {
  df_role <- comp %>%
    filter(role == role_name) %>%
    arrange(desc(composite)) %>%
    slice_head(n = 50)
  lines <- c(lines, paste0("### ", role_name), "", make_table(df_role, include_role = FALSE), "")
}

writeLines(lines, output_path)
cat(sprintf("Wrote %s\n", output_path))
