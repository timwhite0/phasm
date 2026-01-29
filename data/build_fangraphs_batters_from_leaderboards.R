suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

standard_path <- "data/fangraphs_batters_2021_2025_standard.csv"
advanced_path <- "data/fangraphs_batters_2021_2025_advanced.csv"
existing_path <- "data/fangraphs_batters_2021_2025.csv"
base_dir <- dirname(normalizePath(standard_path, mustWork = TRUE))
output_path <- file.path(base_dir, "fangraphs_batters_2021_2025_new.csv")

if (!file.exists(standard_path)) stop(sprintf("Missing standard file: %s", standard_path))
if (!file.exists(advanced_path)) stop(sprintf("Missing advanced file: %s", advanced_path))

std <- read_csv(standard_path, show_col_types = FALSE)
adv <- read_csv(advanced_path, show_col_types = FALSE)

# Ensure Season column exists
if (!"Season" %in% names(std) || !"Season" %in% names(adv)) {
  stop("Both standard and advanced files must have a Season column.")
}

# Use PlayerId + Season as primary key when available
key_cols <- c("Season", "PlayerId")
if (!all(key_cols %in% names(std)) || !all(key_cols %in% names(adv))) {
  stop("Expected Season and PlayerId columns in both files for merging.")
}

# Check duplicate keys
dup_std <- std %>% count(across(all_of(key_cols))) %>% filter(n > 1)
dup_adv <- adv %>% count(across(all_of(key_cols))) %>% filter(n > 1)
if (nrow(dup_std) > 0) message(sprintf("Standard has %d duplicate Season+PlayerId rows.", nrow(dup_std)))
if (nrow(dup_adv) > 0) message(sprintf("Advanced has %d duplicate Season+PlayerId rows.", nrow(dup_adv)))

# Merge; keep all rows from standard, add advanced columns
merged <- std %>%
  left_join(adv, by = key_cols, suffix = c("", "_adv"))

# Filter seasons and PA
merged <- merged %>% filter(Season >= 2021, Season <= 2025)
if ("PA" %in% names(merged)) {
  merged <- merged %>% filter(PA >= 100)
}

# Keep players with >=100 PA in either 2024 or 2025 (season-level PA)
if ("PA" %in% names(merged)) {
  keep_recent <- merged %>%
    group_by(PlayerId) %>%
    summarize(
      PA_2425_max = ifelse(
        any(Season %in% c(2024, 2025)),
        max(PA[Season %in% c(2024, 2025)], na.rm = TRUE),
        0
      ),
      .groups = "drop"
    ) %>%
    filter(PA_2425_max >= 100) %>%
    pull(PlayerId)
  merged <- merged %>% filter(PlayerId %in% keep_recent)
}

# Drop covariates with substantial NA proportions, then drop any player with NA in remaining covariates
na_threshold <- 0.20
id_cols <- c(
  "playerid", "playerid2", "PlayerId", "MLBAMID",
  "Name", "name", "Player", "Season", "year",
  "Team", "team", "Tm", "tm", "Pos", "Position", "position", "ID"
)

covar_cols <- names(merged)[
  !names(merged) %in% id_cols &
    vapply(merged, function(x) is.numeric(x) || is.integer(x), logical(1))
]

if (length(covar_cols) > 0) {
  na_props <- vapply(merged[covar_cols], function(x) mean(is.na(x)), numeric(1))
  drop_covars <- names(na_props)[na_props > na_threshold]
  if (length(drop_covars) > 0) {
    message(sprintf(
      "Dropping %d covariates with >%.0f%% NA: %s",
      length(drop_covars),
      na_threshold * 100,
      paste(drop_covars, collapse = ", ")
    ))
    merged <- merged %>% select(-all_of(drop_covars))
  }
  covar_cols_kept <- setdiff(covar_cols, drop_covars)
  if (length(covar_cols_kept) > 0) {
    bad_ids <- merged %>%
      group_by(PlayerId) %>%
      summarize(any_na = any(if_any(all_of(covar_cols_kept), is.na)), .groups = "drop") %>%
      filter(any_na) %>%
      pull(PlayerId)
    before_n <- nrow(merged)
    merged <- merged %>% filter(!(PlayerId %in% bad_ids))
    message(sprintf("Dropped %d rows from players with any NA in remaining covariates", before_n - nrow(merged)))
  }
}

# Report alignment vs existing file (if present)
if (file.exists(existing_path)) {
  existing <- read_csv(existing_path, show_col_types = FALSE)
  existing_keys <- intersect(key_cols, names(existing))
  if (length(existing_keys) == length(key_cols)) {
    new_keys <- merged %>% distinct(across(all_of(key_cols)))
    old_keys <- existing %>% distinct(across(all_of(key_cols)))
    missing_in_new <- anti_join(old_keys, new_keys, by = key_cols)
    extra_in_new <- anti_join(new_keys, old_keys, by = key_cols)
    message(sprintf("Alignment vs existing: %d missing, %d extra rows by Season+PlayerId.",
                    nrow(missing_in_new), nrow(extra_in_new)))
  } else {
    alt_keys <- c("Season", "Name", "Team")
    if (all(alt_keys %in% names(existing)) && all(alt_keys %in% names(merged))) {
      new_keys <- merged %>% distinct(across(all_of(alt_keys)))
      old_keys <- existing %>% distinct(across(all_of(alt_keys)))
      missing_in_new <- anti_join(old_keys, new_keys, by = alt_keys)
      extra_in_new <- anti_join(new_keys, old_keys, by = alt_keys)
      message(sprintf("Alignment vs existing: %d missing, %d extra rows by Season+Name+Team.",
                      nrow(missing_in_new), nrow(extra_in_new)))
    } else {
      message("Existing file lacks Season+PlayerId keys; cannot compare alignment by key.")
    }
  }
}

# Write new output without overwriting existing file
write_csv(merged, output_path)
message(sprintf("Wrote %s (%d rows, %d cols)", output_path, nrow(merged), ncol(merged)))
