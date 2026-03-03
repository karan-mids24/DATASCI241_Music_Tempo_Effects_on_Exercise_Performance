# process_pilot_csv.R
# Unifies registration + Session 1 + Session 2 CSVs by email, deduplicates,
# and outputs one row per participant with plank times by condition.
# Outcome: "How long were you able to hold your plank (in seconds)"
# Design: Experiment_ID 1 = Control (Session 1 = slow, Session 2 = motivational);
#         Experiment_ID 2 = Treatment (Session 1 = motivational, Session 2 = slow).

library(data.table)

# ------------------------------------------------------------------------------
# Paths (run from project root, e.g. w241/)
# ------------------------------------------------------------------------------
reg_path <- "Physical Performance Study Registration Form (Responses) - Form Responses 1.csv"
s1_path  <- "Physical Performance Research Study Session 1 (Responses) - Form Responses 1.csv"
s2_path  <- "Physical Performance Research Study Session 2 (Responses) - Form Responses 1.csv"
out_path <- "pilot_data.csv"

# ------------------------------------------------------------------------------
# Parse plank time: numeric seconds or "M:SS" (e.g. "1:50" -> 110)
# ------------------------------------------------------------------------------
parse_plank_seconds <- function(x) {
  x <- as.character(trimws(x))
  out <- rep(NA_real_, length(x))
  # Numeric
  num <- suppressWarnings(as.numeric(x))
  ok <- !is.na(num)
  out[ok] <- num[ok]
  # M:SS
  mm_ss <- grep("^[0-9]+:[0-9]+$", x, value = FALSE)
  if (length(mm_ss)) {
    parts <- strsplit(x[mm_ss], ":", fixed = TRUE)
    out[mm_ss] <- vapply(parts, function(p) as.numeric(p[1]) * 60 + as.numeric(p[2]), 0)
  }
  out
}

# ------------------------------------------------------------------------------
# Read and standardize email column
# ------------------------------------------------------------------------------
reg <- fread(reg_path)
s1  <- fread(s1_path)
s2  <- fread(s2_path)

setnames(reg, "Email Address", "email")
setnames(s1,  "Email Address", "email")
setnames(s2,  "Email Address", "email")

# ------------------------------------------------------------------------------
# Deduplicate by email (keep latest by Timestamp)
# ------------------------------------------------------------------------------
reg <- reg[order(Timestamp)][!duplicated(email, fromLast = TRUE)]
s1  <- s1[order(Timestamp)][!duplicated(email, fromLast = TRUE)]
s2  <- s2[order(Timestamp)][!duplicated(email, fromLast = TRUE)]

# ------------------------------------------------------------------------------
# Plank column name (same in Session 1 and Session 2)
# ------------------------------------------------------------------------------
plank_col <- grep("How long were you able to hold your plank", names(s1), value = TRUE)
stopifnot(length(plank_col) == 1L)

# ------------------------------------------------------------------------------
# Build cleaned tables:
# - Keep ALL acquired fields, prefixing by source (reg_, s1_, s2_)
# - Create numeric plank times and experiment_id
# ------------------------------------------------------------------------------

# Registration (baseline + assignment)
reg_clean <- copy(reg)
reg_non_email <- setdiff(names(reg_clean), "email")
setnames(reg_clean, reg_non_email, paste0("reg_", make.names(reg_non_email)))
reg_clean[, experiment_id := as.integer(reg_Experiment_ID)]

# Session 1
s1_clean <- copy(s1)
s1_non_email <- setdiff(names(s1_clean), "email")
setnames(s1_clean, s1_non_email, paste0("s1_", make.names(s1_non_email)))
plank_col_s1 <- paste0("s1_", make.names(plank_col))
s1_clean[, tte_session1 := parse_plank_seconds(get(plank_col_s1))]

# Session 2
s2_clean <- copy(s2)
s2_non_email <- setdiff(names(s2_clean), "email")
setnames(s2_clean, s2_non_email, paste0("s2_", make.names(s2_non_email)))
plank_col_s2 <- paste0("s2_", make.names(plank_col))
s2_clean[, tte_session2 := parse_plank_seconds(get(plank_col_s2))]

# ------------------------------------------------------------------------------
# Merge: one row per email
# ------------------------------------------------------------------------------
pilot <- merge(reg_clean, s1_clean, by = "email", all.x = TRUE)
pilot <- merge(pilot,  s2_clean, by = "email", all.x = TRUE)

# ------------------------------------------------------------------------------
# Condition-specific plank time (slow vs high-tempo)
# Control (1): Session 1 = slow, Session 2 = high
# Treatment (2): Session 1 = high, Session 2 = slow
# ------------------------------------------------------------------------------
pilot[, tte_slow := fifelse(experiment_id == 1, tte_session1, tte_session2)]
pilot[, tte_high := fifelse(experiment_id == 1, tte_session2, tte_session1)]

# Drop rows missing both plank outcomes (incomplete)
pilot <- pilot[!(is.na(tte_slow) & is.na(tte_high))]

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
fwrite(pilot, out_path)
message("Wrote ", nrow(pilot), " rows to ", out_path)
