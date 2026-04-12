# process_plank_sessions_experimental.R
# Same pipeline as process_plank_sessions_experimental.Rmd (narrated; knit/render that file for full documentation).
#
# Design columns (not all appear in codebook; see CSV headers):
#   completed_both: 1 = sessions 1 and 2 both present after dedup; 0 = only one session
#   crossover_order: high_tempo_first | low_tempo_first | NA (NA if session 1 missing)
#   first_session_is_high_tempo: 1/0/NA
#   audio_s1, audio_s2: high_tempo | low_tempo | NA
#   plank_* , diff_high_minus_low, plank_high_tempo, plank_low_tempo: seconds
#   elapsed_sessions_hours: (submitted_at_s2 - submitted_at_s1) in hours, UTC; NA if not computable
# Timestamps are not written to the wide CSV (only elapsed is retained).
#
# - No email / contact_sheet_url in outputs
# - Raw trance/pop -> high_tempo/low_tempo
# - Survey JSON: CSV doubled quotes fixed before jsonlite::fromJSON
# - Integers: yes/no -> 0/1; volume_clear adds somewhat=2; Likert as integers; free text -> *_present 0/1

# Paths relative to this script (not the shell cwd).
args <- commandArgs(trailingOnly = FALSE)
f <- args[grepl("^--file=", args)]
if (length(f)) {
  setwd(dirname(normalizePath(sub("^--file=", "", f[1L]), winslash = "/")))
}

library(data.table)
library(jsonlite)

SRC <- "W241 Plank Study Data - sessions.csv"
OUT_WIDE <- "w241_experiment_wide.csv"
OUT_CODEBOOK <- "w241_experiment_codebook.csv"

AUDIO_MAP <- c(trance = "high_tempo", pop = "low_tempo")

# Fields encoded as yes=1, no=0 (case-insensitive; true/false)
YN_FIELDS <- c("headphones", "plank_pause")

# volume_clear: yes=1, no=0, somewhat=2
VOLUME_FIELD <- "volume_clear"

# Free text -> 0/1 presence columns named pre_/post_ + field + _present + _sN
TEXT_FIELDS <- c("comments", "plank_pause_detail")

# Known numeric (integer) Likert / count fields in JSON
NUMERIC_FIELDS <- c(
  "energy_pre_plank", "motivation_pre_plank", "music_liking",
  "rpe", "music_effect", "music_effect_overall",
  "instructions_ease", "overall_experience"
)

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------
parse_obj <- function(txt) {
  if (is.na(txt) || !nzchar(trimws(as.character(txt)))) return(NULL)
  # Google Sheets / CSV exports often double quotes inside JSON strings
  txt <- gsub('""', '"', txt, fixed = TRUE)
  out <- tryCatch(
    fromJSON(txt, simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(out) || length(out) == 0L) return(NULL)
  if (!is.list(out)) return(NULL)
  out
}

norm_yn <- function(x) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) return(NA_integer_)
  v <- tolower(trimws(as.character(x[[1L]])))
  if (!nzchar(v)) return(NA_integer_)
  if (v %in% c("yes", "y", "true", "1")) return(1L)
  if (v %in% c("no", "n", "false", "0")) return(0L)
  NA_integer_
}

norm_volume <- function(x) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) return(NA_integer_)
  v <- tolower(trimws(as.character(x[[1L]])))
  if (!nzchar(v)) return(NA_integer_)
  if (v == "yes") return(1L)
  if (v == "no") return(0L)
  if (v == "somewhat") return(2L)
  NA_integer_
}

as_int_num <- function(x) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) return(NA_integer_)
  z <- suppressWarnings(as.integer(round(as.numeric(x[[1L]]))))
  if (length(z) != 1L || is.na(z)) NA_integer_ else z
}

text_present <- function(x) {
  if (is.null(x)) return(NA_integer_)
  s <- trimws(as.character(x))
  if (length(s) != 1L) return(NA_integer_)
  if (!nzchar(s) || s == "NA") return(0L)
  1L
}

# Global catalogs for string categoricals (excluding YN/volume handled above)
catalog <- new.env(parent = emptyenv())
codebook_rows <- list()

add_codebook <- function(variable, code, label) {
  codebook_rows[[length(codebook_rows) + 1L]] <<- data.table(
    column_name = variable,
    code_value = code,
    value_label = label
  )
}

encode_scalar <- function(key, val, colname) {
  if (key %in% TEXT_FIELDS) {
    p <- text_present(val)
    return(list(col = paste0(colname, "_present"), val = p))
  }
  if (key %in% YN_FIELDS) {
    return(list(col = colname, val = norm_yn(val)))
  }
  if (identical(key, VOLUME_FIELD)) {
    return(list(col = colname, val = norm_volume(val)))
  }
  if (key %in% NUMERIC_FIELDS) {
    return(list(col = colname, val = as_int_num(val)))
  }
  # Categorical string: maintain global catalog per base key
  if (is.null(val) || (length(val) == 1L && is.na(val))) {
    return(list(col = colname, val = NA_integer_))
  }
  lab <- trimws(as.character(val[[1L]]))
  if (!nzchar(lab)) return(list(col = colname, val = NA_integer_))

  ck <- paste0("cat__", key)
  if (!exists(ck, envir = catalog, inherits = FALSE)) {
    assign(ck, character(), envir = catalog)
  }
  lev <- get(ck, envir = catalog)
  if (!lab %in% lev) {
    lev <- sort(unique(c(lev, lab)))
    assign(ck, lev, envir = catalog)
  }
  lev <- get(ck, envir = catalog)
  code <- match(lab, lev)
  list(col = colname, val = as.integer(code))
}

flatten_json_block <- function(obj, prefix, session_num) {
  if (is.null(obj)) return(list())
  sfx <- paste0("_s", session_num)
  out <- list()
  for (key in names(obj)) {
    val <- obj[[key]]
    base_col <- paste0(prefix, "_", key, sfx)
    if (key %in% TEXT_FIELDS) {
      enc <- encode_scalar(key, val, paste0(prefix, "_", key))
      nm <- paste0(enc$col, sfx)
      out[[nm]] <- enc$val
      next
    }
    enc <- encode_scalar(key, val, base_col)
    out[[enc$col]] <- enc$val
  }
  out
}

# First pass: build catalogs from all rows (string fields)
build_catalogs_pass <- function(dt) {
  for (i in seq_len(nrow(dt))) {
    sn <- dt$session_num[i]
    pre <- parse_obj(dt$pre_task_answers[i])
    post <- parse_obj(dt$post_task_answers[i])
    for (obj in list(pre, post)) {
      if (is.null(obj)) next
      for (key in names(obj)) {
        if (key %in% c(YN_FIELDS, VOLUME_FIELD, NUMERIC_FIELDS, TEXT_FIELDS)) next
        val <- obj[[key]]
        if (is.null(val)) next
        lab <- trimws(as.character(val[[1L]]))
        if (!nzchar(lab) || is.na(lab)) next
        ck <- paste0("cat__", key)
        if (!exists(ck, envir = catalog, inherits = FALSE)) assign(ck, character(), envir = catalog)
        cur <- get(ck, envir = catalog)
        if (!lab %in% cur) assign(ck, sort(unique(c(cur, lab))), envir = catalog)
      }
    }
  }
  # Register codebook entries for categorical keys (column names include session in second pass)
  # We add per-column codebook in second pass when column name is known
}

register_cat_codebooks_for_session <- function(keys, prefix, session_num) {
  sfx <- paste0("_s", session_num)
  for (key in keys) {
    if (key %in% c(YN_FIELDS, VOLUME_FIELD, NUMERIC_FIELDS, TEXT_FIELDS)) next
    ck <- paste0("cat__", key)
    if (!exists(ck, envir = catalog, inherits = FALSE)) next
    lev <- get(ck, envir = catalog)
    colnm <- paste0(prefix, "_", key, sfx)
    for (i in seq_along(lev)) {
      add_codebook(colnm, i, lev[i])
    }
  }
}

# ------------------------------------------------------------------------------
# Load and clean long
# ------------------------------------------------------------------------------
dt <- fread(SRC)
stopifnot(all(c(
  "participant_id", "session_num", "audio_track",
  "plank_duration_sec", "pre_task_answers", "post_task_answers", "submitted_at"
) %in% names(dt)))

if ("email" %in% names(dt)) dt[, email := NULL]
if ("contact_sheet_url" %in% names(dt)) dt[, contact_sheet_url := NULL]

dt[, audio_track := trimws(tolower(as.character(audio_track)))]
bad_audio <- unique(dt[!audio_track %in% names(AUDIO_MAP), audio_track])
if (length(bad_audio)) {
  stop("Unknown audio_track value(s): ", paste(bad_audio, collapse = ", "))
}
dt[, audio_track := unname(AUDIO_MAP[audio_track])]

dt[, submitted_at := as.POSIXct(submitted_at, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OSZ")]
if (all(is.na(dt$submitted_at))) {
  dt[, submitted_at := as.POSIXct(submitted_at, tz = "UTC")]
}

setorder(dt, participant_id, session_num, submitted_at)
dt <- dt[, .SD[.N], by = .(participant_id, session_num)]

# First pass catalogs
build_catalogs_pass(dt)

# ------------------------------------------------------------------------------
# Build per-session tables (outcomes + survey)
# ------------------------------------------------------------------------------
process_session <- function(dt, sn) {
  d <- dt[session_num == sn]
  if (nrow(d) == 0L) {
    return(data.table(participant_id = character()))
  }

  keys_pre <- unique(unlist(lapply(seq_len(nrow(d)), function(i) {
    o <- parse_obj(d$pre_task_answers[i])
    if (is.null(o)) character() else names(o)
  })))
  keys_post <- unique(unlist(lapply(seq_len(nrow(d)), function(i) {
    o <- parse_obj(d$post_task_answers[i])
    if (is.null(o)) character() else names(o)
  })))
  register_cat_codebooks_for_session(keys_pre, "pre", sn)
  register_cat_codebooks_for_session(keys_post, "post", sn)

  # YN / volume / numeric codebook notes
  for (fld in YN_FIELDS) {
    add_codebook(paste0("post_", fld, "_s", sn), 0L, "no")
    add_codebook(paste0("post_", fld, "_s", sn), 1L, "yes")
    add_codebook(paste0("pre_", fld, "_s", sn), 0L, "no")
    add_codebook(paste0("pre_", fld, "_s", sn), 1L, "yes")
  }
  add_codebook(paste0("post_", VOLUME_FIELD, "_s", sn), 0L, "no")
  add_codebook(paste0("post_", VOLUME_FIELD, "_s", sn), 1L, "yes")
  add_codebook(paste0("post_", VOLUME_FIELD, "_s", sn), 2L, "somewhat")
  add_codebook(paste0("pre_", VOLUME_FIELD, "_s", sn), 0L, "no")
  add_codebook(paste0("pre_", VOLUME_FIELD, "_s", sn), 1L, "yes")
  add_codebook(paste0("pre_", VOLUME_FIELD, "_s", sn), 2L, "somewhat")

  for (fld in TEXT_FIELDS) {
    add_codebook(paste0("pre_", fld, "_present_s", sn), 0L, "empty or missing")
    add_codebook(paste0("pre_", fld, "_present_s", sn), 1L, "non-empty text (not exported)")
    add_codebook(paste0("post_", fld, "_present_s", sn), 0L, "empty or missing")
    add_codebook(paste0("post_", fld, "_present_s", sn), 1L, "non-empty text (not exported)")
  }

  rows <- vector("list", nrow(d))
  for (i in seq_len(nrow(d))) {
    pre <- parse_obj(d$pre_task_answers[i])
    post <- parse_obj(d$post_task_answers[i])
    L <- c(
      flatten_json_block(pre, "pre", sn),
      flatten_json_block(post, "post", sn)
    )
    rows[[i]] <- c(
      list(
        participant_id = d$participant_id[i],
        plank_duration_sec = d$plank_duration_sec[i],
        audio_track = d$audio_track[i],
        submitted_at = d$submitted_at[i]
      ),
      L
    )
  }
  rbindlist(lapply(rows, function(r) as.data.table(r)), fill = TRUE)
}

s1 <- process_session(dt, 1L)
s2 <- process_session(dt, 2L)

setnames(s1, c("plank_duration_sec", "audio_track", "submitted_at"),
         c("plank_s1", "audio_s1", "submitted_at_s1"), skip_absent = TRUE)
setnames(s2, c("plank_duration_sec", "audio_track", "submitted_at"),
         c("plank_s2", "audio_s2", "submitted_at_s2"), skip_absent = TRUE)

all_pid <- unique(c(s1$participant_id, s2$participant_id))
wide <- data.table(participant_id = all_pid)
wide <- merge(wide, s1, by = "participant_id", all.x = TRUE)
wide <- merge(wide, s2, by = "participant_id", all.x = TRUE)

# completed_both
wide[, completed_both := as.integer(!is.na(plank_s1) & !is.na(plank_s2))]

# crossover / design
wide[, crossover_order := NA_character_]
wide[audio_s1 == "high_tempo", crossover_order := "high_tempo_first"]
wide[audio_s1 == "low_tempo", crossover_order := "low_tempo_first"]

wide[, first_session_is_high_tempo := NA_integer_]
wide[audio_s1 == "high_tempo", first_session_is_high_tempo := 1L]
wide[audio_s1 == "low_tempo", first_session_is_high_tempo := 0L]

wide[, plank_high_tempo := NA_real_]
wide[, plank_low_tempo := NA_real_]
wide[audio_s1 == "high_tempo", plank_high_tempo := plank_s1]
wide[audio_s1 == "low_tempo", plank_low_tempo := plank_s1]
wide[audio_s2 == "high_tempo", plank_high_tempo := plank_s2]
wide[audio_s2 == "low_tempo", plank_low_tempo := plank_s2]

wide[, diff_high_minus_low := plank_high_tempo - plank_low_tempo]

wide[, elapsed_sessions_hours := NA_real_]
ok_time <- !is.na(wide$submitted_at_s1) & !is.na(wide$submitted_at_s2) & (wide$completed_both == 1L)
wide[ok_time, elapsed_sessions_hours := as.numeric(difftime(submitted_at_s2, submitted_at_s1, units = "hours"))]

# Drop timestamps from exported wide (only elapsed retained)
wide[, c("submitted_at_s1", "submitted_at_s2") := NULL]

# Column order: IDs and design first
core <- c(
  "participant_id", "completed_both", "crossover_order", "first_session_is_high_tempo",
  "audio_s1", "audio_s2", "plank_s1", "plank_s2",
  "plank_high_tempo", "plank_low_tempo", "diff_high_minus_low",
  "elapsed_sessions_hours"
)
rest <- setdiff(names(wide), core)
setcolorder(wide, c(core, sort(rest)))

fwrite(wide, OUT_WIDE)

cb <- rbindlist(codebook_rows, fill = TRUE)
if (nrow(cb)) {
  cb <- unique(cb, by = c("column_name", "code_value"))
  cb <- cb[column_name %in% names(wide)]
  setorder(cb, column_name, code_value)
}
fwrite(cb, OUT_CODEBOOK)

message("Wrote ", nrow(wide), " rows to ", OUT_WIDE)
message("Wrote ", nrow(cb), " codebook rows to ", OUT_CODEBOOK)
