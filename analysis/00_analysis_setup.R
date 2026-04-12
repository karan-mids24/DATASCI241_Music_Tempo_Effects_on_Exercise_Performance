library(data.table)
library(knitr)

get_analysis_dir <- function() {
  if (exists(".analysis_dir", envir = .GlobalEnv, inherits = FALSE)) {
    return(normalizePath(get(".analysis_dir", envir = .GlobalEnv), winslash = "/"))
  }

  for (i in rev(seq_along(sys.frames()))) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile) && nzchar(ofile)) {
      return(dirname(normalizePath(ofile, winslash = "/")))
    }
  }

  normalizePath(getwd(), winslash = "/")
}

ANALYSIS_DIR <- get_analysis_dir()
PROJECT_DIR <- dirname(ANALYSIS_DIR)
DERIVED_DIR <- file.path(ANALYSIS_DIR, "derived")

project_path <- function(...) {
  file.path(PROJECT_DIR, ...)
}

analysis_path <- function(...) {
  file.path(ANALYSIS_DIR, ...)
}

derived_path <- function(...) {
  file.path(DERIVED_DIR, ...)
}

sanitize_ascii <- function(x) {
  if (!is.character(x)) return(x)
  x <- gsub("\u2265", ">=", x, fixed = TRUE)
  x <- gsub("\u2264", "<=", x, fixed = TRUE)
  x <- gsub("\u2013", "-", x, fixed = TRUE)
  x <- gsub("\u2014", "-", x, fixed = TRUE)
  x
}

sanitize_dt_ascii <- function(dt) {
  for (j in seq_along(dt)) {
    if (is.character(dt[[j]])) {
      set(dt, j = j, value = sanitize_ascii(dt[[j]]))
    }
  }
  dt
}

coalesce_chr <- function(...) {
  vals <- list(...)
  n <- max(vapply(vals, length, integer(1)))
  out <- rep(NA_character_, n)
  for (v in vals) {
    vv <- as.character(v)
    take <- is.na(out) & !is.na(vv) & nzchar(trimws(vv))
    out[take] <- vv[take]
  }
  out
}

coalesce_num <- function(...) {
  vals <- list(...)
  n <- max(vapply(vals, length, integer(1)))
  out <- rep(NA_real_, n)
  for (v in vals) {
    vv <- suppressWarnings(as.numeric(v))
    take <- is.na(out) & !is.na(vv)
    out[take] <- vv[take]
  }
  out
}

read_experiment_wide <- function() {
  sanitize_dt_ascii(fread(project_path("w241_experiment_wide.csv")))
}

read_participants <- function() {
  dt <- sanitize_dt_ascii(fread(project_path("W241 Plank Study Data - participants.csv")))
  setnames(dt, "id", "participant_id", skip_absent = TRUE)
  if ("age" %in% names(dt)) dt[["age"]] <- suppressWarnings(as.numeric(dt[["age"]]))
  if ("gender" %in% names(dt)) {
    dt[["gender"]] <- trimws(tolower(as.character(dt[["gender"]])))
    dt[!nzchar(dt[["gender"]]), "gender"] <- NA_character_
  }
  for (nm in c("registered_at", "session1_planned_at", "session2_planned_at")) {
    if (nm %in% names(dt)) {
      dt[, (nm) := as.POSIXct(get(nm), tz = "UTC", format = "%Y-%m-%dT%H:%M:%OSZ")]
    }
  }
  drop_cols <- intersect(c("name", "email", "email_opt_in"), names(dt))
  if (length(drop_cols)) dt[, (drop_cols) := NULL]
  unique(dt, by = "participant_id")
}

condition_from_sessions <- function(audio_s1, audio_s2, value_s1, value_s2, target_condition) {
  out <- value_s1
  out[] <- NA
  idx1 <- !is.na(audio_s1) & audio_s1 == target_condition
  idx2 <- !is.na(audio_s2) & audio_s2 == target_condition
  out[idx1] <- value_s1[idx1]
  out[idx2] <- value_s2[idx2]
  out
}

write_derived_csv <- function(dt, filename) {
  if (!dir.exists(DERIVED_DIR)) dir.create(DERIVED_DIR, recursive = TRUE, showWarnings = FALSE)
  fwrite(dt, derived_path(filename))
}
