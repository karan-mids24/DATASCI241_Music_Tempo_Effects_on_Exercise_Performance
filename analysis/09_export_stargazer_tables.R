library(data.table)
library(sandwich)
library(lmtest)
library(stargazer)

`%||%` <- function(x, y) if (is.null(x) || !nzchar(x)) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)][1]
script_path <- sub("^--file=", "", file_arg %||% "")
analysis_dir <- normalizePath(dirname(script_path %||% file.path(getwd(), "analysis")), winslash = "/")
derived_dir <- file.path(analysis_dir, "derived")
output_dir <- file.path(analysis_dir, "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

paired <- fread(file.path(derived_dir, "paired_analysis.csv"))
long_dt <- fread(file.path(derived_dir, "long_repeated_measures.csv"))

long_primary <- copy(long_dt[in_primary_analysis == 1L])
long_primary[, participant_id := factor(participant_id)]
long_primary[, period_factor := factor(period)]

bench <- copy(long_dt[in_first_period_benchmark == 1L & period == 1L])
bench[, high_tempo_first := high_tempo]
bench[, male_participant := as.integer(participant_gender == "male")]

# Model order required by the user:
# 1. Raw paired contrast model for the primary crossover sample
# 2. Fixed-effects crossover regression with high-tempo + Period 2
# 3. First-period benchmark raw contrast
# 4. First-period benchmark precision-adjusted regression
m1 <- lm(outcome_tte ~ high_tempo + participant_id, data = long_primary)
m2 <- lm(outcome_tte ~ high_tempo + period_factor + participant_id, data = long_primary)
m3 <- lm(outcome_tte ~ high_tempo_first, data = bench)
m4 <- lm(outcome_tte ~ high_tempo_first + participant_age + male_participant, data = bench)

models <- list(m1, m2, m3, m4)
model_classes <- vapply(models, function(x) class(x)[1], character(1))
cat("Model classes:\n")
print(model_classes)
stopifnot(all(model_classes == "lm"))

summary_vectors <- function(model) {
  sm <- summary(model)$coefficients
  list(
    se = sm[, "Std. Error"],
    p = sm[, "Pr(>|t|)"]
  )
}

sm1 <- summary_vectors(m1)
sm3 <- summary_vectors(m3)
sm4 <- summary_vectors(m4)

fe_vcov <- sandwich::vcovCL(m2, cluster = ~ participant_id, type = "HC1")
fe_ct <- lmtest::coeftest(m2, vcov. = fe_vcov)
fe_se <- fe_ct[, "Std. Error"]
fe_p <- fe_ct[, "Pr(>|t|)"]

table_notes <- c(
  "Primary analysis uses the randomized crossover design.",
  "Standard errors for the fixed-effects model are clustered at the participant level.",
  "The benchmark uses Session 1 only to emulate a parallel-arm design."
)

full_html <- file.path(output_dir, "stargazer_main_models.html")
full_txt <- file.path(output_dir, "stargazer_main_models.txt")
compact_html <- file.path(output_dir, "stargazer_primary_models_compact.html")

stargazer::stargazer(
  m1, m2, m3, m4,
  type = "html",
  out = full_html,
  title = "Main Regression Models for the Plank Experiment",
  dep.var.caption = "",
  dep.var.labels = "Plank time to exhaustion (seconds)",
  column.labels = c(
    "Paired contrast",
    "FE crossover",
    "Benchmark raw",
    "Benchmark adjusted"
  ),
  model.numbers = FALSE,
  omit = c("participant_id", "Constant"),
  keep = c("high_tempo", "period_factor2", "high_tempo_first", "participant_age", "male_participant"),
  covariate.labels = c(
    "High-tempo indicator",
    "Period 2 indicator",
    "High-tempo first",
    "Participant age",
    "Male participant"
  ),
  se = list(sm1$se, fe_se, sm3$se, sm4$se),
  p = list(sm1$p, fe_p, sm3$p, sm4$p),
  keep.stat = c("n", "rsq", "adj.rsq"),
  notes = table_notes,
  notes.align = "l",
  header = FALSE,
  digits = 3,
  align = TRUE
)

stargazer::stargazer(
  m1, m2, m3, m4,
  type = "text",
  out = full_txt,
  title = "Main Regression Models for the Plank Experiment",
  dep.var.caption = "",
  dep.var.labels = "Plank time to exhaustion (seconds)",
  column.labels = c(
    "Paired contrast",
    "FE crossover",
    "Benchmark raw",
    "Benchmark adjusted"
  ),
  model.numbers = FALSE,
  omit = c("participant_id", "Constant"),
  keep = c("high_tempo", "period_factor2", "high_tempo_first", "participant_age", "male_participant"),
  covariate.labels = c(
    "High-tempo indicator",
    "Period 2 indicator",
    "High-tempo first",
    "Participant age",
    "Male participant"
  ),
  se = list(sm1$se, fe_se, sm3$se, sm4$se),
  p = list(sm1$p, fe_p, sm3$p, sm4$p),
  keep.stat = c("n", "rsq", "adj.rsq"),
  notes = table_notes,
  notes.align = "l",
  header = FALSE,
  digits = 3,
  align = TRUE
)

stargazer::stargazer(
  m1,
  m2,
  type = "html",
  out = compact_html,
  title = "Primary Crossover Models",
  dep.var.caption = "",
  dep.var.labels = "Plank time to exhaustion (seconds)",
  column.labels = c("Paired", "FE crossover"),
  model.numbers = FALSE,
  omit = c("participant_id", "Constant"),
  keep = c("high_tempo", "period_factor2"),
  covariate.labels = c("High-tempo indicator", "Period 2 indicator"),
  se = list(sm1$se, fe_se),
  p = list(sm1$p, fe_p),
  keep.stat = c("n", "rsq"),
  notes = c(
    "Primary analysis uses the randomized crossover design.",
    "Fixed-effects standard errors are clustered at the participant level."
  ),
  notes.align = "l",
  header = FALSE,
  digits = 3,
  align = TRUE
)

cat("Wrote:\n")
cat(full_html, "\n")
cat(full_txt, "\n")
cat(compact_html, "\n")
