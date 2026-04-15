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
long_primary[, crossover_order := factor(crossover_order)]

bench <- copy(long_dt[in_first_period_benchmark == 1L & period == 1L])
bench[, high_tempo_first := high_tempo]
bench[, male_participant := as.integer(participant_gender == "male")]

# Primary family
m1 <- lm(outcome_tte ~ high_tempo + participant_id, data = long_primary)
m2 <- lm(outcome_tte ~ high_tempo + period_factor + participant_id, data = long_primary)

# Diagnostic family
d1 <- lm(outcome_tte ~ period_factor * crossover_order, data = long_primary)

# Benchmark family
m3 <- lm(outcome_tte ~ high_tempo_first, data = bench)
m4 <- lm(outcome_tte ~ high_tempo_first + participant_age + male_participant, data = bench)

# Secondary-outcome family
s1 <- lm(post_rpe ~ high_tempo + period_factor + participant_id, data = long_primary)
s2 <- lm(pre_motivation ~ high_tempo + period_factor + participant_id, data = long_primary)
s3 <- lm(pre_energy ~ high_tempo + period_factor + participant_id, data = long_primary)

# HTE family
make_hte_model <- function(data, moderator_values) {
  tmp <- copy(data)
  tmp[, moderator := moderator_values]
  lm(outcome_tte ~ high_tempo + moderator + high_tempo:moderator + period_factor + participant_id, data = tmp)
}
h1 <- make_hte_model(
  long_primary,
  long_primary$participant_age - mean(long_primary$participant_age, na.rm = TRUE)
)
h2 <- make_hte_model(
  long_primary,
  as.integer(long_primary$participant_gender == "male")
)
h3 <- make_hte_model(
  long_primary,
  as.numeric(long_primary$activity_level_baseline)
)
h4 <- make_hte_model(
  long_primary,
  fcase(
    long_primary$plank_frequency_baseline == 1L, 1L,
    long_primary$plank_frequency_baseline %in% c(2L, 3L), 2L,
    long_primary$plank_frequency_baseline == 4L, 3L,
    default = NA_integer_
  )
)

# Randomization-inference bridge model
r1 <- lm(diff_high_minus_low ~ 1, data = paired)

models <- list(m1, m2, d1, m3, m4, s1, s2, s3, h1, h2, h3, h4, r1)
model_classes <- vapply(models, function(x) class(x)[1], character(1))
cat("Model classes:\n")
print(model_classes)
stopifnot(all(model_classes == "lm"))

extract_sp <- function(model, vcov = NULL) {
  sm <- summary(model)$coefficients
  if (!is.null(vcov)) {
    sm <- lmtest::coeftest(model, vcov. = vcov)
  }
  list(
    se = setNames(sm[, "Std. Error"], rownames(sm)),
    p = setNames(sm[, "Pr(>|t|)"], rownames(sm))
  )
}

clustered_sp <- function(model, data = long_primary) {
  vc <- sandwich::vcovCL(model, cluster = data$participant_id, type = "HC1")
  extract_sp(model, vcov = vc)
}

sm1 <- clustered_sp(m1)
sm2 <- clustered_sp(m2)
sd1 <- clustered_sp(d1)
sm3 <- extract_sp(m3)
sm4 <- extract_sp(m4)
ss1 <- clustered_sp(s1)
ss2 <- clustered_sp(s2)
ss3 <- clustered_sp(s3)
sh1 <- clustered_sp(h1)
sh2 <- clustered_sp(h2)
sh3 <- clustered_sp(h3)
sh4 <- clustered_sp(h4)
sr1 <- extract_sp(r1)

main_notes <- c(
  "Primary analysis uses the randomized crossover design.",
  "Model 1 uses participant fixed effects without a period adjustment; Model 2 adds the Period 2 indicator.",
  "Standard errors for the primary fixed-effects models are clustered at the participant level.",
  "The benchmark uses Session 1 only to emulate a parallel-arm design."
)

write_stargazer <- function(..., out, type = "text", title, dep_var, column_labels = NULL,
                            omit = NULL, keep = NULL, covariate_labels = NULL,
                            se = NULL, p = NULL, keep_stat = c("n", "rsq", "adj.rsq"),
                            notes = NULL, omit_stat = NULL, align = TRUE,
                            font_size = NULL, no_space = FALSE, column_sep_width = "5pt",
                            float = TRUE) {
  stargazer::stargazer(
    ...,
    type = type,
    out = out,
    title = title,
    dep.var.caption = "",
    dep.var.labels = dep_var,
    column.labels = column_labels,
    model.numbers = FALSE,
    omit = omit,
    keep = keep,
    covariate.labels = covariate_labels,
    se = se,
    p = p,
    keep.stat = keep_stat,
    omit.stat = omit_stat,
    notes = notes,
    notes.align = "l",
    header = FALSE,
    digits = 3,
    align = align,
    font.size = font_size,
    no.space = no_space,
    column.sep.width = column_sep_width,
    float = float
  )
}

write_family_stargazer <- function(..., out_txt, out_tex, title, dep_var, column_labels = NULL,
                                   omit = NULL, keep = NULL, covariate_labels = NULL,
                                   se = NULL, p = NULL, keep_stat = c("n", "rsq", "adj.rsq"),
                                   notes = NULL, omit_stat = NULL, latex_font_size = "scriptsize",
                                   latex_column_sep_width = "3pt") {
  write_stargazer(
    ...,
    out = out_txt,
    type = "text",
    title = title,
    dep_var = dep_var,
    column_labels = column_labels,
    omit = omit,
    keep = keep,
    covariate_labels = covariate_labels,
    se = se,
    p = p,
    keep_stat = keep_stat,
    notes = notes,
    omit_stat = omit_stat,
    align = TRUE
  )
  write_stargazer(
    ...,
    out = out_tex,
    type = "latex",
    title = title,
    dep_var = dep_var,
    column_labels = column_labels,
    omit = omit,
    keep = keep,
    covariate_labels = covariate_labels,
    se = se,
    p = p,
    keep_stat = keep_stat,
    notes = notes,
    omit_stat = omit_stat,
    align = FALSE,
    font_size = latex_font_size,
    no_space = TRUE,
    column_sep_width = latex_column_sep_width,
    float = FALSE
  )
}

wrap_stargazer_notes <- function(path, note_width = "0.82\\\\textwidth") {
  lines <- readLines(path, warn = FALSE)
  lines <- gsub(
    "\\\\multicolumn\\{([0-9]+)\\}\\{l\\}\\{",
    paste0("\\\\multicolumn{\\1}{p{", note_width, "}}{"),
    lines
  )
  writeLines(lines, path)
}

full_txt <- file.path(output_dir, "stargazer_main_models.txt")
compact_txt <- file.path(output_dir, "stargazer_primary_models_compact.txt")
primary_family_txt <- file.path(output_dir, "stargazer_primary_family.txt")
primary_family_tex <- file.path(output_dir, "stargazer_primary_family.tex")
diagnostic_family_txt <- file.path(output_dir, "stargazer_diagnostic_family.txt")
diagnostic_family_tex <- file.path(output_dir, "stargazer_diagnostic_family.tex")
benchmark_family_txt <- file.path(output_dir, "stargazer_benchmark_family.txt")
benchmark_family_tex <- file.path(output_dir, "stargazer_benchmark_family.tex")
secondary_family_txt <- file.path(output_dir, "stargazer_secondary_fe_family.txt")
secondary_family_tex <- file.path(output_dir, "stargazer_secondary_fe_family.tex")
hte_family_txt <- file.path(output_dir, "stargazer_hte_family.txt")
hte_family_tex <- file.path(output_dir, "stargazer_hte_family.tex")
ri_family_txt <- file.path(output_dir, "stargazer_ri_bridge_family.txt")
ri_family_tex <- file.path(output_dir, "stargazer_ri_bridge_family.tex")

write_stargazer(
  m1, m2, m3, m4,
  out = full_txt,
  title = "Main Regression Models for the Plank Experiment",
  dep_var = "Plank time to exhaustion (seconds)",
  column_labels = c(
    "FE no period",
    "FE with period",
    "Benchmark raw",
    "Benchmark adjusted"
  ),
  omit = c("participant_id", "Constant"),
  keep = c("high_tempo", "period_factor2", "high_tempo_first", "participant_age", "male_participant"),
  covariate_labels = c(
    "High-tempo indicator",
    "Period 2 indicator",
    "High-tempo first",
    "Participant age",
    "Male participant"
  ),
  se = list(sm1$se, sm2$se, sm3$se, sm4$se),
  p = list(sm1$p, sm2$p, sm3$p, sm4$p),
  keep_stat = c("n", "rsq", "adj.rsq"),
  notes = main_notes
)

write_stargazer(
  m1,
  m2,
  out = compact_txt,
  title = "Primary Crossover Linear Models",
  dep_var = "Plank time to exhaustion (seconds)",
  column_labels = c("FE no period", "FE with period"),
  omit = c("participant_id", "Constant"),
  keep = c("high_tempo", "period_factor2"),
  covariate_labels = c("High-tempo indicator", "Period 2 indicator"),
  se = list(sm1$se, sm2$se),
  p = list(sm1$p, sm2$p),
  keep_stat = c("n", "rsq"),
  notes = c(
    "Primary analysis uses the randomized crossover design.",
    "Model 1 omits the period adjustment; Model 2 adds the Period 2 indicator.",
    "Fixed-effects standard errors are clustered at the participant level."
  )
)

write_family_stargazer(
  m1,
  m2,
  out_txt = primary_family_txt,
  out_tex = primary_family_tex,
  title = "Model Family P: Primary Crossover Regressions",
  dep_var = "Plank time to exhaustion (seconds)",
  column_labels = c("P2: FE no period", "P3: FE with period"),
  omit = c("participant_id", "Constant"),
  keep = c("high_tempo", "period_factor2"),
  covariate_labels = c("High-tempo indicator", "Period 2 indicator"),
  se = list(sm1$se, sm2$se),
  p = list(sm1$p, sm2$p),
  keep_stat = c("n", "rsq", "adj.rsq"),
  notes = c(
    "This table covers the regression members of the primary family; P1 is a paired t-test and is reported outside stargazer.",
    "Both models use the repeated-measures crossover sample.",
    "Standard errors are clustered at the participant level."
  )
)
wrap_stargazer_notes(primary_family_tex)

write_family_stargazer(
  d1,
  out_txt = diagnostic_family_txt,
  out_tex = diagnostic_family_tex,
  title = "Model Family D: Period and Order Diagnostic Regression",
  dep_var = "Plank time to exhaustion (seconds)",
  omit = c("Constant"),
  keep = c("period_factor2", "crossover_orderlow_tempo_first", "period_factor2:crossover_orderlow_tempo_first"),
  covariate_labels = c("Period 2 indicator", "Low-tempo-first order", "Period 2 x low-tempo-first"),
  se = list(sd1$se),
  p = list(sd1$p),
  keep_stat = c("n", "rsq", "adj.rsq"),
  notes = c(
    "This table covers the regression member of the diagnostic family; D1 is the broad period t-test and is reported outside stargazer.",
    "The interaction term is the key validity check.",
    "Standard errors are clustered at the participant level."
  )
)
wrap_stargazer_notes(diagnostic_family_tex)

write_family_stargazer(
  m3,
  m4,
  out_txt = benchmark_family_txt,
  out_tex = benchmark_family_tex,
  title = "Model Family B: First-Period Benchmark Models",
  dep_var = "Plank time to exhaustion (seconds)",
  column_labels = c("B1: Raw contrast", "B2: Adjusted benchmark"),
  omit = c("Constant"),
  keep = c("high_tempo_first", "participant_age", "male_participant"),
  covariate_labels = c("High-tempo first", "Participant age", "Male participant"),
  se = list(sm3$se, sm4$se),
  p = list(sm3$p, sm4$p),
  keep_stat = c("n", "rsq", "adj.rsq"),
  notes = c(
    "Both models use Session 1 only to emulate a parallel-arm design.",
    "B2 adds baseline adjustment to the raw first-period contrast.",
    "Standard errors are classical OLS because each participant contributes one observation."
  )
)
wrap_stargazer_notes(benchmark_family_tex)

write_family_stargazer(
  s1,
  s2,
  s3,
  out_txt = secondary_family_txt,
  out_tex = secondary_family_tex,
  title = "Model Family S: Fixed-Effects Secondary Outcome Models",
  dep_var = c("RPE", "Motivation", "Energy"),
  column_labels = c("S4: RPE FE", "S5: Motivation FE", "S6: Energy FE"),
  omit = c("participant_id", "Constant"),
  keep = c("high_tempo", "period_factor2"),
  covariate_labels = c("High-tempo indicator", "Period 2 indicator"),
  se = list(ss1$se, ss2$se, ss3$se),
  p = list(ss1$p, ss2$p, ss3$p),
  keep_stat = c("n", "rsq", "adj.rsq"),
  notes = c(
    "These are the regression members of the secondary-outcome family; S1-S3 are paired t-tests reported outside stargazer.",
    "All three models use participant fixed effects and a Period 2 control.",
    "Standard errors are clustered at the participant level."
  )
)
wrap_stargazer_notes(secondary_family_tex)

write_family_stargazer(
  h1,
  h2,
  h3,
  h4,
  out_txt = hte_family_txt,
  out_tex = hte_family_tex,
  title = "Model Family H: Heterogeneous Treatment Effect Models",
  dep_var = "Plank time to exhaustion (seconds)",
  column_labels = c("H1: Age", "H2: Gender", "H3: Activity", "H4: Familiarity"),
  omit = c("participant_id", "Constant"),
  keep = c(
    "^high_tempo$",
    "^moderator$",
    "^period_factor2$",
    "^high_tempo:moderator$"
  ),
  covariate_labels = c(
    "High-tempo main effect",
    "Moderator main effect",
    "Period 2 indicator",
    "High tempo x moderator"
  ),
  se = list(sh1$se, sh2$se, sh3$se, sh4$se),
  p = list(sh1$p, sh2$p, sh3$p, sh4$p),
  keep_stat = c("n", "rsq", "adj.rsq"),
  notes = c(
    "The coefficient of interest changes across columns: the treatment-by-subgroup interaction is the HTE test.",
    "All models use participant fixed effects and a Period 2 control.",
    "Standard errors are clustered at the participant level; multiplicity adjustment is discussed in the main report."
  ),
  latex_font_size = "tiny",
  latex_column_sep_width = "1pt"
)
wrap_stargazer_notes(hte_family_tex, note_width = "0.78\\\\textwidth")

write_family_stargazer(
  r1,
  out_txt = ri_family_txt,
  out_tex = ri_family_tex,
  title = "Model Family R: Bridge Linear Model",
  dep_var = "Paired high-minus-low difference (seconds)",
  omit = NULL,
  keep = c("Constant"),
  covariate_labels = c("Average paired effect"),
  se = list(sr1$se),
  p = list(sr1$p),
  keep_stat = c("n"),
  notes = c(
    "This table covers the regression member of the design-based inference family; R2 is the sign-flip randomization-inference procedure reported outside stargazer.",
    "The intercept equals the observed mean paired treatment difference."
  )
)
wrap_stargazer_notes(ri_family_tex)

cat("Wrote:\n")
cat(full_txt, "\n")
cat(compact_txt, "\n")
cat(primary_family_txt, "\n")
cat(primary_family_tex, "\n")
cat(diagnostic_family_txt, "\n")
cat(diagnostic_family_tex, "\n")
cat(benchmark_family_txt, "\n")
cat(benchmark_family_tex, "\n")
cat(secondary_family_txt, "\n")
cat(secondary_family_tex, "\n")
cat(hte_family_txt, "\n")
cat(hte_family_tex, "\n")
cat(ri_family_txt, "\n")
cat(ri_family_tex, "\n")
