# pilot_power_calculation.R
# Reads pilot_data.csv (from process_pilot_csv.R) and runs power simulations
# for (1) between-subjects and (2) within-subjects (same subjects) designs.
# Outcome: plank hold time (seconds). Comparison: slow vs high-tempo.

library(data.table)
library(sandwich)
library(lmtest)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------
PILOT_FILE <- "pilot_data.csv"
SAMPLE_SIZES <- seq(20, 300, by = 20)
N_SIM <- 1000
ALPHA <- 0.05

# ------------------------------------------------------------------------------
# Read pilot data
# ------------------------------------------------------------------------------
pilot <- fread(PILOT_FILE)

# Complete cases for both conditions (needed for within-subjects and for estimating SDs)
pilot_cc <- pilot[!is.na(tte_slow) & !is.na(tte_high)]

if (nrow(pilot_cc) < 4) {
  stop("Need at least 4 complete cases in pilot_data.csv to estimate parameters.")
}

# ------------------------------------------------------------------------------
# Estimate parameters from pilot
# ------------------------------------------------------------------------------
# Between-subjects: two independent groups (slow-only vs high-tempo-only).
# Use marginal means and SDs (pool across groups for each condition).
mean_slow <- pilot_cc[, mean(tte_slow, na.rm = TRUE)]
mean_high <- pilot_cc[, mean(tte_high, na.rm = TRUE)]
sd_slow   <- pilot_cc[, sd(tte_slow, na.rm = TRUE)]
sd_high   <- pilot_cc[, sd(tte_high, na.rm = TRUE)]

# If SD is 0 or NA, use pooled SD or a small default
pooled_sd <- pilot_cc[, sqrt((var(tte_slow, na.rm = TRUE) + var(tte_high, na.rm = TRUE)) / 2)]
if (is.na(sd_slow) || sd_slow <= 0) sd_slow <- max(pooled_sd, 1)
if (is.na(sd_high) || sd_high <= 0) sd_high <- max(pooled_sd, 1)

effect_pilot <- mean_high - mean_slow
# Within-subjects: use difference and SD of difference
pilot_cc[, diff_tte := tte_high - tte_slow]
mean_diff_pilot <- pilot_cc[, mean(diff_tte)]
sd_diff_pilot   <- pilot_cc[, sd(diff_tte)]
if (is.na(sd_diff_pilot) || sd_diff_pilot <= 0) sd_diff_pilot <- max(pooled_sd, 1)

cat("Pilot estimates (complete cases n =", nrow(pilot_cc), "):\n")
cat("  Slow: mean =", round(mean_slow, 1), ", SD =", round(sd_slow, 1), "\n")
cat("  High: mean =", round(mean_high, 1), ", SD =", round(sd_high, 1), "\n")
cat("  Effect (high - slow) =", round(effect_pilot, 1), "s\n")
cat("  Within: mean diff =", round(mean_diff_pilot, 1), ", SD(diff) =", round(sd_diff_pilot, 1), "\n\n")

# ------------------------------------------------------------------------------
# Between-subjects: one observation per person (slow vs high as two groups)
# Simulate Group A = slow condition, Group B = high-tempo condition.
# ------------------------------------------------------------------------------
run_sim_between <- function(N, mean_slow, mean_high, sd_slow, sd_high) {
  n_per_group <- floor(N / 2)
  if (n_per_group < 1) return(NA)
  y_slow <- rnorm(n_per_group, mean = mean_slow, sd = sd_slow)
  y_high <- rnorm(n_per_group, mean = mean_high, sd = sd_high)
  d <- data.table(
    y = c(y_slow, y_high),
    treat = rep(0:1, each = n_per_group)
  )
  mod <- lm(y ~ treat, data = d)
  ci <- coefci(mod, vcov = vcovHC(mod, type = "HC3"), level = 1 - ALPHA)
  lb <- ci["treat", 1]
  ub <- ci["treat", 2]
  as.numeric(!(lb <= 0 && ub >= 0))
}

# ------------------------------------------------------------------------------
# Within-subjects: paired slow vs motivational per person
# ------------------------------------------------------------------------------
run_sim_within <- function(N, mean_diff, sd_diff) {
  diff_sim <- rnorm(N, mean = mean_diff, sd = sd_diff)
  test_result <- t.test(diff_sim, mu = 0)
  as.numeric(test_result$p.value < ALPHA)
}

# ------------------------------------------------------------------------------
# Power estimation
# ------------------------------------------------------------------------------
power_between <- sapply(SAMPLE_SIZES, function(n) {
  mean(replicate(N_SIM, run_sim_between(n, mean_slow, mean_high, sd_slow, sd_high)))
})

power_within <- sapply(SAMPLE_SIZES, function(n) {
  mean(replicate(N_SIM, run_sim_within(n, mean_diff_pilot, sd_diff_pilot)))
})

# ------------------------------------------------------------------------------
# Results table
# ------------------------------------------------------------------------------
results <- data.table(
  N = rep(SAMPLE_SIZES, 2),
  Power = c(power_between, power_within),
  Design = rep(c("Between-subjects", "Within-subjects"), each = length(SAMPLE_SIZES))
)

# Drop NA if any
results <- results[!is.na(Power)]

cat("Power by sample size (alpha =", ALPHA, ", n_sim =", N_SIM, "):\n")
print(dcast(results, N ~ Design, value.var = "Power"))

# Optional: save results
fwrite(results, "pilot_power_results.csv")
message("Saved pilot_power_results.csv")
