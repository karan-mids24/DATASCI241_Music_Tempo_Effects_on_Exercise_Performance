## Purpose

This companion report explains, in plain language, what statistical
models were actually run in the experiment pipeline and why each one
appears. The goal is not to replace the main report, but to make the
analysis faster to audit. Instead of searching through multiple code
chunks, you can use this document as a map from research question to
model, sample, and exact R syntax.

## Model Index

<table>
<caption>Model inventory across the experiment pipeline</caption>
<colgroup>
<col style="width: 10%" />
<col style="width: 16%" />
<col style="width: 19%" />
<col style="width: 39%" />
<col style="width: 14%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Section</th>
<th style="text-align: left;">Model</th>
<th style="text-align: left;">Sample</th>
<th style="text-align: left;">R code</th>
<th style="text-align: left;">File</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Primary crossover</td>
<td style="text-align: left;">Raw paired contrast</td>
<td style="text-align: left;">38 completed pairs</td>
<td style="text-align: left;">t.test(high_tempo_tte, low_tempo_tte,
paired = TRUE)</td>
<td style="text-align: left;">02_primary_crossover_analysis.Rmd</td>
</tr>
<tr class="even">
<td style="text-align: left;">Primary crossover</td>
<td style="text-align: left;">Fixed-effects model without period</td>
<td style="text-align: left;">76 repeated-measures rows from 38
participants</td>
<td style="text-align: left;">lm(outcome_tte ~ high_tempo +
participant_id)</td>
<td style="text-align: left;">02_primary_crossover_analysis.Rmd</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Primary crossover</td>
<td style="text-align: left;">Fixed-effects crossover regression</td>
<td style="text-align: left;">76 repeated-measures rows from 38
participants</td>
<td style="text-align: left;">lm(outcome_tte ~ high_tempo +
period_factor + participant_id)</td>
<td style="text-align: left;">02_primary_crossover_analysis.Rmd</td>
</tr>
<tr class="even">
<td style="text-align: left;">Period/order diagnostics</td>
<td style="text-align: left;">Broad period check</td>
<td style="text-align: left;">76 repeated-measures rows from 38
participants</td>
<td style="text-align: left;">t.test(Session2, Session1)</td>
<td style="text-align: left;">03_period_order_diagnostics.Rmd</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Period/order diagnostics</td>
<td style="text-align: left;">Sequence diagnostic regression</td>
<td style="text-align: left;">76 repeated-measures rows from 38
participants</td>
<td style="text-align: left;">lm(outcome_tte ~ period_factor *
crossover_order)</td>
<td style="text-align: left;">03_period_order_diagnostics.Rmd</td>
</tr>
<tr class="even">
<td style="text-align: left;">First-period benchmark</td>
<td style="text-align: left;">Raw benchmark contrast</td>
<td style="text-align: left;">46 Session 1 observations</td>
<td style="text-align: left;">t.test(outcome_tte ~
high_tempo_first)</td>
<td style="text-align: left;">04_first_period_benchmark.Rmd</td>
</tr>
<tr class="odd">
<td style="text-align: left;">First-period benchmark</td>
<td style="text-align: left;">Precision-adjusted benchmark
regression</td>
<td style="text-align: left;">46 Session 1 observations</td>
<td style="text-align: left;">lm(outcome_tte ~ high_tempo_first +
participant_age + participant_gender)</td>
<td style="text-align: left;">04_first_period_benchmark.Rmd</td>
</tr>
<tr class="even">
<td style="text-align: left;">Precision comparison</td>
<td style="text-align: left;">Precision formulas</td>
<td style="text-align: left;">38 pairs versus 46 benchmark
observations</td>
<td style="text-align: left;">SE, CI width, MDE, and required N
formulas</td>
<td style="text-align: left;">05_precision_power_comparison.Rmd</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Secondary outcomes</td>
<td style="text-align: left;">Secondary paired contrasts</td>
<td style="text-align: left;">38 completed pairs</td>
<td style="text-align: left;">paired t-tests for RPE, Motivation,
Energy</td>
<td style="text-align: left;">06_secondary_outcomes_and_hte.Rmd</td>
</tr>
<tr class="even">
<td style="text-align: left;">Secondary outcomes</td>
<td style="text-align: left;">Secondary fixed-effects models</td>
<td style="text-align: left;">76 repeated-measures rows from 38
participants</td>
<td style="text-align: left;">lm(outcome ~ high_tempo + period_factor +
participant_id)</td>
<td style="text-align: left;">06_secondary_outcomes_and_hte.Rmd</td>
</tr>
<tr class="odd">
<td style="text-align: left;">HTE</td>
<td style="text-align: left;">Treatment-by-subgroup interaction
models</td>
<td style="text-align: left;">76 repeated-measures rows from 38
participants</td>
<td style="text-align: left;">lm(outcome_tte ~ high_tempo + subgroup +
high_tempo:subgroup + period_factor + participant_id)</td>
<td style="text-align: left;">06_secondary_outcomes_and_hte.Rmd</td>
</tr>
<tr class="even">
<td style="text-align: left;">Randomization inference</td>
<td style="text-align: left;">Bridge linear model</td>
<td style="text-align: left;">38 participant-level treatment
differences</td>
<td style="text-align: left;">lm(diff_high_minus_low ~ 1)</td>
<td style="text-align: left;">07_randomization_inference.Rmd</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Randomization inference</td>
<td style="text-align: left;">Sign-flip randomization inference</td>
<td style="text-align: left;">38 participant-level treatment
differences</td>
<td style="text-align: left;">mean(diff * random sign), repeated 10,000
times</td>
<td style="text-align: left;">07_randomization_inference.Rmd</td>
</tr>
</tbody>
</table>

Model inventory across the experiment pipeline

## Primary Crossover Models

The first model family answers the main causal question: did high-tempo
music improve plank time to exhaustion within the same participant? The
simplest version is the raw paired contrast, implemented as
`t.test(high_tempo_tte, low_tempo_tte, paired = TRUE)`. This test uses
the 38 completed pairs and asks whether the average within-person
difference is different from zero. It is the cleanest design-consistent
summary because the treatment effect is measured inside each participant
rather than across different people.

The next step is an intermediate participant fixed-effects model without
a period adjustment, written as
`lm(outcome_tte ~ high_tempo + participant_id)` and estimated with
participant-clustered standard errors. This keeps the linear-model
framework while relying on the same within-person variation as the
paired contrast. In the current analysis, the high-tempo coefficient in
that baseline fixed-effects model is 14.37 seconds with clustered p =
0.0381.

The most formal version is the fixed-effects crossover regression,
written as
`lm(outcome_tte ~ high_tempo + period_factor + participant_id)` and
estimated with participant-clustered standard errors. In words, this
model regresses plank time on the high-tempo indicator, a Period 2
indicator, and a full set of participant fixed effects. The fixed
effects absorb stable demographic and fitness differences, so the
treatment effect is identified from within-person changes. In the
current analysis, the high-tempo coefficient is 14.37 seconds with
clustered p = 0.0391, while the Period 2 coefficient is 3.84 seconds
with p = 0.5703.

## Period And Order Diagnostics

The diagnostic section runs two additional checks. The first is a broad
period comparison, coded as a simple `t.test()` between all Session 2
outcomes and all Session 1 outcomes. This is not the primary causal
model and should be read as a rough descriptive check. It asks whether
second-session performance tended to be higher or lower overall,
regardless of music condition. In this sample, the period-check p-value
is 0.7441, so it does not provide strong evidence of a broad session
effect.

The second diagnostic is the sequence regression
`lm(outcome_tte ~ period_factor * crossover_order)`, again with
participant-clustered standard errors. This model is designed to detect
whether period patterns differ by randomized order arm, which would be
the kind of pattern one might worry about if carryover or order-specific
adaptation were materially affecting the results. The most important
term is the interaction between `period_factor` and `crossover_order`;
its p-value here is 0.0034. That makes this model a validity diagnostic
rather than a headline treatment estimate.

## First-Period Benchmark Models

The benchmark section intentionally throws away Session 2 and keeps only
Session 1. This creates a rough parallel-arm comparison that mimics what
the study would have looked like if each person had only been observed
under one treatment. The raw benchmark contrast is
`t.test(outcome_tte ~ high_tempo_first)`. It is useful because it shows
how much weaker the evidence becomes when the design can no longer
compare participants to themselves. In the benchmark sample of 46
observations, the p-value is 0.8558.

The adjusted benchmark regression is
`lm(outcome_tte ~ high_tempo_first + participant_age + participant_gender)`.
Here demographics become more relevant than they were in the crossover
model because this is now a between-person comparison. The age and
gender terms are included to recover some precision and to reduce
imbalance risk in the smaller Session 1 sample. Even with that
adjustment, the benchmark high-tempo coefficient is -6.68 seconds with p
= 0.6595, which is far less informative than the within-subject
estimate.

## Precision Comparison

Step 5 is not a regression section. Instead, it is a design-comparison
section that calculates standard errors, confidence-interval widths,
minimum detectable effects, and the total sample size a parallel-arm
design would have needed to match the crossover design. For the
crossover design, the key standard error is based on the standard
deviation of the within-person differences divided by the square root of
38 pairs. For the benchmark design, the standard error is based on the
two Session 1 group variances. In this analysis, the crossover standard
error is 4.69 seconds and the benchmark standard error is 13.87 seconds.
This section matters because it explains why the crossover estimate is
much tighter even though the study is not large.

## Secondary Outcome Models

The secondary outcomes section repeats the primary logic for outcomes
other than plank time. The first pass uses paired t-tests for RPE,
motivation, and energy. These are direct within-person contrasts and are
the secondary-outcome analogue of the primary paired treatment test.
Among these, the clearest paired result is for Motivation, with an
estimated difference of 0.474 and p = 0.0371.

The section then re-estimates those secondary outcomes with
fixed-effects regressions of the form
`lm(outcome ~ high_tempo + period_factor + participant_id)` using
participant-clustered standard errors. These models serve the same
purpose as the primary FE model: they account for period and stable
participant traits while focusing interpretation on the high-tempo
coefficient. The strongest fixed-effects secondary result is again
Motivation, with coefficient 0.474 and p = 0.1117.

## HTE Interaction Models

The heterogeneous treatment effect section asks a different question.
Instead of estimating one average treatment effect for everyone, it asks
whether the high-tempo effect changes systematically across subgroups.
The general form is
`lm(outcome_tte ~ high_tempo + subgroup + high_tempo:subgroup + period_factor + participant_id)`,
estimated separately for age, gender, activity level, and plank
familiarity, always with participant-clustered standard errors.

In these models, the key coefficient is not the main `high_tempo` term
but the interaction term `high_tempo:subgroup`. That interaction
measures whether the treatment effect gets larger or smaller as the
subgroup changes. None of the planned subgroup interactions was
statistically persuasive after Bonferroni correction. The smallest raw
interaction p-value is for Age, with interaction estimate 0.272, raw p =
0.5960, and Bonferroni-adjusted p = 1.0000.

## Randomization Inference

The randomization-inference section now begins with a bridge linear
model, `lm(diff_high_minus_low ~ 1)`, whose intercept is exactly the
same paired mean difference used elsewhere in the crossover analysis. In
the current output, that intercept is 14.37 seconds with a conventional
two-sided p-value of 0.0041. This bridge model is not meant to replace
RI; it simply writes the same observed paired estimand in regression
form.

The final inferential check is the design-based randomization-inference
exercise using random sign flips of the participant-level treatment
differences. Under the sharp null of no treatment effect, each observed
within-person difference could just as easily have had the opposite
sign. The code repeatedly flips those signs at random and recalculates
the mean difference 10,000 times, generating the null distribution
implied by the crossover design itself. The resulting two-sided
randomization-inference p-value is 0.0031, which closely tracks the
paired and fixed-effects results.

## Reading The Pipeline Efficiently

If you want the fastest path through the codebase, the main treatment
models live in `02_primary_crossover_analysis.Rmd`, the benchmark
comparison lives in `04_first_period_benchmark.Rmd`, the secondary and
HTE models live in `06_secondary_outcomes_and_hte.Rmd`, and the
design-based inference check lives in `07_randomization_inference.Rmd`.
The precision section in `05_precision_power_comparison.Rmd` is not a
regression section, so it is better read as a design-efficiency appendix
than as another model block.
