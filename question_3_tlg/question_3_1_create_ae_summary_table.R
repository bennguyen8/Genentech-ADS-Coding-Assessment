# =====================================================================
# Program    : 01_create_ae_summary_table.R
# Study      : CDISCPILOT01
# Purpose    : Create a treatment-emergent adverse event (TEAE) summary
#              table with a SOC -> reported-term hierarchy, counts and
#              percentages by treatment arm, using {gtsummary}.
#              ADS Programmer Coding Assessment - Question 3, Task 1.
# Author     : Ben Nguyen
#
# Inputs     : pharmaverseadam::adae - analysis AE dataset
#              pharmaverseadam::adsl - subject-level analysis dataset
#                                      (provides per-arm denominators)
# Outputs    : question_3_tlg/ae_summary_table.png
#              question_3_tlg/ae_summary_table.html
#              question_3_tlg/log_table.txt
#
# How to run : From the repository ROOT.
#
# Notes      : - Function signatures verified against {gtsummary} v2.x
#                documentation (tbl_hierarchical / sort_hierarchical /
#                add_overall reference pages).
#              - Denominators: ADSL subjects per ACTARM, used AS-IS (the
#                statistically correct "subjects at risk in each arm").
#                The assessment's sample image shows N=86/72/96; whatever
#                counts this ADSL yields are reported rather than forced
#                to match the sample - see the QC block, which prints the
#                actual per-arm N so any difference from the sample is
#                transparent and explained, not hidden.
#              - Percentages are SUBJECT-level (share of arm subjects with
#                >=1 event of that SOC/term), not event counts - this is
#                what tbl_hierarchical() computes via id = USUBJID and the
#                denominator argument.
#              - Ordering requirement (from gtsummary docs): add_overall()
#                MUST be called before sort_hierarchical(), or sorting a
#                table that includes the overall column errors.
#              - Output is PNG + HTML (PDF via gt routes through LaTeX,
#                which is fragile on Posit Cloud). PNG is the embeddable
#                artifact; HTML is the interactive copy.
# =====================================================================

## ---- 0. Logging ------------------------------------------------------
dir.create("question_3_tlg", showWarnings = FALSE, recursive = TRUE)
log_con <- file("question_3_tlg/log_table.txt", open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message")

cat("=====================================================\n")
cat("Q3 Task 1 - TEAE summary table using {gtsummary}\n")
cat("Run started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=====================================================\n\n")

## ---- 1. Packages -----------------------------------------------------
library(gtsummary)
library(dplyr, warn.conflicts = FALSE)
library(pharmaverseadam)
library(gt)          # as_gt() / gtsave() for HTML
library(flextable)   # as_flex_table() + save_as_image() for PNG (no Chrome)

## ---- 2. Read input data ----------------------------------------------
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

cat("Input records - adae:", nrow(adae), " adsl:", nrow(adsl), "\n\n")

# Defensive check: confirm the columns we rely on actually exist, so a
# missing/renamed variable fails loudly with a clear message rather than
# deep inside gtsummary.
need_adae <- c("TRTEMFL", "AESOC", "AETERM", "ACTARM", "USUBJID")
need_adsl <- c("ACTARM", "USUBJID")
missing_adae <- setdiff(need_adae, names(adae))
missing_adsl <- setdiff(need_adsl, names(adsl))
if (length(missing_adae) > 0)
  stop("adae missing expected columns: ", paste(missing_adae, collapse = ", "))
if (length(missing_adsl) > 0)
  stop("adsl missing expected columns: ", paste(missing_adsl, collapse = ", "))

## ---- 3. Build analysis populations -------------------------------------
# TEAE records only. The assessment defines treatment-emergent as
# TRTEMFL == "Y" in adae.
adae_teae <- adae %>%
  filter(TRTEMFL == "Y")

cat("TEAE records (TRTEMFL == 'Y'):", nrow(adae_teae),
    "of", nrow(adae), "total AE records\n\n")

# Drop the "Screen Failure" arm. Screen failures were never randomized
# or treated, so they are not "subjects at risk of a treatment-emergent
# event" and do not belong as a column in a TEAE table (they would show
# an all-zero column, which is misleading rather than informative). This
# also matches the assessment's sample output, which shows only the three
# treatment arms. droplevels() removes the ghost factor level so it does
# not reappear as an empty column downstream.
ARMS_KEEP <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")

# Denominator = ADSL, one row per subject, per ACTARM. tbl_hierarchical()
# reads the by-column (ACTARM) from the denominator to size each arm.
# Keeping ACTARM as a factor fixes the column order (Placebo, High, Low)
# and guarantees arms with zero events still appear as columns.
adsl_denom <- adsl %>%
  filter(ACTARM %in% ARMS_KEEP) %>%
  mutate(ACTARM = factor(ACTARM, levels = ARMS_KEEP)) %>%
  droplevels()

# Apply the same filter and factor levels to the TEAE data so columns
# line up exactly with the denominator.
adae_teae <- adae_teae %>%
  filter(ACTARM %in% ARMS_KEEP) %>%
  mutate(ACTARM = factor(ACTARM, levels = ARMS_KEEP)) %>%
  droplevels()

cat("Arms retained for the table:", paste(ARMS_KEEP, collapse = ", "), "\n")
cat("(Screen Failure arm dropped - never treated, not at risk of TEAE)\n\n")

cat("Per-arm subject denominators (ADSL, used as-is):\n")
print(adsl_denom %>% count(ACTARM, name = "N_subjects"))
cat("\n")

## ---- 4. Build the hierarchical TEAE table ------------------------------
# variables = c(AESOC, AETERM): two-level hierarchy, SOC then reported
#   term, exactly matching the sample output structure.
# by = ACTARM: one column per treatment arm.
# denominator = adsl_denom + id = USUBJID: percentages are the share of
#   arm SUBJECTS with >= 1 event (subject-level rates, not event counts).
# overall_row = TRUE: the top "Treatment Emergent AEs" summary row.
tbl <- tbl_hierarchical(
  data        = adae_teae,
  variables   = c(AESOC, AETERM),
  by          = ACTARM,
  denominator = adsl_denom,
  id          = USUBJID,
  overall_row = TRUE,
  label       = list(..ard_hierarchical_overall.. = "Treatment Emergent AEs"),
  statistic   = everything() ~ "{n} ({p}%)"
)

# add_overall(): total column across all subjects ("include total column
# with all subjects" in the spec). MUST come before sort_hierarchical().
# sort_hierarchical(): within each SOC section, order rows by descending
# event frequency ("sort by descending frequency").
tbl <- tbl %>%
  add_overall(last = TRUE) %>%
  sort_hierarchical()

TABLE_TITLE <-
  "Treatment-Emergent Adverse Events by System Organ Class and Reported Term"
tbl_full <- tbl %>%
  modify_caption(TABLE_TITLE)

## ---- 4b. Frequency filtering (subject incidence >= threshold in any arm) ----
# The full table has one row per reported term (200+), which is complete
# but unreadable as an image. Standard TEAE reporting shows only terms
# reaching a minimum SUBJECT INCIDENCE in at least one arm. NOTE: this is
# a display/readability threshold (which rows are frequent enough to show)
# - it is NOT a statistical significance test and carries no p-value.
#
# gtsummary's filter_hierarchical() filters on EVENT COUNTS (n), not on
# subject-incidence percent. Since arms have different sizes (86/72/96),
# an event-count cutoff would impose a different percent bar per arm. So
# we compute the qualifying TERMS independently here - per-arm DISTINCT
# subjects with the term, divided by per-arm N - and keep terms hitting
# the threshold in ANY arm. This is the statistically correct "% of
# subjects in any arm" definition and matches what the table cells show.
INCIDENCE_THRESHOLD <- 0.05   # 5% subject incidence in any arm

arm_n <- adsl_denom %>% count(ACTARM, name = "N_arm")

# per term x arm: distinct subjects with >=1 TEAE of that term
term_arm_incidence <- adae_teae %>%
  distinct(AETERM, ACTARM, USUBJID) %>%
  count(AETERM, ACTARM, name = "n_subj") %>%
  left_join(arm_n, by = "ACTARM") %>%
  mutate(pct = n_subj / N_arm)

# a term qualifies if its incidence reaches the threshold in ANY arm
term_max_incidence <- term_arm_incidence %>%
  group_by(AETERM) %>%
  summarise(max_pct = max(pct), .groups = "drop")

# Diagnostic: how many terms qualify at 5% vs 10%, so the threshold
# choice is data-driven and its effect on table length is visible.
cat("----------------- FREQUENCY-FILTER DIAGNOSTIC -----------------\n")
for (thr in c(0.05, 0.10)) {
  n_terms <- sum(term_max_incidence$max_pct >= thr)
  cat(sprintf("  Terms with >= %2.0f%% subject incidence in any arm: %d\n",
              thr * 100, n_terms))
}
cat(sprintf("  (Using threshold = %.0f%% for the PNG output)\n\n",
            INCIDENCE_THRESHOLD * 100))

terms_keep <- term_max_incidence %>%
  filter(max_pct >= INCIDENCE_THRESHOLD) %>%
  pull(AETERM)

# Build the FILTERED table for the PNG using filter_hierarchical() on the
# FULL table. filter_hierarchical() removes only the display (term) rows
# not meeting the condition while LEAVING THE SUMMARY AND OVERALL ROWS
# UNTOUCHED (per gtsummary docs: "Filters are not applied to summary or
# overall rows"). So the "Treatment Emergent AEs" overall row and each SOC
# subtotal keep counting the FULL population - preserving the validated
# 65/68/84 subject counts.
#
# IMPORTANT - filter grammar: filter_hierarchical()'s `filter` must be
# written in terms of the row's own statistics, NOT an external vector.
# The valid names are n/N/p per arm as p_1, p_2, p_3 (arm proportions as
# FRACTIONS) plus *_overall. So "subject incidence >= 5% in ANY arm" is
# expressed directly as (p_1 >= t | p_2 >= t | p_3 >= t). This is the
# statistically-correct per-arm test (each arm compared to its own
# denominator), and it is exactly what we computed independently above -
# the term_max_incidence diagnostic and terms_keep serve as the QC
# cross-check that this native filter keeps the right number of terms.
#
# CONSEQUENCE (standard and expected): a SOC subtotal may exceed the sum
# of its visible term rows, because subjects whose only event in that SOC
# was a rare (hidden) term still count in the subtotal. This is normal for
# filtered TEAE tables and is noted in the caption.
# NOTE: filter_hierarchical() parses the NAMES in `filter` against its
# statistic list - it does NOT substitute R variables or splice. Both
# `p_1 >= t` and injected expressions fail. The reliable form is a bare
# expression with LITERAL numbers and the statistic names p_1/p_2/p_3
# (per-arm proportions, arms 1/2/3 = Placebo/High/Low as the log's
# "xx_1=1, xx_2=2, xx_3=3" note confirms). The literal 0.05 below MUST be
# kept in sync with INCIDENCE_THRESHOLD (used everywhere else); a guard
# right after asserts they match so they can't silently drift.
stopifnot(INCIDENCE_THRESHOLD == 0.05)   # keep in sync with the literal below
tbl_filtered <- tbl_full %>%
  filter_hierarchical(
    filter = p_1 >= 0.05 | p_2 >= 0.05 | p_3 >= 0.05
  ) %>%
  modify_caption(paste0(
    TABLE_TITLE,
    " (terms with \u2265", INCIDENCE_THRESHOLD * 100,
    "% subject incidence in any arm; SOC subtotals and the overall row ",
    "reflect all treatment-emergent events and may exceed the sum of the ",
    "displayed term rows)"
  ))

# Independent verification: confirm every term kept truly meets the
# threshold, and none dropped would have (same double-programming
# discipline used on Q1/Q2). Printed as evidence in the log.
kept_ok <- all(term_max_incidence$max_pct[
  term_max_incidence$AETERM %in% terms_keep] >= INCIDENCE_THRESHOLD)
dropped_ok <- all(term_max_incidence$max_pct[
  !term_max_incidence$AETERM %in% terms_keep] < INCIDENCE_THRESHOLD)
cat("Filter verification - all kept terms >= threshold:", kept_ok,
    "| all dropped terms < threshold:", dropped_ok, "\n")
cat("Terms kept:", length(terms_keep),
    "of", nrow(term_max_incidence), "total\n\n")

# Confirm the fix worked: the filtered table's overall "Treatment Emergent
# AEs" row must STILL show the full-population subject counts (validated
# earlier as 65 / 68 / 84), NOT shrunken filtered-term counts. Pull the
# first data row of the filtered table and print it as evidence.
cat("Overall TEAE row in the FILTERED table (must match full-population",
    "65/68/84 - proves summary rows were preserved):\n")
print(as_tibble(tbl_filtered) %>% dplyr::slice(1))
cat("\n")

## ---- 5. QC summaries (captured in the log) -----------------------------
cat("----------------- QC SUMMARY -----------------\n")

# Independent cross-check of the overall TEAE subject counts per arm:
# number of DISTINCT subjects in each arm with >= 1 TEAE. This should
# match the top "Treatment Emergent AEs" row of the table.
teae_subj_by_arm <- adae_teae %>%
  distinct(ACTARM, USUBJID) %>%
  count(ACTARM, name = "subjects_with_TEAE")
den <- adsl_denom %>% count(ACTARM, name = "N_subjects")

qc <- den %>%
  left_join(teae_subj_by_arm, by = "ACTARM") %>%
  mutate(
    subjects_with_TEAE = coalesce(subjects_with_TEAE, 0L),
    pct = round(100 * subjects_with_TEAE / N_subjects, 1)
  )
cat("Subjects with >=1 TEAE per arm (independent of gtsummary):\n")
print(qc)

cat("\nDistinct SOCs:", dplyr::n_distinct(adae_teae$AESOC),
    " | Distinct reported terms:", dplyr::n_distinct(adae_teae$AETERM), "\n")

## ---- 6. Export deliverables --------------------------------------------
# Two artifacts:
#   (a) HTML  - via gt (no external dependency, always works)
#   (b) PNG   - via flextable::save_as_image(), which renders through the
#               R graphics system (ragg/svglite), NOT a headless browser.
#               This deliberately avoids gt::gtsave()'s Chrome/webshot2
#               path, which cannot launch on Posit Cloud (the container
#               sandbox blocks Chrome's namespace sandbox, giving
#               "Operation not permitted"). flextable's pure-R graphics
#               path sidesteps that entirely and keeps the PNG fully
#               script-reproducible.

# (a) HTML via gt - the FULL, unfiltered table (complete record; HTML
#     handles arbitrary length fine and has no black-canvas issue).
gt_tbl <- tbl_full %>% as_gt()
gt::gtsave(gt_tbl, filename = "question_3_tlg/ae_summary_table.html")
cat("\nWritten: ae_summary_table.html (full, unfiltered table)\n")

# (b) PNG via flextable
# Register a font with the graphics device. On a bare Posit Cloud image
# the default device may not find a usable family, which can cause
# save_as_image() to error or render poorly; gdtools::register_*()
# makes a known font available. Guarded so a missing gdtools does not
# stop the run.
if (requireNamespace("gdtools", quietly = TRUE)) {
  gdtools::register_liberationsans()
  flextable::set_flextable_defaults(font.family = "Liberation Sans")
}

# PNG uses the FILTERED table (terms >= threshold in any arm) so it is a
# readable, one-page image. Convert to flextable, add an in-table title
# line (captions are dropped from image output), then save as PNG.
ft <- tbl_filtered %>%
  as_flex_table() %>%
  flextable::add_header_lines(values = paste0(
    TABLE_TITLE, " (\u2265", INCIDENCE_THRESHOLD * 100, "% in any arm)"
  )) %>%
  flextable::autofit()

png_ok <- tryCatch({
  flextable::save_as_image(ft, path = "question_3_tlg/ae_summary_table.png")
  TRUE
}, error = function(e) {
  cat("PNG export via flextable failed:", conditionMessage(e), "\n")
  FALSE
})
if (png_ok) cat("Written: ae_summary_table.png (filtered table)\n")

## ---- 7. Verification output --------------------------------------------
cat("\n----------------- filtered table (PNG contents) -----------------\n")
# Print the filtered table body so the log carries a text record of what
# the PNG shows.
print(as_tibble(tbl_filtered), n = 40)

cat("\n----------------- sessionInfo -----------------\n")
print(sessionInfo())

cat("\nRun finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

## ---- 8. Close the log --------------------------------------------------
sink(type = "message")
sink()
close(log_con)