# =====================================================================
# Program    : create_adsl.R
# Study      : CDISCPILOT01
# Purpose    : Create the ADaM ADSL (Subject Level Analysis Dataset)
#              from SDTM source data using the {admiral} family of
#              packages and tidyverse tools.
#              ADS Programmer Coding Assessment - Question 2.
# Author     : Ben Nguyen
#
# Inputs     : pharmaversesdtm::dm - basis of ADSL (one row per subject)
#              pharmaversesdtm::vs - vital signs (for LSTAVLDT)
#              pharmaversesdtm::ex - exposure (for TRTSDTM/TRTEDTM, LSTAVLDT)
#              pharmaversesdtm::ds - disposition (for LSTAVLDT)
#              pharmaversesdtm::ae - adverse events (for LSTAVLDT)
# Outputs    : question_2_adam/adsl.xpt   (SAS transport v5)
#              question_2_adam/adsl.rds   (native R format)
#              question_2_adam/log.txt    (execution log / evidence)
#
# How to run : Set the working directory to the repository ROOT.
#
# Notes      : - Function signatures verified against {admiral} v1.5.0
#                documentation (pharmaverse.github.io/admiral), in
#                particular the "Creating ADSL" vignette and the
#                derive_vars_dtm() / derive_vars_cat() reference pages.
#              - AGEGR9/AGEGR9N boundary interpretation (confirmed with
#                the candidate): "18 - 50" is EXCLUSIVE of both 18 and
#                50. Since a 3-bucket partition must still cover every
#                age with no gap, the exact-boundary ages roll into the
#                adjacent named bucket: "<18" effectively means age<=18,
#                and ">50" effectively means age>=50. This is coded
#                explicitly below and called out again at the point of
#                derivation - flag for review if a different boundary
#                convention was intended.
#              - TRTEDTM/TRTETMF are not on the assessment's required
#                variable list, but are kept in the final ADSL (labelled
#                as bonus/intermediate) per the candidate's request, so
#                the derivation is documented rather than silently
#                dropped. TRTEDTM is also directly needed as the 4th
#                LSTAVLDT source component (last valid-dose exposure).
#              - The "valid dose" definition (EXDOSE > 0, or EXDOSE == 0
#                with EXTRT containing "PLACEBO") is written ONCE, as a
#                pre-filtered EX dataset, and reused for both the
#                TRTSDTM/TRTEDTM derivation and the LSTAVLDT component
#                that needs "last valid-dose administration date" -
#                per the candidate's agreed approach, rather than
#                repeating the same filter logic in multiple places.
# =====================================================================

## ---- 0. Logging ------------------------------------------------------
dir.create("question_2_adam", showWarnings = FALSE, recursive = TRUE)
log_con <- file("question_2_adam/log.txt", open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message")

cat("=====================================================\n")
cat("Q2 - ADaM ADSL creation using {admiral}\n")
cat("Run started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=====================================================\n\n")

## ---- 1. Packages -----------------------------------------------------
library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(lubridate)
library(stringr)
library(haven)

## ---- 2. Read input data -----------------------------------------------
dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae

# Blank strings in SDTM character variables (e.g. ARM = "" for a screen
# failure who was never randomized) are NOT the same as NA to R, but
# ARE meant to be treated as missing per CDISC convention. Converting
# them up front matters here in particular: ITTFL below uses !is.na(ARM),
# and if blanks were left as "" that check would wrongly evaluate to
# TRUE (not missing) for every screen failure.
dm <- convert_blanks_to_na(dm)
vs <- convert_blanks_to_na(vs)
ex <- convert_blanks_to_na(ex)
ds <- convert_blanks_to_na(ds)
ae <- convert_blanks_to_na(ae)

cat("Input records - dm:", nrow(dm), " vs:", nrow(vs), " ex:", nrow(ex),
    " ds:", nrow(ds), " ae:", nrow(ae), "\n\n")

## ---- 3. ADSL basis -----------------------------------------------------
# Per {admiral}'s ADSL vignette, DM is the basis of ADSL: one row per
# subject, dropping the DOMAIN variable (not meaningful at subject level).
adsl <- dm %>%
  select(-DOMAIN)

## ---- 4. Shared valid-dose EX filter -------------------------------------
# A valid dose is defined once here and reused everywhere "valid dose" is
# needed (TRTSDTM, TRTEDTM, and the LSTAVLDT exposure component), rather
# than repeating the same filter_add condition multiple times.
#   valid dose := EXDOSE > 0
#              OR (EXDOSE == 0 AND EXTRT contains "PLACEBO")
ex_valid <- ex %>%
  filter(EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO")))

cat("EX records:", nrow(ex), "-> valid-dose EX records:", nrow(ex_valid), "\n\n")

## ---- 5. TRTSDTM/TRTSTMF and TRTEDTM/TRTETMF -----------------------------
# Convert EXSTDTC/EXENDTC (character, possibly partial) to datetimes,
# imputing only the TIME part (highest_imputation = "h" - date itself is
# NOT imputed, matching "the derivation only includes observations where
# ... datepart of Start Date/Time of Treatment is complete").
#   - time_imputation = "00:00:00": a completely missing time is imputed
#     as 00:00:00 (first possible time), matching the spec's "impute
#     completely missing time with 00:00:00 ... 00 for missing hours,
#     00 for missing minutes, 00 for missing seconds".
#   - ignore_seconds_flag = TRUE: per {admiral} docs, this is the exact,
#     documented mechanism for the ADaM IG rule quoted in the spec -
#     "If only seconds are missing then do not populate the imputation
#     flag (TRTSTMF)" - not a workaround, this is what the argument is
#     built for.
ex_ext <- ex_valid %>%
  derive_vars_dtm(
    dtc               = EXSTDTC,
    new_vars_prefix   = "EXST",
    highest_imputation = "h",
    time_imputation    = "00:00:00",
    ignore_seconds_flag = TRUE
  ) %>%
  derive_vars_dtm(
    dtc               = EXENDTC,
    new_vars_prefix   = "EXEN",
    highest_imputation = "h",
    time_imputation    = "00:00:00",
    ignore_seconds_flag = TRUE
  )

adsl <- adsl %>%
  # TRTSDTM: first valid-dose exposure record with a complete start date,
  # sorted by start datetime then EXSEQ as a tiebreaker.
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add  = !is.na(EXSTDTM),
    new_vars    = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order       = exprs(EXSTDTM, EXSEQ),
    mode        = "first",
    by_vars     = exprs(STUDYID, USUBJID)
  ) %>%
  # TRTEDTM (bonus/intermediate - see header note): last valid-dose
  # exposure record with a complete end date.
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add  = !is.na(EXENDTM),
    new_vars    = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order       = exprs(EXENDTM, EXSEQ),
    mode        = "last",
    by_vars     = exprs(STUDYID, USUBJID)
  )

# TRTEDT (date-only) is needed as the "last dose administration" source
# for LSTAVLDT below (component 4 of the spec). Derived here as a small,
# clearly-labelled intermediate rather than repeating date logic later.
adsl <- adsl %>%
  derive_vars_dtm_to_dt(source_vars = exprs(TRTEDTM))

cat("TRTSDTM missing:", sum(is.na(adsl$TRTSDTM)),
    "  TRTEDTM missing:", sum(is.na(adsl$TRTEDTM)), "\n\n")

## ---- 6. AGEGR9 / AGEGR9N ------------------------------------------------
# Boundary convention (confirmed with candidate): "18 - 50" excludes both
# 18 and 50 exactly; those boundary ages roll into the adjacent named
# bucket so the partition has no gap:
#   AGE <= 18            -> "<18"     (1)
#   18 < AGE < 50        -> "18 - 50" (2)
#   AGE >= 50             -> ">50"    (3)
#   AGE missing          -> NA        (checked first, so it is never
#                                       accidentally caught by is.na()
#                                       propagating through a comparison)
agegr9_lookup <- exprs(
  ~condition,        ~AGEGR9,    ~AGEGR9N,
  is.na(AGE),        NA_character_, NA_real_,
  AGE <= 18,         "<18",      1,
  AGE > 18 & AGE < 50, "18 - 50", 2,
  AGE >= 50,         ">50",      3
)

adsl <- adsl %>%
  derive_vars_cat(definition = agegr9_lookup)

cat("AGEGR9 frequency:\n")
print(table(adsl$AGEGR9, useNA = "ifany"))
cat("\n")

## ---- 7. ITTFL ------------------------------------------------------------
# "Y" if DM.ARM is populated (i.e. the subject was randomized to an arm),
# "N" otherwise. Relies on the convert_blanks_to_na() step above so that
# blank-string ARM values (e.g. for screen failures) are correctly
# treated as missing rather than "populated".
adsl <- adsl %>%
  mutate(ITTFL = if_else(!is.na(ARM), "Y", "N"))

cat("ITTFL frequency:\n")
print(table(adsl$ITTFL, useNA = "ifany"))
cat("\n")

## ---- 8. LSTAVLDT ---------------------------------------------------------
# Last known alive date = max of, across four sources:
#   (1) last complete VS visit date with a valid result
#       (VSSTRESN and VSSTRESC not BOTH missing) and VSDTC datepart present
#   (2) last complete AE onset date (AESTDTC datepart present)
#   (3) last complete DS disposition start date (DSSTDTC datepart present)
#   (4) last valid-dose exposure end date (ADSL.TRTEDT, derived in step 5
#       from the SAME shared valid-dose filter used for TRTEDTM - this is
#       exactly "the last date of treatment administration where patient
#       received a valid dose")
#
# Implemented with derive_vars_extreme_event(), mirroring the LSTALVDT
# pattern in the official admiral ADSL vignette (there sourced from
# AE/LB/ADSL; here adapted to VS/AE/DS/ADSL per this assessment's spec).
# convert_dtc_to_dt() with highest_imputation = "M" would impute partial
# dates; the spec calls for *complete* dates only, so no imputation
# argument is passed (a partial date will not parse and is excluded,
# matching "last complete ... date").
adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      # (1) vital signs: valid result + complete date
      event(
        dataset_name = "vs",
        condition    = !(is.na(VSSTRESN) & is.na(VSSTRESC)) & !is.na(VSDTC),
        order        = exprs(convert_dtc_to_dt(VSDTC), VSSEQ),
        set_values_to = exprs(
          LSTAVLDT = convert_dtc_to_dt(VSDTC),
          LALVDOM  = "VS",
          LALVSEQ  = VSSEQ
        )
      ),
      # (2) adverse event onset
      event(
        dataset_name = "ae",
        condition    = !is.na(AESTDTC),
        order        = exprs(convert_dtc_to_dt(AESTDTC), AESEQ),
        set_values_to = exprs(
          LSTAVLDT = convert_dtc_to_dt(AESTDTC),
          LALVDOM  = "AE",
          LALVSEQ  = AESEQ
        )
      ),
      # (3) disposition start date
      event(
        dataset_name = "ds",
        condition    = !is.na(DSSTDTC),
        order        = exprs(convert_dtc_to_dt(DSSTDTC), DSSEQ),
        set_values_to = exprs(
          LSTAVLDT = convert_dtc_to_dt(DSSTDTC),
          LALVDOM  = "DS",
          LALVSEQ  = DSSEQ
        )
      ),
      # (4) last valid-dose exposure end date, taken from ADSL itself
      # (TRTEDT was derived in step 5 from the shared valid-dose filter)
      event(
        dataset_name = "adsl",
        condition    = !is.na(TRTEDT),
        set_values_to = exprs(
          LSTAVLDT = TRTEDT,
          LALVDOM  = "ADSL",
          LALVSEQ  = NA_integer_
        )
      )
    ),
    source_datasets = list(vs = vs, ae = ae, ds = ds, adsl = adsl),
    tmp_event_nr_var = event_nr,
    order   = exprs(LSTAVLDT, LALVSEQ, event_nr),
    mode    = "last",
    new_vars = exprs(LSTAVLDT, LALVDOM, LALVSEQ)
  )

cat("LSTAVLDT missing:", sum(is.na(adsl$LSTAVLDT)), "\n")
cat("LSTAVLDT source domain (LALVDOM) frequency:\n")
print(table(adsl$LALVDOM, useNA = "ifany"))
cat("\n")

## ---- 9. QC summaries (captured in the log as run evidence) ---------------
cat("----------------- QC SUMMARY -----------------\n")
cat("ADSL subjects:", nrow(adsl), " (DM subjects:", nrow(dm), ")\n\n")

cat("AGEGR9 x AGEGR9N cross-check (should be a clean 1:1 mapping):\n")
print(table(adsl$AGEGR9, adsl$AGEGR9N, useNA = "ifany"))

cat("\nITTFL x ARM populated cross-check:\n")
print(table(adsl$ITTFL, !is.na(adsl$ARM), useNA = "ifany"))

cat("\nMissingness for all newly derived variables:\n")
print(colSums(is.na(adsl[, c(
  "AGEGR9", "AGEGR9N", "TRTSDTM", "TRTSTMF",
  "TRTEDTM", "TRTETMF", "ITTFL", "LSTAVLDT"
)])))

## ---- 10. Export deliverables ----------------------------------------------
haven::write_xpt(adsl, "question_2_adam/adsl.xpt", version = 5, name = "adsl")
saveRDS(adsl, "question_2_adam/adsl.rds")
cat("\nWritten: question_2_adam/adsl.xpt and question_2_adam/adsl.rds\n")

## ---- 11. Verification output (required by the assessment) ----------------
cat("\n----------------- str(adsl) -----------------\n")
str(adsl)

cat("\n----------------- head(adsl, 10) -----------------\n")
print(head(adsl, 10))

cat("\n----------------- sessionInfo -----------------\n")
print(sessionInfo())

cat("\nRun finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

## ---- 12. Close the log ------------------------------------------------------
sink(type = "message")
sink()
close(log_con)