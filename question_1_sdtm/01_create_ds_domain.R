# =====================================================================
# Program    : 01_create_ds_domain.R
# Study      : CDISCPILOT01
# Purpose    : Create the SDTM DS (Disposition) domain from raw EDC data
#              using the {sdtm.oak} package.
#              ADS Programmer Coding Assessment - Question 1.
# Author     : Ben Nguyen
#
# Inputs     : pharmaverseraw::ds_raw  - raw "Subject Disposition" eCRF data
#              study_ct.csv            - study controlled terminology (CT)
#              pharmaversesdtm::dm     - source of USUBJID and the reference
#                                        start date (RFSTDTC) for study day
# Outputs    : question_1_sdtm/ds.xpt          (SAS transport v5)
#              question_1_sdtm/ds_domain.rds   (native R format)
#              question_1_sdtm/log.txt         (execution log / evidence)
#
# How to run : Set the working directory to the repository ROOT
#              (the folder that contains question_1_sdtm/).
#              study_ct.csv must sit in that root folder.
#
# Notes      : - Function signatures verified against {sdtm.oak} v0.2.0
#                documentation (pharmaverse.github.io/sdtm.oak).
#              - {sdtm.oak} prints informational messages such as
#                "These terms could not be mapped per the controlled
#                terminology" for values outside the codelist (e.g. the
#                protocol-milestone term "Randomized", which is not part
#                of codelist C66727). This is expected, documented
#                behaviour: unmapped values are passed through in upper
#                case. Those messages are captured in the log on purpose.
#              - Raw variables FORM/FORML/SITENM/DEATHDT are not needed
#                for the 12 requested DS variables and are intentionally
#                not mapped (death dates are typically reflected in
#                DM.DTHDTC rather than in DS timing variables).
# =====================================================================

## ---- 0. Logging ------------------------------------------------------
# Ensure the output folder exists before anything tries to write into
# it. Without this, file() below fails with "cannot open the
# connection" whenever the script is run fresh (e.g. after cloning the
# repo), which in turn leaves `log_con` unassigned and produces a
# confusing downstream "object 'log_con' not found" error.
dir.create("question_1_sdtm", showWarnings = FALSE, recursive = TRUE)

# A file connection + two sinks so that BOTH regular output and
# messages/warnings (including {sdtm.oak}'s CT mapping messages) are
# captured as run evidence. split = TRUE echoes output to the console
# as well, which is convenient when running interactively.
# NOTE: if the script errors mid-run, restore the console with:
#   sink(type = "message"); sink()
log_con <- file("question_1_sdtm/log.txt", open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message")

cat("=====================================================\n")
cat("Q1 - SDTM DS domain creation using {sdtm.oak}\n")
cat("Run started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=====================================================\n\n")

## ---- 1. Packages -----------------------------------------------------
library(sdtm.oak)        # SDTM mapping algorithms
library(dplyr)           # data manipulation
library(pharmaverseraw)  # raw eCRF data: ds_raw
library(pharmaversesdtm) # SDTM DM domain (USUBJID / RFSTDTC)
library(haven)           # write_xpt() for SAS transport files

## ---- 2. Read input data ----------------------------------------------
# Raw disposition data. One row per collected disposition record; the
# mock eCRF ("Subject Disposition and Study Drug Completion") shows:
#   IT.DSTERM  - reported term for the disposition event (verbatim)
#   IT.DSDECOD - the reason checkbox (Randomized / Completed / ... )
#   OTHERSP    - "if something else, please specify" free text
#   DSDTCOL    - date of collection, MM-DD-YYYY
#   DSTMCOL    - time of collection, HH:MM (often not collected)
#   IT.DSSTDAT - date subject completed/discontinued, MM-DD-YYYY
ds_raw <- pharmaverseraw::ds_raw

# The DM domain provides the authoritative USUBJID and the subject
# reference start date (RFSTDTC) needed for DSSTDY.
dm <- pharmaversesdtm::dm

# Fail fast with a clear message if the CT file is not where expected.
if (!file.exists("study_ct.csv")) {
  stop(
    "study_ct.csv not found in the working directory. ",
    "Run this script from the repository root and make sure the ",
    "controlled terminology CSV is saved there as 'study_ct.csv'."
  )
}

# Study controlled terminology. This file follows the {sdtm.oak}
# ct_spec structure (codelist_code / term_code / term_value /
# collected_value / term_preferred_term / term_synonyms) and contains,
# among others:
#   C66727   - Completion/Reason for Non-Completion (DSDECOD)
#   C74558   - Category for Disposition Event (DSCAT values)
#   VISIT /  - study-specific lookups mapping the raw INSTANCE text
#   VISITNUM   (e.g. "Baseline", "Week 26") to SDTM VISIT / VISITNUM
study_ct <- read.csv("study_ct.csv", stringsAsFactors = FALSE)

# --- Align study CT collected_value to the raw verbatim terms ----------
# {sdtm.oak}'s assign_ct()/ct_map() matches the raw value against the CT
# 'collected_value' (and 'term_synonyms') columns, CASE-SENSITIVELY, and
# passes any non-match through in upper case. Inspecting the first run's
# log showed several C66727 disposition reasons that did NOT map because
# the study's as-collected verbatim text differs from the CT's
# collected_value - some by case only, some by wording:
#
#   raw IT.DSDECOD                 CT collected_value (before)
#   -----------------------------  ------------------------------
#   "Completed"                    "Complete"                    (wording)
#   "Screen Failure"               "Trial Screen Failure"        (wording)
#   "Study Terminated by Sponsor"  "Study Terminated By Sponsor" (case)
#   "Lost to Follow-Up"            "Lost To Follow-Up"           (case)
#   (Death already maps via the "Death" synonym.)
#
# The correct SDTM fix is to make the study CT's collected_value reflect
# what was actually collected on the eCRF - i.e. CT authoring - rather
# than bending the matching engine or post-processing DSDECOD by hand.
# This keeps oak's validated ct_map() in charge and leaves a clear,
# auditable record of exactly which terms were aligned. A case-only
# normalization was rejected because two of these ("Completed",
# "Screen Failure") differ by wording, not case, and would remain
# unmapped.
#
# NOTE: "Randomized" is deliberately NOT added here - it is a protocol
# milestone, not a C66727 disposition reason, and is meant to pass
# through to DSDECOD = "RANDOMIZED" (see DSCAT logic below).
ct_align <- c(
  "Complete"                    = "Completed",
  "Trial Screen Failure"        = "Screen Failure",
  "Study Terminated By Sponsor" = "Study Terminated by Sponsor",
  "Lost To Follow-Up"           = "Lost to Follow-Up"
)
for (old_val in names(ct_align)) {
  study_ct$collected_value[study_ct$collected_value == old_val] <-
    ct_align[[old_val]]
}

# Same case-sensitivity issue affects the visit lookups: the raw INSTANCE
# value "Ambul Ecg Removal" did not match the CT collected_value
# "Ambul ECG Removal" (ECG upper-cased), leaving 4 records with a missing
# VISITNUM in the first run. Align the CT to the raw casing so the visit
# maps cleanly to VISITNUM 6 / VISIT "AMBUL ECG REMOVAL".
study_ct$collected_value[study_ct$collected_value == "Ambul ECG Removal"] <-
  "Ambul Ecg Removal"

cat("Input records read from pharmaverseraw::ds_raw:", nrow(ds_raw), "\n\n")

## ---- 3. Generate oak id variables --------------------------------------
# {sdtm.oak} links every derived variable back to the raw records via
# three key variables (oak_id, raw_source, patient_number). These must
# be created once on the raw dataset before any mapping starts.
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",   # raw subject identifier, e.g. "701-1015"
    raw_src = "ds_raw"
  )

## ---- 4. Map the topic variable: DSTERM ---------------------------------
# DSTERM = reported term for the disposition event (no CT, verbatim).
# Two collection paths on the eCRF feed the same SDTM variable:
#   (1) IT.DSTERM - populated for checkbox-based disposition records
#   (2) OTHERSP   - populated when the site chose "something else,
#                   please specify" (protocol milestones such as
#                   "Final Lab Visit")
# Sequential assign_no_ct() calls implement this: per {sdtm.oak}
# documentation, later calls only fill values that are still missing,
# so (2) fills the records (1) left as NA and never overwrites.
ds <-
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "OTHERSP",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )

## ---- 5. Map qualifier and timing variables -----------------------------
ds <- ds %>%
  # --- DSDECOD: standardized disposition term, CT codelist C66727 ------
# Same two-source pattern as DSTERM. Values found in C66727 (via
# collected_value / synonyms) are recoded to the submission value
# (e.g. "Complete" -> "COMPLETED"). Values NOT in C66727 - the
# protocol milestone "Randomized" and OTHERSP milestones such as
# "Final Lab Visit" - are passed through upper-cased by {sdtm.oak}
# ("RANDOMIZED", "FINAL LAB VISIT"), which is exactly the DSDECOD
# convention for non-disposition events in SDTMIG. The console
# message listing unmapped terms is expected and reviewed below.
assign_ct(
  raw_dat = ds_raw,
  raw_var = "IT.DSDECOD",
  tgt_var = "DSDECOD",
  ct_spec = study_ct,
  ct_clst = "C66727",
  id_vars = oak_id_vars()
) %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "OTHERSP",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) %>%
  # --- VISIT / VISITNUM: from the raw visit label (INSTANCE) -----------
# The study CT ships study-specific "VISIT" and "VISITNUM" codelists
# that map collected visit labels (e.g. "Baseline") to the SDTM visit
# name ("BASELINE") and number ("3"). Unscheduled visits not listed
# in the CT pass through upper-cased (e.g. "UNSCHEDULED 8.2") and are
# handled in the post-processing step below.
assign_ct(
  raw_dat = ds_raw,
  raw_var = "INSTANCE",
  tgt_var = "VISIT",
  ct_spec = study_ct,
  ct_clst = "VISIT",
  id_vars = oak_id_vars()
) %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  ) %>%
  # --- DSDTC: collection date-time, ISO 8601 ---------------------------
# eCRF collects date (MM-DD-YYYY) and time (HH:MM) in two fields;
# assign_datetime() combines them (formats matched by position) and
# emits ISO 8601, omitting the time part when it was not collected.
assign_datetime(
  raw_dat = ds_raw,
  raw_var = c("DSDTCOL", "DSTMCOL"),
  raw_fmt = c("m-d-y", "H:M"),
  tgt_var = "DSDTC",
  id_vars = oak_id_vars()
) %>%
  # --- DSSTDTC: start date of the disposition event, ISO 8601 ----------
assign_datetime(
  raw_dat = ds_raw,
  raw_var = "IT.DSSTDAT",
  raw_fmt = "m-d-y",
  tgt_var = "DSSTDTC",
  id_vars = oak_id_vars()
)

## ---- 6. Identifiers and record-level derivations ------------------------
# STUDYID comes straight from the raw data; assert it is single-valued
# before using it so a bad input fails loudly rather than silently.
stopifnot(length(unique(ds_raw$STUDY)) == 1L)

# USUBJID: derived by joining to DM rather than string-pasting a study
# prefix. In DM, USUBJID is "01-<SITEID>-<SUBJID>" (e.g. "01-701-1015")
# while the raw PATNUM is "<SITEID>-<SUBJID>" ("701-1015"), so
# SITEID-SUBJID is the join key. Joining to DM guarantees the values
# match the rest of the SDTM package - which derive_study_day() below
# depends on (it merges to DM by USUBJID to pick up RFSTDTC).
subj_key <- dm %>%
  mutate(PATNUM_KEY = paste(SITEID, SUBJID, sep = "-")) %>%
  select(PATNUM_KEY, USUBJID)

ds <- ds %>%
  mutate(
    STUDYID = unique(ds_raw$STUDY),
    DOMAIN  = "DS"
  ) %>%
  left_join(subj_key, by = c("patient_number" = "PATNUM_KEY"))

# QC: every raw subject should resolve to a DM USUBJID.
n_unmatched <- sum(is.na(ds$USUBJID))
cat("\nRecords with USUBJID not found in DM:", n_unmatched, "\n")
if (n_unmatched > 0) {
  warning("Some ds_raw subjects could not be matched to DM - inspect PATNUM values.")
}

ds <- ds %>%
  mutate(
    # VISITNUM to numeric. Two cases:
    #  (1) CT-mapped values are numeric strings ("3", "13", "201", "3.1")
    #      -> direct as.numeric()
    #  (2) unscheduled visits absent from the CT pass through as e.g.
    #      "UNSCHEDULED 8.2" -> strip non-numeric characters to recover
    #      the embedded number (8.2), matching this study's convention
    #      that the unscheduled VISITNUM equals the decimal in the name
    #      (the CT's own "Unscheduled 3.1" -> 3.1 entry confirms this).
    # Any value that fits neither pattern becomes NA and is reported in
    # the QC block below.
    VISITNUM = coalesce(
      suppressWarnings(as.numeric(VISITNUM)),
      suppressWarnings(as.numeric(gsub("[^0-9.]", "", VISITNUM)))
    ),
    # DSCAT per SDTMIG and CDISC codelist C74558 (which is present in
    # study_ct with exactly these three submission values):
    #   PROTOCOL MILESTONE - randomization is a milestone, not a reason
    #                        for leaving the study
    #   DISPOSITION EVENT  - any standardized completion /
    #                        discontinuation reason from C66727
    #   OTHER EVENT        - remaining "other, specify" records such as
    #                        FINAL LAB VISIT
    # NOTE: a flat DSCAT = "PROTOCOL MILESTONE" for all records was
    # considered but would be incorrect for the majority of records
    # (e.g. ADVERSE EVENT, DEATH, COMPLETED are disposition events),
    # so the category is assigned conditionally.
    DSCAT = case_when(
      DSDECOD == "RANDOMIZED" ~ "PROTOCOL MILESTONE",
      DSDECOD %in% study_ct$term_value[study_ct$codelist_code == "C66727"] ~
        "DISPOSITION EVENT",
      TRUE ~ "OTHER EVENT"
    )
  )

## ---- 7. Study day and sequence number -----------------------------------
ds <- ds %>%
  # DSSTDY: study day of DSSTDTC relative to DM.RFSTDTC, per the SDTM
  # convention (no day 0: dates on/after the reference date give +1).
  # derive_study_day() merges to DM by USUBJID; records with a partial
  # or missing DSSTDTC/RFSTDTC yield NA by design.
  derive_study_day(
    sdtm_in       = .,
    dm_domain     = dm,
    tgdt          = "DSSTDTC",
    refdt         = "RFSTDTC",
    study_day_var = "DSSTDY"
  ) %>%
  # DSSEQ: unique sequence number within subject. derive_seq() sorts by
  # rec_vars and then numbers records within each subject, so ordering
  # by start date (ISO 8601 strings sort chronologically), visit number
  # and term gives a stable, reproducible sequence.
  derive_seq(
    tgt_dat  = .,
    tgt_var  = "DSSEQ",
    rec_vars = c("USUBJID", "DSSTDTC", "VISITNUM", "DSTERM")
  )

## ---- 8. Final dataset -----------------------------------------------------
ds_domain <- ds %>%
  mutate(
    # assign_datetime() returns a special iso8601 vector class; coerce
    # to plain character for a clean transport-file export.
    DSDTC   = as.character(DSDTC),
    DSSTDTC = as.character(DSSTDTC)
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT,
    VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY
  ) %>%
  arrange(USUBJID, DSSEQ)

## ---- 9. QC summaries (captured in the log as run evidence) ---------------
cat("\n----------------- QC SUMMARY -----------------\n")
cat("Output records :", nrow(ds_domain), "(input:", nrow(ds_raw), ")\n\n")

cat("DSCAT frequency:\n")
print(table(ds_domain$DSCAT, useNA = "ifany"))

cat("\nDSDECOD by DSCAT:\n")
print(table(ds_domain$DSDECOD, ds_domain$DSCAT, useNA = "ifany"))

cat("\nVISIT values with missing VISITNUM (expect none):\n")
print(table(ds_domain$VISIT[is.na(ds_domain$VISITNUM)]))

cat("\nMissing values per required variable:\n")
print(colSums(is.na(ds_domain)))

cat("\nDSSTDY range:",
    paste(range(ds_domain$DSSTDY, na.rm = TRUE), collapse = " to "), "\n")

# --- Explain the missing DSSTDY values (expected, not a defect) ---------
# derive_study_day() returns NA whenever the subject has no RFSTDTC in DM
# - i.e. the subject was screened but never randomized/dosed, so there is
# no reference start date to count study day from. SCREEN FAILURE is
# exactly this case. The check below prints the DSDECOD breakdown of the
# missing-DSSTDY records as evidence that all of them - and only them -
# are screen failures, confirming NA is the correct SDTM result here
# rather than a derivation error.
cat("\nDSDECOD breakdown of records with missing DSSTDY",
    "(expected: SCREEN FAILURE only, since those subjects have no",
    "DM.RFSTDTC to count study day from):\n")
print(table(ds_domain$DSDECOD[is.na(ds_domain$DSSTDY)], useNA = "ifany"))

# --- Optional cross-check against the reference SDTM DS domain ---------
# {pharmaversesdtm} ships a finalized, "gold standard" DS domain for this
# same study (CDISCPILOT01). Comparing our derived DSDECOD/DSCAT
# distributions against it is a useful independent sanity check - not a
# requirement of the assessment, so it is left commented out by default.
# Uncomment to run (requires the {waldo} package: install.packages("waldo")):
#
# reference_ds <- pharmaversesdtm::ds
# cat("\n--- Cross-check vs pharmaversesdtm::ds ---\n")
# cat("Our DSDECOD/DSCAT counts:\n")
# print(ds_domain %>% count(DSDECOD, DSCAT))
# cat("\nReference DSDECOD/DSCAT counts:\n")
# print(reference_ds %>% count(DSDECOD, DSCAT))
# print(waldo::compare(
#   ds_domain %>% count(DSDECOD, DSCAT),
#   reference_ds %>% count(DSDECOD, DSCAT)
# ))

## ---- 10. Export deliverables ----------------------------------------------
haven::write_xpt(ds_domain, "question_1_sdtm/ds.xpt", version = 5, name = "ds")
saveRDS(ds_domain, "question_1_sdtm/ds_domain.rds")
cat("\nWritten: question_1_sdtm/ds.xpt and question_1_sdtm/ds_domain.rds\n")

## ---- 11. Verification output (required by the assessment) ------------------
cat("\n----------------- str(ds_domain) -----------------\n")
str(ds_domain)

cat("\n----------------- head(ds_domain, 10) -----------------\n")
print(head(ds_domain, 10))

cat("\n----------------- sessionInfo -----------------\n")
print(sessionInfo())

cat("\nRun finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

## ---- 12. Close the log ------------------------------------------------------
sink(type = "message")
sink()
close(log_con)