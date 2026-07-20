# =====================================================================
# Program : verify_adsl_subject.R
# Purpose : Independent, multi-subject hand-verification of the ADSL
#           derivations produced by create_adsl.R.
#
#           This is INDEPENDENT double-programming: every derived value
#           is recomputed here from raw SDTM source data using base R /
#           minimal dplyr, WITHOUT calling the same {admiral} functions
#           used in create_adsl.R. If this script simply re-ran admiral,
#           it would only prove the code agrees with itself; recomputing
#           by hand is what actually tests correctness.
#
#           Two subjects are checked, chosen to cover different logic
#           branches:
#             - a Placebo subject whose LSTAVLDT comes from VS
#             - a non-Placebo (active-dose) subject whose LSTAVLDT comes
#               from the last-dose (ADSL/TRTEDT) path
#
#           For each variable it prints: the ADSL value, the value
#           recomputed here, and PASS/FAIL. A global tally drives a
#           single ALL-PASS / FAILURES-FOUND message at the very end.
#
# How run : From repo root (ideally a fresh R session), AFTER
#           create_adsl.R has produced question_2_adam/adsl.rds
# =====================================================================

library(admiral)   # convert_blanks_to_na() lives here - required when the
# script is run cold (not relying on create_adsl.R having
# loaded admiral earlier in the session)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)

# ---- Global pass/fail tally -----------------------------------------
# Incremented by check() below so the closing message can be conditional
# rather than always telling the reader to "review failures".
.fail_count <- 0L
.pass_count <- 0L

# Small helper to report and TALLY a check result.
check <- function(label, adsl_val, hand_val) {
  match <- (is.na(adsl_val) && is.na(hand_val)) ||
    (!is.na(adsl_val) && !is.na(hand_val) &&
       as.character(adsl_val) == as.character(hand_val))
  if (match) .pass_count <<- .pass_count + 1L else .fail_count <<- .fail_count + 1L
  cat(sprintf("%-11s | ADSL: %-22s | hand: %-22s | %s\n",
              label,
              ifelse(is.na(adsl_val), "NA", as.character(adsl_val)),
              ifelse(is.na(hand_val), "NA", as.character(hand_val)),
              ifelse(match, "PASS", "*** FAIL ***")))
  invisible(match)
}

# helper: parse only COMPLETE (10-char yyyy-mm-dd) dates, else NA
complete_date <- function(x) {
  d <- ifelse(!is.na(x) & nchar(x) >= 10, substr(x, 1, 10), NA_character_)
  as.Date(d)
}
fix_inf <- function(d) if (is.infinite(d)) as.Date(NA) else d

# ---- Load the ADSL we are checking (once) ---------------------------
adsl <- readRDS("question_2_adam/adsl.rds")

# ---- Load raw source data once (blanks -> NA, as in main script) ----
dm <- convert_blanks_to_na(pharmaversesdtm::dm)
ex <- convert_blanks_to_na(pharmaversesdtm::ex)
vs <- convert_blanks_to_na(pharmaversesdtm::vs)
ds <- convert_blanks_to_na(pharmaversesdtm::ds)
ae <- convert_blanks_to_na(pharmaversesdtm::ae)

# =====================================================================
# Per-subject verification routine
# =====================================================================
verify_subject <- function(SUBJ) {
  cat("\n=====================================================\n")
  cat("Verifying subject:", SUBJ, "\n")
  cat("=====================================================\n")
  
  a <- adsl %>% filter(USUBJID == SUBJ)
  stopifnot(nrow(a) == 1)
  
  dm_s <- dm %>% filter(USUBJID == SUBJ)
  ex_s <- ex %>% filter(USUBJID == SUBJ)
  vs_s <- vs %>% filter(USUBJID == SUBJ)
  ds_s <- ds %>% filter(USUBJID == SUBJ)
  ae_s <- ae %>% filter(USUBJID == SUBJ)
  
  cat("Raw record counts -> EX:", nrow(ex_s), " VS:", nrow(vs_s),
      " DS:", nrow(ds_s), " AE:", nrow(ae_s), "\n\n")
  
  # --- 1. AGEGR9 / AGEGR9N -------------------------------------------
  age <- dm_s$AGE
  hand_agegr9 <- if (is.na(age)) NA_character_ else if (age <= 18) "<18" else
    if (age > 18 & age < 50) "18 - 50" else ">50"
  hand_agegr9n <- if (is.na(age)) NA_real_ else if (age <= 18) 1 else
    if (age > 18 & age < 50) 2 else 3
  cat("--- AGEGR9 / AGEGR9N (DM.AGE =", age, ") ---\n")
  check("AGEGR9",  a$AGEGR9,  hand_agegr9)
  check("AGEGR9N", a$AGEGR9N, hand_agegr9n)
  
  # --- 2. ITTFL ------------------------------------------------------
  hand_ittfl <- if (!is.na(dm_s$ARM)) "Y" else "N"
  cat("--- ITTFL (DM.ARM =", ifelse(is.na(dm_s$ARM), "NA", dm_s$ARM), ") ---\n")
  check("ITTFL", a$ITTFL, hand_ittfl)
  
  # --- 3. TRTSDTM / TRTEDTM (date part) ------------------------------
  ex_valid_s <- ex_s %>%
    filter(EXDOSE > 0 | (EXDOSE == 0 & grepl("PLACEBO", EXTRT)))
  cat("--- Valid-dose EX (of", nrow(ex_s), "total): kept", nrow(ex_valid_s),
      "| EXDOSE:", paste(ex_s$EXDOSE, collapse = ","),
      "| EXTRT:", paste(unique(ex_s$EXTRT), collapse = ","), "---\n")
  
  hand_trtstart_dtc <- ex_valid_s %>% filter(!is.na(EXSTDTC)) %>%
    arrange(EXSTDTC, EXSEQ) %>% slice(1) %>% pull(EXSTDTC)
  hand_trtend_dtc <- ex_valid_s %>% filter(!is.na(EXENDTC)) %>%
    arrange(EXENDTC, EXSEQ) %>% slice(n()) %>% pull(EXENDTC)
  
  # guard: subject may have no valid end date
  hand_trtsdt <- if (length(hand_trtstart_dtc) == 0) as.Date(NA) else as.Date(hand_trtstart_dtc)
  hand_trtedt <- if (length(hand_trtend_dtc) == 0) as.Date(NA) else as.Date(hand_trtend_dtc)
  
  check("TRTSDT",     as.Date(a$TRTSDTM), hand_trtsdt)
  check("TRTEDT",     as.Date(a$TRTEDTM), hand_trtedt)
  check("TRTEDT=col", a$TRTEDT,           as.Date(a$TRTEDTM))
  
  # --- 4. LSTAVLDT / LALVDOM -----------------------------------------
  vs_dates <- vs_s %>%
    filter(!(is.na(VSSTRESN) & is.na(VSSTRESC))) %>%
    mutate(d = complete_date(VSDTC)) %>% pull(d)
  max_vs <- fix_inf(suppressWarnings(max(vs_dates, na.rm = TRUE)))
  max_ae <- fix_inf(suppressWarnings(max(complete_date(ae_s$AESTDTC), na.rm = TRUE)))
  max_ds <- fix_inf(suppressWarnings(max(complete_date(ds_s$DSSTDTC), na.rm = TRUE)))
  max_ex <- hand_trtedt
  
  cat("--- LSTAVLDT component maxima: VS", as.character(max_vs),
      "| AE", as.character(max_ae), "| DS", as.character(max_ds),
      "| EX-end", as.character(max_ex), "---\n")
  
  all_maxes <- c(max_vs, max_ae, max_ds, max_ex)
  hand_lstavldt <- fix_inf(suppressWarnings(max(all_maxes, na.rm = TRUE)))
  src_names <- c("VS", "AE", "DS", "ADSL")
  winner <- if (is.na(hand_lstavldt)) NA_character_ else
    src_names[which(all_maxes == hand_lstavldt)[1]]
  
  check("LSTAVLDT", a$LSTAVLDT, hand_lstavldt)
  # LALVDOM printed for context, not tallied: ties are resolved by
  # admiral's event ORDER, so a differing winner with a matching
  # LSTAVLDT is a tie, not an error.
  cat(sprintf("%-11s | ADSL: %-22s | hand: %-22s | %s\n",
              "LALVDOM(*)", a$LALVDOM, ifelse(is.na(winner), "NA", winner),
              "context only - see note"))
}

# =====================================================================
# Run verification on two subjects covering different branches
# =====================================================================
cat("=====================================================\n")
cat("ADSL INDEPENDENT HAND-VERIFICATION (2 subjects)\n")
cat("=====================================================\n")

# Subject A: Placebo, LSTAVLDT from VS (zero-dose placebo edge case)
verify_subject("01-701-1023")

# Subject B: non-Placebo active-dose subject, LSTAVLDT from last-dose
# (ADSL) path. Chosen from the head() output where LALVDOM == "ADSL":
# 01-701-1015 is Placebo(pick another) -> 01-701-1028 has LALVDOM ADSL
# and ACTARM "Xanomeline High Dose" (a real active, non-zero dose),
# exercising the EXDOSE > 0 branch of the valid-dose filter AND the
# last-dose LSTAVLDT path.
verify_subject("01-701-1028")

# =====================================================================
# Conditional closing summary (driven by the global tally)
# =====================================================================
cat("\n=====================================================\n")
cat(sprintf("Checks run: %d  |  Passed: %d  |  Failed: %d\n",
            .pass_count + .fail_count, .pass_count, .fail_count))
if (.fail_count == 0L) {
  cat("RESULT: ALL CHECKS PASSED across both subjects.\n")
} else {
  cat("RESULT: ", .fail_count,
      " check(s) FAILED - review the *** FAIL *** line(s) above.\n", sep = "")
}
cat("(LALVDOM lines are context-only and not counted: ties are resolved\n")
cat(" by admiral's event order, so a differing winner with a matching\n")
cat(" LSTAVLDT is a tie, not an error.)\n")
cat("=====================================================\n")