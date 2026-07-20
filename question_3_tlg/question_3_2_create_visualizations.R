# =====================================================================
# Program    : 02_create_visualizations.R
# Study      : CDISCPILOT01
# Purpose    : Two adverse-event visualizations for the TLG question:
#              (1) AE severity distribution by treatment arm (stacked bar,
#                  event counts)
#              (2) Top 10 adverse events by pooled subject incidence, with
#                  95% Clopper-Pearson confidence intervals
#              ADS Programmer Coding Assessment - Question 3, Task 2.
# Author     : Ben Nguyen
#
# Inputs     : pharmaverseadam::adae - analysis AE dataset
#              pharmaverseadam::adsl - subject-level dataset (denominators)
# Outputs    : question_3_tlg/ae_severity_by_treatment.png
#              question_3_tlg/top10_ae_incidence.png
#              question_3_tlg/log_viz.txt
#
# How to run : From the repository ROOT.
#
# Notes      : - PNG export uses ggplot2::ggsave() with the {ragg} PNG
#                device (device = ragg::agg_png). This is pure-R graphics
#                (no headless browser), so unlike gt/webshot2 it works on
#                Posit Cloud, whose sandbox blocks Chrome.
#              - Plot 1 counts AE RECORDS (events), matching the sample's
#                "Count of AEs" y-axis (confirmed with candidate).
#              - Plot 2 ranks the top 10 terms by POOLED subject incidence
#                across all treatment arms (distinct subjects with the
#                term / total treated subjects), matching the sample's
#                "n = ... subjects" framing. CIs are exact Clopper-Pearson
#                via base R binom.test(x, n)$conf.int - the exact method,
#                no extra package needed.
#              - Denominator population: treated subjects (the 3 real arms;
#                Screen Failure excluded, as in Task 1). The actual pooled
#                N is printed in the log; the sample image shows n = 225,
#                whatever our data yields is reported rather than forced.
# =====================================================================

## ---- 0. Logging ------------------------------------------------------
dir.create("question_3_tlg", showWarnings = FALSE, recursive = TRUE)
log_con <- file("question_3_tlg/log_viz.txt", open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message")

cat("=====================================================\n")
cat("Q3 Task 2 - AE visualizations using {ggplot2}\n")
cat("Run started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=====================================================\n\n")

## ---- 1. Packages -----------------------------------------------------
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(pharmaverseadam)

# ragg provides a high-quality PNG device that renders without a browser.
have_ragg <- requireNamespace("ragg", quietly = TRUE)
png_device <- if (have_ragg) ragg::agg_png else grDevices::png

## ---- 2. Read and prepare data ----------------------------------------
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Restrict to the three real treatment arms (exclude Screen Failure), for
# consistency with Task 1. Fix the arm order so plots read Placebo -> High
# -> Low.
ARMS_KEEP <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")

adae <- adae %>%
  filter(TRTEMFL == "Y", ACTARM %in% ARMS_KEEP) %>%
  mutate(ACTARM = factor(ACTARM, levels = ARMS_KEEP))

adsl <- adsl %>%
  filter(ACTARM %in% ARMS_KEEP) %>%
  mutate(ACTARM = factor(ACTARM, levels = ARMS_KEEP))

cat("TEAE records (3 arms):", nrow(adae),
    " | treated subjects:", nrow(adsl), "\n\n")

## =====================================================================
## PLOT 1 - AE severity distribution by treatment arm (event counts)
## =====================================================================
# AESEV has values MILD / MODERATE / SEVERE. Order them so the stack and
# legend read from least to most severe. Count AE RECORDS per arm x
# severity (events, per the "Count of AEs" y-axis).
sev_levels <- c("MILD", "MODERATE", "SEVERE")

sev_data <- adae %>%
  filter(!is.na(AESEV)) %>%
  mutate(AESEV = factor(AESEV, levels = sev_levels)) %>%
  count(ACTARM, AESEV, name = "n_events")

cat("Plot 1 - AE event counts by arm x severity:\n")
print(sev_data)
cat("\nAny AESEV values outside MILD/MODERATE/SEVERE:",
    any(!adae$AESEV[!is.na(adae$AESEV)] %in% sev_levels), "\n\n")

p1 <- ggplot(sev_data, aes(x = ACTARM, y = n_events, fill = AESEV)) +
  geom_col() +                                  # stacked bars (default)
  scale_fill_manual(
    values = c(MILD = "#4E79A7", MODERATE = "#F28E2B", SEVERE = "#E15759"),
    name   = "Severity/Intensity"
  ) +
  labs(
    title = "AE Severity Distribution by Treatment",
    x     = "Treatment Arm",
    y     = "Count of AEs"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "right")

ggsave(
  filename = "question_3_tlg/ae_severity_by_treatment.png",
  plot = p1, device = png_device,
  width = 8, height = 5, units = "in", dpi = 150, bg = "white"
)
cat("Written: ae_severity_by_treatment.png\n\n")

## =====================================================================
## PLOT 2 - Top 10 AEs by pooled subject incidence, 95% Clopper-Pearson CI
## =====================================================================
# Denominator choice (investigated and documented):
#   N = 254 = all TREATED subjects (the at-risk population).
# The assessment's sample image shows "n = 225 subjects". We investigated:
# distinct subjects with any treatment-emergent AE in the 3 arms = 217,
# and with any AE at all is larger - so the sample's 225 is a
# "subjects-with-AEs" count, not the treated population. We deliberately
# use 254 instead, because an incidence rate ("% of patients with this
# AE") must divide by everyone AT RISK: a treated subject who had zero
# AEs is an informative negative and belongs in the denominator. Dropping
# those AE-free subjects (using 217 or 225) shrinks the denominator and
# INFLATES every rate - worst for the most frequent terms (e.g. the top
# term's incidence is overstated by ~3-4 percentage points). 254 is the
# statistically correct incidence denominator; the sample's 225 is
# reported here in a comment rather than matched.
N_total <- dplyr::n_distinct(adsl$USUBJID)
cat("Plot 2 - pooled subject denominator N =", N_total,
    "(treated/at-risk population; sample image shows 225)\n")

# Numerator per term: distinct subjects with >= 1 TEAE of that term
# (subject incidence, NOT event count - a subject with the AE twice
# counts once).
term_counts <- adae %>%
  distinct(AETERM, USUBJID) %>%
  count(AETERM, name = "n_subj") %>%
  arrange(desc(n_subj)) %>%
  slice_head(n = 10)

# Exact Clopper-Pearson CI for each term via base R binom.test(). Mapping
# row-by-row keeps it transparent and dependency-free.
ci <- lapply(seq_len(nrow(term_counts)), function(i) {
  bt <- binom.test(term_counts$n_subj[i], N_total)
  data.frame(
    pct   = 100 * term_counts$n_subj[i] / N_total,
    lower = 100 * bt$conf.int[1],
    upper = 100 * bt$conf.int[2]
  )
})
plot2_data <- bind_cols(term_counts, do.call(rbind, ci)) %>%
  # order factor by incidence so the plot sorts high -> low top to bottom
  mutate(AETERM = factor(AETERM, levels = rev(AETERM)))

cat("Plot 2 - top 10 terms with Clopper-Pearson 95% CIs:\n")
print(plot2_data %>%
        mutate(across(c(pct, lower, upper), ~round(., 1))))
cat("\n")

p2 <- ggplot(plot2_data, aes(x = pct, y = AETERM)) +
  geom_point(size = 2.5) +
  # geom_errorbar with orientation = "y" is the non-deprecated replacement
  # for geom_errorbarh() (removed-in-favour-of as of ggplot2 4.0.0).
  geom_errorbar(aes(xmin = lower, xmax = upper),
                orientation = "y", width = 0.3) +
  # Explicit breaks every 5% so readers can read incidence values off the
  # axis (default breaks were only at 10 and 20, making the dots hard to
  # place precisely).
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
  labs(
    title    = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", N_total, " subjects; 95% Clopper-Pearson CIs"),
    x        = "Percentage of Patients (%)",
    y        = NULL
  ) +
  theme_bw(base_size = 12)

ggsave(
  filename = "question_3_tlg/top10_ae_incidence.png",
  plot = p2, device = png_device,
  width = 8, height = 5, units = "in", dpi = 150, bg = "white"
)
cat("Written: top10_ae_incidence.png\n\n")

## ---- 3. QC / verification ----------------------------------------------
cat("----------------- QC SUMMARY -----------------\n")

# Independent recompute of the single most frequent term's CI, so the log
# carries a hand-checkable example of the Clopper-Pearson derivation.
top_term <- as.character(plot2_data$AETERM[which.max(plot2_data$pct)])
x_top <- term_counts$n_subj[term_counts$AETERM == top_term]
bt_top <- binom.test(x_top, N_total)
cat(sprintf("Most frequent term: %s | %d/%d subjects = %.1f%% (95%% CI %.1f-%.1f%%)\n",
            top_term, x_top, N_total, 100 * x_top / N_total,
            100 * bt_top$conf.int[1], 100 * bt_top$conf.int[2]))

cat("\nragg PNG device used:", have_ragg,
    "(FALSE = fell back to grDevices::png)\n")

cat("\n----------------- sessionInfo -----------------\n")
print(sessionInfo())

cat("\nRun finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

## ---- 4. Close the log --------------------------------------------------
sink(type = "message")
sink()
close(log_con)
