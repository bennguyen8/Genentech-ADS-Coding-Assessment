# export_adae.R
# One-off helper: export the AE dataset to CSV for the Python agent (Q4).
# The assessment names the input "adae.csv (pharmaversesdtm::ae)".
library(pharmaversesdtm)

# Create the output folder first - write.csv() cannot create a file inside
# a folder that does not exist yet.
dir.create("question_4_python", showWarnings = FALSE, recursive = TRUE)

ae <- pharmaversesdtm::ae
write.csv(ae, "question_4_python/adae.csv", row.names = FALSE)
cat("Wrote question_4_python/adae.csv:",
    nrow(ae), "rows x", ncol(ae), "cols\n")

# Print the column names so we can confirm which AE variables actually
# exist in raw `ae` (in particular whether AESOC / AESEV are present, or
# whether the body-system column is AEBODSYS) before running the agent.
cat("\nColumns in ae:\n")
print(names(ae))
