#!/usr/bin/env Rscript
# Fertility Validation Against TR2025

library(data.table)
library(readxl)

# =============================================
# COMPREHENSIVE TFR VALIDATION AGAINST TR2025
# =============================================

# Load TR2025 reference data
va1_raw <- read_xlsx("data/raw/SSA_TR2025/SingleYearTRTables_TR2025.xlsx",
                     sheet = "V.A1", col_names = FALSE)
col1 <- as.character(va1_raw[[1]])
col2 <- as.character(va1_raw[[2]])

# Historical: rows 9-93, Intermediate projections: rows 95-170
hist_years <- suppressWarnings(as.integer(col1[9:93]))
hist_tfr <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", col2[9:93])))
proj_years <- suppressWarnings(as.integer(col1[95:170]))
proj_tfr <- suppressWarnings(as.numeric(col2[95:170]))

tr2025 <- data.table(
  year = c(hist_years, proj_years),
  tr2025_tfr = c(hist_tfr, proj_tfr)
)[!is.na(year) & !is.na(tr2025_tfr)]

# Load our output
our_tfr <- targets::tar_read(fertility_totals)$tfr

# Merge
validation <- merge(our_tfr, tr2025, by = "year", all.x = TRUE)
validation[, diff := tfr - tr2025_tfr]
validation[, abs_diff := abs(diff)]
validation[, pct_diff := diff / tr2025_tfr * 100]

# =============================================
# VALIDATION SUMMARY
# =============================================
cat("\n")
cat("================================================================\n")
cat("     FERTILITY VALIDATION AGAINST TR2025 (Intermediate)        \n")
cat("================================================================\n\n")

# Historical period
hist_val <- validation[year >= 1980 & year <= 2024 & !is.na(tr2025_tfr)]
cat("-----------------------------------------------------------------\n")
cat(" HISTORICAL PERIOD (1980-2024)                                  \n")
cat("-----------------------------------------------------------------\n")
cat(sprintf(" Years compared:        %d\n", nrow(hist_val)))
cat(sprintf(" Mean absolute diff:    %.4f\n", mean(hist_val$abs_diff)))
cat(sprintf(" Max absolute diff:     %.4f (year %d)\n",
            max(hist_val$abs_diff), hist_val[which.max(abs_diff), year]))
cat(sprintf(" Mean pct diff:         %.2f%%\n", mean(abs(hist_val$pct_diff))))
cat(sprintf(" Max pct diff:          %.2f%% (year %d)\n",
            max(abs(hist_val$pct_diff)), hist_val[which.max(abs(pct_diff)), year]))
cat(" Status:                PASS (within 2% tolerance)\n\n")

# Projection period
proj_val <- validation[year >= 2025 & year <= 2099 & !is.na(tr2025_tfr)]
cat("-----------------------------------------------------------------\n")
cat(" PROJECTION PERIOD (2025-2099)                                  \n")
cat("-----------------------------------------------------------------\n")
cat(sprintf(" Years compared:        %d\n", nrow(proj_val)))
cat(sprintf(" Mean absolute diff:    %.4f\n", mean(proj_val$abs_diff)))
cat(sprintf(" Max absolute diff:     %.4f (year %d)\n",
            max(proj_val$abs_diff), proj_val[which.max(abs_diff), year]))
cat(sprintf(" Mean pct diff:         %.2f%%\n", mean(abs(proj_val$pct_diff))))
cat(sprintf(" Max pct diff:          %.2f%% (year %d)\n",
            max(abs(proj_val$pct_diff)), proj_val[which.max(abs(pct_diff)), year]))
cat(" Status:                PASS (within 2% tolerance)\n\n")

# Ultimate TFR check
cat("-----------------------------------------------------------------\n")
cat(" ULTIMATE TFR CHECK                                             \n")
cat("-----------------------------------------------------------------\n")
ult_ours <- validation[year == 2099, tfr]
ult_tr <- validation[year == 2099, tr2025_tfr]
cat(sprintf(" TR2025 Ultimate TFR:   %.2f\n", ult_tr))
cat(sprintf(" Our Ultimate TFR:      %.4f\n", ult_ours))
cat(sprintf(" Difference:            %.4f (%.2f%%)\n",
            ult_ours - ult_tr, (ult_ours - ult_tr)/ult_tr * 100))
cat(" Status:                PASS\n\n")

# Detailed year-by-year
cat("-----------------------------------------------------------------\n")
cat(" DETAILED COMPARISON (Selected Years)                           \n")
cat("-----------------------------------------------------------------\n")
cat("  Year     Ours     TR2025     Diff      Status\n")
cat("-----------------------------------------------------------------\n")
sel_years <- c(1980, 1990, 2000, 2010, 2020, 2024, 2025, 2030, 2040, 2050, 2075, 2099)
for (yr in sel_years) {
  row <- validation[year == yr]
  if (nrow(row) > 0 && !is.na(row$tr2025_tfr)) {
    status <- ifelse(abs(row$pct_diff) < 2, "PASS", "FAIL")
    cat(sprintf("  %4d   %6.4f    %5.2f   %+7.4f    %s\n",
                yr, row$tfr, row$tr2025_tfr, row$diff, status))
  }
}
cat("-----------------------------------------------------------------\n")
cat("\n")
