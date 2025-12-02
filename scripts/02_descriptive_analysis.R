# ===================================================================
# 02_DESCRIPTIVE_ANALYSIS.R - General Characteristics (Table 1)
# ===================================================================
# Generates descriptive statistics for the full sample
# Output: Table 1 with demographic, anthropometric, and chrono variables
# ===================================================================

library(dplyr)
library(readr)
library(knitr)

cat("=== DESCRIPTIVE ANALYSIS ===\n\n")

# Load data
datos <- read_csv("data/chrono_patterns_n459.csv", show_col_types = FALSE)

# -------------------------------------------------------------------
# 1. Demographic characteristics
# -------------------------------------------------------------------

cat("1. Demographic characteristics (n =", nrow(datos), ")\n\n")

# Age
cat("Age:\n")
cat("  Mean (SD):", round(mean(datos$age), 1), "(", round(sd(datos$age), 1), ")\n")
cat("  Range:", min(datos$age), "-", max(datos$age), "\n\n")

# Sex
cat("Sex:\n")
sex_table <- table(datos$sex)
cat("  Female:", sex_table["Female"], "(", round(100*sex_table["Female"]/nrow(datos), 1), "%)\n")
cat("  Male:", sex_table["Male"], "(", round(100*sex_table["Male"]/nrow(datos), 1), "%)\n\n")

# Academic program
cat("Academic Program:\n")
prog_table <- table(datos$academic_program)
for (prog in names(prog_table)) {
  cat("  ", prog, ":", prog_table[prog], "(", round(100*prog_table[prog]/nrow(datos), 1), "%)\n")
}

# -------------------------------------------------------------------
# 2. Anthropometric characteristics
# -------------------------------------------------------------------

cat("\n2. Anthropometric characteristics\n\n")

cat("BMI (kg/m²):\n")
cat("  Mean (SD):", round(mean(datos$bmi, na.rm=TRUE), 1),
    "(", round(sd(datos$bmi, na.rm=TRUE), 1), ")\n\n")

cat("Waist circumference (cm):\n")
cat("  Mean (SD):", round(mean(datos$waist_circumference, na.rm=TRUE), 1),
    "(", round(sd(datos$waist_circumference, na.rm=TRUE), 1), ")\n\n")

# -------------------------------------------------------------------
# 3. Chrononutrition characteristics
# -------------------------------------------------------------------

cat("3. Chrononutrition characteristics\n\n")

chrono_vars <- c("breakfast_time", "lunch_time", "dinner_time",
                 "eating_window", "overnight_fasting")

for (var in chrono_vars) {
  n_valid <- sum(!is.na(datos[[var]]))
  cat(var, "(n =", n_valid, "):\n")
  cat("  Mean (SD):", round(mean(datos[[var]], na.rm=TRUE), 2),
      "(", round(sd(datos[[var]], na.rm=TRUE), 2), ")\n")
}

# -------------------------------------------------------------------
# 4. Save summary table
# -------------------------------------------------------------------

cat("\n4. Saving summary table...\n")

if (!dir.exists("output/tables")) dir.create("output/tables", recursive = TRUE)

# Create summary data frame
summary_df <- data.frame(
  Variable = c("N", "Age (years)", "Female (%)", "BMI (kg/m²)",
               "Waist circumference (cm)", "Breakfast time (h)",
               "Lunch time (h)", "Dinner time (h)", "Eating window (h)"),
  Value = c(
    nrow(datos),
    paste0(round(mean(datos$age), 1), " (", round(sd(datos$age), 1), ")"),
    paste0(round(100*sex_table["Female"]/nrow(datos), 1), "%"),
    paste0(round(mean(datos$bmi, na.rm=TRUE), 1), " (", round(sd(datos$bmi, na.rm=TRUE), 1), ")"),
    paste0(round(mean(datos$waist_circumference, na.rm=TRUE), 1), " (", round(sd(datos$waist_circumference, na.rm=TRUE), 1), ")"),
    paste0(round(mean(datos$breakfast_time, na.rm=TRUE), 2), " (", round(sd(datos$breakfast_time, na.rm=TRUE), 2), ")"),
    paste0(round(mean(datos$lunch_time, na.rm=TRUE), 2), " (", round(sd(datos$lunch_time, na.rm=TRUE), 2), ")"),
    paste0(round(mean(datos$dinner_time, na.rm=TRUE), 2), " (", round(sd(datos$dinner_time, na.rm=TRUE), 2), ")"),
    paste0(round(mean(datos$eating_window, na.rm=TRUE), 2), " (", round(sd(datos$eating_window, na.rm=TRUE), 2), ")")
  )
)

write.csv(summary_df, "output/tables/table1_descriptive.csv", row.names = FALSE)
cat("   ✓ Saved: output/tables/table1_descriptive.csv\n\n")

cat("=== DESCRIPTIVE ANALYSIS COMPLETE ===\n")
