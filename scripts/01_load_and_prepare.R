# ===================================================================
# 01_LOAD_AND_PREPARE.R - Load and Prepare Data for Clustering
# ===================================================================
# Loads the anonymized dataset and prepares it for clustering analysis
# Output: datos_clustering (n=388 complete cases)
# ===================================================================

library(dplyr)
library(readr)

cat("=== LOADING AND PREPARING DATA ===\n\n")

# Create output directories
dir.create("output/data", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures/supplementary", recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------
# 1. Load anonymized data
# -------------------------------------------------------------------

cat("1. Loading data...\n")
datos <- read_csv("data/chrono_patterns_n459.csv", show_col_types = FALSE)
cat("   - Total participants: n =", nrow(datos), "\n\n")

# -------------------------------------------------------------------
# 2. Select clustering variables
# -------------------------------------------------------------------

cat("2. Selecting clustering variables...\n")

# Primary clustering variables (3 meal times)
vars_clustering <- c("breakfast_time", "lunch_time", "dinner_time")

cat("   - Variables:", paste(vars_clustering, collapse = ", "), "\n\n")

# -------------------------------------------------------------------
# 3. Filter to complete cases
# -------------------------------------------------------------------

cat("3. Filtering to complete cases...\n")

datos_clustering <- datos %>%
  dplyr::select(id, all_of(vars_clustering)) %>%
  na.omit()

cat("   - Complete cases: n =", nrow(datos_clustering), "\n")
cat("   - Excluded (meal skipping):", nrow(datos) - nrow(datos_clustering), "\n\n")

# -------------------------------------------------------------------
# 4. Standardize variables (z-score)
# -------------------------------------------------------------------

cat("4. Standardizing variables (z-score)...\n")

# Extract raw values for later use
datos_raw <- datos_clustering[, vars_clustering]

# Standardize
datos_scaled <- scale(datos_raw)

# Store scaling parameters
scaling_params <- list(
  center = attr(datos_scaled, "scaled:center"),
  scale = attr(datos_scaled, "scaled:scale")
)

cat("   - Scaling parameters:\n")
for (var in vars_clustering) {
  cat("     ", var, ": mean =", round(scaling_params$center[var], 2),
      ", sd =", round(scaling_params$scale[var], 2), "\n")
}

# -------------------------------------------------------------------
# 5. Summary statistics
# -------------------------------------------------------------------

cat("\n5. Summary of clustering variables (original scale):\n\n")

summary_stats <- datos_raw %>%
  summarise(across(everything(), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  )))

for (var in vars_clustering) {
  cat("   ", var, ":\n")
  cat("     Mean:", round(summary_stats[[paste0(var, "_mean")]], 2), "h\n")
  cat("     SD:  ", round(summary_stats[[paste0(var, "_sd")]], 2), "h\n")
  cat("     Range:", round(summary_stats[[paste0(var, "_min")]], 2), "-",
      round(summary_stats[[paste0(var, "_max")]], 2), "h\n\n")
}

# -------------------------------------------------------------------
# 6. Save prepared data
# -------------------------------------------------------------------

cat("6. Saving prepared data...\n")

# Create output directory if needed
if (!dir.exists("output")) dir.create("output")

# Save as RDS for use in subsequent scripts
saveRDS(
  list(
    datos_full = datos,
    datos_clustering = datos_clustering,
    datos_scaled = as.data.frame(datos_scaled),
    scaling_params = scaling_params,
    vars_clustering = vars_clustering
  ),
  file = "output/data/prepared_data.rds"
)

cat("   ✓ Saved: output/data/prepared_data.rds\n\n")

cat("=== DATA PREPARATION COMPLETE ===\n")
cat("Ready for clustering analysis with n =", nrow(datos_clustering), "participants\n")
