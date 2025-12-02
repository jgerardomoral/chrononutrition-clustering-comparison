# ===================================================================
# 11_GENERATE_SUPPLEMENTARY.R - Generate All Supplementary Figures
# ===================================================================
# Master script that sources all supplementary figure scripts
# Creates Figures S1-S6 for the supplementary materials
# ===================================================================

cat("=== GENERATING ALL SUPPLEMENTARY FIGURES ===\n\n")

# Create output directories
dir.create("output/figures/supplementary", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# Track script execution
scripts_run <- character()
scripts_failed <- character()

# Helper function to run a script with error handling
run_script <- function(script_path, description) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("Running:", description, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  result <- tryCatch({
    source(script_path, local = FALSE)
    TRUE
  }, error = function(e) {
    cat("\n!!! ERROR in", script_path, ":\n")
    cat(conditionMessage(e), "\n\n")
    FALSE
  })

  return(result)
}

# -------------------------------------------------------------------
# 1. Extract All Metrics (required by Figure S2)
# -------------------------------------------------------------------

if (run_script("scripts/supplementary/01_extract_all_metrics.R",
               "01: Extract validation metrics for k=2-5")) {
  scripts_run <- c(scripts_run, "01_extract_all_metrics.R")
} else {
  scripts_failed <- c(scripts_failed, "01_extract_all_metrics.R")
}

# -------------------------------------------------------------------
# 2. Figure S1: PCA Composite (16 panels) - First cited in manuscript
# -------------------------------------------------------------------

if (run_script("scripts/supplementary/04_figure_S1_pca_composite.R",
               "04: Figure S1 - PCA Biplot Composite (16 panels)")) {
  scripts_run <- c(scripts_run, "04_figure_S1_pca_composite.R")
} else {
  scripts_failed <- c(scripts_failed, "04_figure_S1_pca_composite.R")
}

# -------------------------------------------------------------------
# 3. Figure S2: K-Selection Metrics
# -------------------------------------------------------------------

if (run_script("scripts/supplementary/02_figure_S2_k_selection.R",
               "02: Figure S2 - K-Selection Metrics (6 panels)")) {
  scripts_run <- c(scripts_run, "02_figure_S2_k_selection.R")
} else {
  scripts_failed <- c(scripts_failed, "02_figure_S2_k_selection.R")
}

# -------------------------------------------------------------------
# 4. Figure S3: Gap Statistic
# -------------------------------------------------------------------

if (run_script("scripts/supplementary/03_figure_S3_gap_statistic.R",
               "03: Figure S3 - Gap Statistic with First-SE Rule")) {
  scripts_run <- c(scripts_run, "03_figure_S3_gap_statistic.R")
} else {
  scripts_failed <- c(scripts_failed, "03_figure_S3_gap_statistic.R")
}

# -------------------------------------------------------------------
# 5. Figure S4: Silhouette Composite (16 panels)
# -------------------------------------------------------------------

if (run_script("scripts/supplementary/05_figure_S4_silhouette_composite.R",
               "05: Figure S4 - Silhouette Composite (16 panels)")) {
  scripts_run <- c(scripts_run, "05_figure_S4_silhouette_composite.R")
} else {
  scripts_failed <- c(scripts_failed, "05_figure_S4_silhouette_composite.R")
}

# -------------------------------------------------------------------
# 6. Figure S5: Participant Flow (Method Concordance)
# -------------------------------------------------------------------

if (run_script("scripts/supplementary/06_figure_S5_participant_flow.R",
               "06: Figure S5 - Participant Flow Between Methods")) {
  scripts_run <- c(scripts_run, "06_figure_S5_participant_flow.R")
} else {
  scripts_failed <- c(scripts_failed, "06_figure_S5_participant_flow.R")
}

# -------------------------------------------------------------------
# 7. Figure S6: Bootstrap Stability
# -------------------------------------------------------------------

if (run_script("scripts/supplementary/07_figure_S6_bootstrap_stability.R",
               "07: Figure S6 - Bootstrap Stability Analysis")) {
  scripts_run <- c(scripts_run, "07_figure_S6_bootstrap_stability.R")
} else {
  scripts_failed <- c(scripts_failed, "07_figure_S6_bootstrap_stability.R")
}

# -------------------------------------------------------------------
# Summary
# -------------------------------------------------------------------

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("=== SUPPLEMENTARY FIGURE GENERATION SUMMARY ===\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

cat("Scripts executed successfully:", length(scripts_run), "\n")
for (s in scripts_run) {
  cat("  [OK]", s, "\n")
}

if (length(scripts_failed) > 0) {
  cat("\nScripts failed:", length(scripts_failed), "\n")
  for (s in scripts_failed) {
    cat("  [FAILED]", s, "\n")
  }
}

# List generated figures
cat("\n=== Generated Supplementary Figures ===\n")
if (dir.exists("output/figures/supplementary")) {
  figures <- list.files("output/figures/supplementary", pattern = "\\.(png|pdf)$")
  if (length(figures) > 0) {
    for (fig in figures) {
      cat("  -", fig, "\n")
    }
  } else {
    cat("  No figures found!\n")
  }
}

# Save execution log
execution_log <- list(
  timestamp = Sys.time(),
  scripts_run = scripts_run,
  scripts_failed = scripts_failed,
  figures_generated = if (dir.exists("output/figures/supplementary")) {
    list.files("output/figures/supplementary", pattern = "\\.(png|pdf)$")
  } else {
    character()
  }
)

saveRDS(execution_log, "output/data/supplementary_execution_log.rds")

cat("\n=== SUPPLEMENTARY FIGURE GENERATION COMPLETE ===\n")

# Return success status
if (length(scripts_failed) == 0) {
  cat("\nAll scripts completed successfully!\n")
} else {
  cat("\nSome scripts failed. Check logs above for details.\n")
}
