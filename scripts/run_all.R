# ===================================================================
# RUN_ALL.R - Master Script to Reproduce All Analyses
# ===================================================================
# Executes all analysis scripts in the correct order
# Run this script to fully reproduce the paper's results
# ===================================================================

cat("=========================================================\n")
cat("REPRODUCING: Traditional and Non-Traditional Clustering\n")
cat("             Techniques for Identifying Chrononutrition Patterns\n")
cat("=========================================================\n\n")

# Record start time
start_time <- Sys.time()

# Set working directory to scripts folder if not already there
if (!file.exists("data/chrono_patterns_n459.csv")) {
  if (file.exists("scripts/run_all.R")) {
    # We're in the root folder
    cat("Working from repository root directory\n\n")
  } else {
    stop("Please run this script from the repository root directory")
  }
}

# -------------------------------------------------------------------
# 0. Setup and package installation
# -------------------------------------------------------------------

cat("STEP 0: Checking packages...\n")
cat(rep("-", 50), "\n", sep = "")

# Check if key packages are available
required_packages <- c("dplyr", "readr", "cluster", "factoextra",
                       "mclust", "kernlab", "NbClust", "fpc",
                       "ggplot2", "tidyr", "patchwork", "clusterSim")

missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing) > 0) {
  cat("Missing packages:", paste(missing, collapse = ", "), "\n")
  cat("Running setup script to install packages...\n\n")
  source("scripts/00_setup.R")
} else {
  cat("All required packages are installed.\n\n")
}

# -------------------------------------------------------------------
# 1. Load and prepare data
# -------------------------------------------------------------------

cat("STEP 1: Loading and preparing data...\n")
cat(rep("-", 50), "\n", sep = "")
source("scripts/01_load_and_prepare.R")
cat("\n")

# -------------------------------------------------------------------
# 2. Descriptive analysis
# -------------------------------------------------------------------

cat("STEP 2: Descriptive analysis (Table 1)...\n")
cat(rep("-", 50), "\n", sep = "")
source("scripts/02_descriptive_analysis.R")
cat("\n")

# -------------------------------------------------------------------
# 3. Determine optimal k
# -------------------------------------------------------------------

cat("STEP 3: Determining optimal number of clusters...\n")
cat(rep("-", 50), "\n", sep = "")
source("scripts/03_determine_optimal_k.R")
cat("\n")

# -------------------------------------------------------------------
# 4. K-means clustering
# -------------------------------------------------------------------

cat("STEP 4: K-means clustering...\n")
cat(rep("-", 50), "\n", sep = "")
source("scripts/04_clustering_kmeans.R")
cat("\n")

# -------------------------------------------------------------------
# 5. Hierarchical clustering
# -------------------------------------------------------------------

cat("STEP 5: Hierarchical clustering (Ward)...\n")
cat(rep("-", 50), "\n", sep = "")
source("scripts/05_clustering_hierarchical.R")
cat("\n")

# -------------------------------------------------------------------
# 6. Gaussian Mixture Models
# -------------------------------------------------------------------

cat("STEP 6: Gaussian Mixture Model clustering...\n")
cat(rep("-", 50), "\n", sep = "")
source("scripts/06_clustering_gmm.R")
cat("\n")

# -------------------------------------------------------------------
# 7. Spectral clustering
# -------------------------------------------------------------------

cat("STEP 7: Spectral clustering...\n")
cat(rep("-", 50), "\n", sep = "")
source("scripts/07_clustering_spectral.R")
cat("\n")

# -------------------------------------------------------------------
# 8. Compare methods
# -------------------------------------------------------------------

cat("STEP 8: Comparing clustering methods (Table 6)...\n")
cat(rep("-", 50), "\n", sep = "")
source("scripts/08_compare_methods.R")
cat("\n")

# -------------------------------------------------------------------
# 9. Cluster characterization
# -------------------------------------------------------------------

cat("STEP 9: Cluster characterization (Tables 2-5)...\n")
cat(rep("-", 50), "\n", sep = "")
source("scripts/09_characterization.R")
cat("\n")

# -------------------------------------------------------------------
# 9b. Generate table summaries for Figure 1
# -------------------------------------------------------------------

cat("STEP 9b: Generating table summaries for Figure 1...\n")
cat(rep("-", 50), "\n", sep = "")
source("scripts/09b_generate_summaries.R")
cat("\n")

# -------------------------------------------------------------------
# 10. Generate figures
# -------------------------------------------------------------------

cat("STEP 10: Generating Figure 1 (temporal meal profiles)...\n")
cat(rep("-", 50), "\n", sep = "")
source("scripts/10_generate_figure.R")
cat("\n")

# -------------------------------------------------------------------
# 11. Generate supplementary figures
# -------------------------------------------------------------------

cat("STEP 11: Generating supplementary figures (S1-S6)...\n")
cat(rep("-", 50), "\n", sep = "")
source("scripts/11_generate_supplementary.R")
cat("\n")

# -------------------------------------------------------------------
# Summary
# -------------------------------------------------------------------

end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "mins")

cat("=========================================================\n")
cat("                    ANALYSIS COMPLETE\n")
cat("=========================================================\n\n")

cat("Total execution time:", round(as.numeric(elapsed), 2), "minutes\n\n")

cat("Output files generated:\n\n")

cat("Data files (output/data/):\n")
cat("  - prepared_data.rds\n")
cat("  - optimal_k_results.rds\n")
cat("  - kmeans_results.rds\n")
cat("  - hierarchical_results.rds\n")
cat("  - gmm_results.rds\n")
cat("  - spectral_results.rds\n")
cat("  - comparison_results.rds\n")
cat("  - characterization_results.rds\n")
cat("  - table_summaries.rds\n\n")

cat("Tables (output/tables/):\n")
cat("  - table1_descriptive.csv\n")
cat("  - table6_validation_metrics.csv\n")
cat("  - table6_ari_matrix.csv\n")
cat("  - cluster_summary.csv\n\n")

cat("Figures (output/figures/):\n")
cat("  - figure1_temporal_profiles.png\n\n")

cat("Supplementary Figures (output/figures/supplementary/):\n")
cat("  - figS1_pca_composite_16panel.png\n")
cat("  - figS2_k_selection_metrics.png\n")
cat("  - figS3_gap_statistic.png\n")
cat("  - figS4_silhouette_composite_16panel.png\n")
cat("  - figS5_participant_flow.png\n")
cat("  - figS6_bootstrap_stability.png\n\n")

cat("=========================================================\n")
cat("Session info:\n")
cat("=========================================================\n")
sessionInfo()
