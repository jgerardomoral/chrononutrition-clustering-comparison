# ===================================================================
# 00_SETUP.R - Package Installation and Environment Setup
# ===================================================================
# This script installs/restores all required packages using renv
# Run this ONCE before running any other scripts
# ===================================================================

cat("=== SETTING UP R ENVIRONMENT ===\n\n")

# Check if renv is installed
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("Installing renv package...\n")
  install.packages("renv")
}

# Check if renv.lock exists (restore from lock file)
if (file.exists("renv.lock")) {
  cat("Restoring packages from renv.lock...\n")
  renv::restore()
} else {
  # If no lock file, install required packages manually
  cat("No renv.lock found. Installing required packages...\n")

  packages <- c(
    # Data manipulation
    "dplyr",
    "tidyr",
    "readr",

    # Clustering
    "cluster",      # K-means, PAM, silhouette
    "mclust",       # Gaussian Mixture Models
    "kernlab",      # Spectral clustering
    "factoextra",   # Cluster visualization
    "NbClust",      # Optimal cluster determination
    "fpc",          # Cluster validation (Dunn, etc.)
    "clusterSim",   # Davies-Bouldin index

    # Visualization
    "ggplot2",
    "patchwork",
    "scales",
    "RColorBrewer",

    # Tables
    "knitr",
    "kableExtra"
  )

  # Install missing packages
  installed <- rownames(installed.packages())
  to_install <- packages[!packages %in% installed]

  if (length(to_install) > 0) {
    cat("Installing:", paste(to_install, collapse = ", "), "\n")
    install.packages(to_install)
  }

  # Create renv snapshot
  cat("\nCreating renv.lock for reproducibility...\n")
  renv::init()
  renv::snapshot()
}

cat("\n=== SETUP COMPLETE ===\n")
cat("You can now run the analysis scripts.\n")
