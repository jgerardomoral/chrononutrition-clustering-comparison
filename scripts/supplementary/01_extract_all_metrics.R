# =============================================================================
# Script: 01_extract_all_metrics.R
# Purpose: Extract and consolidate all clustering validation metrics
# Adapted for Zenodo repository - uses output from main pipeline scripts
# Original: manuscript/scripts/supplementary/01_extract_all_metrics.R
# =============================================================================

# -----------------------------------------------------------------------------
# SETUP
# -----------------------------------------------------------------------------

# Create output directories
dir.create("output/figures/supplementary", recursive = TRUE, showWarnings = FALSE)

cat("=== Extracting Clustering Validation Metrics (Zenodo) ===\n\n")

# Load required packages
library(cluster)
library(fpc)
library(clusterSim)  # For Davies-Bouldin index

# -----------------------------------------------------------------------------
# LOAD DATA FROM MAIN PIPELINE
# -----------------------------------------------------------------------------

cat("Loading data from main pipeline...\n")

prepared <- readRDS("output/data/prepared_data.rds")
datos_scaled <- as.matrix(prepared$datos_scaled)
dist_matrix <- dist(datos_scaled)

# -----------------------------------------------------------------------------
# CALCULATE METRICS FOR ALL K AND METHODS
# -----------------------------------------------------------------------------

cat("\nCalculating metrics for k=2 to k=5...\n")

calc_metrics <- function(clusters, data_scaled, dist_mat) {
  stats <- cluster.stats(dist_mat, clusters)

  # Calculate Davies-Bouldin index
  db_index <- tryCatch({
    clusterSim::index.DB(data_scaled, clusters)$DB
  }, error = function(e) {
    # Fallback calculation if clusterSim not available
    NA
  })

  list(
    silhouette = stats$avg.silwidth,
    dunn = stats$dunn,
    ch = stats$ch,
    gamma = stats$pearsongamma,
    entropy = stats$entropy,
    davies_bouldin = db_index
  )
}

# Initialize results
results <- data.frame(
  Method = character(),
  k = integer(),
  Silhouette = numeric(),
  Dunn = numeric(),
  CalinskiHarabasz = numeric(),
  Gamma = numeric(),
  Entropy = numeric(),
  DaviesBouldin = numeric(),
  stringsAsFactors = FALSE
)

methods <- c("K-means", "Hierarchical", "GMM", "Spectral")

for (k in 2:5) {
  cat("  k =", k, "...")

  # K-means
  set.seed(2025)
  km <- kmeans(datos_scaled, centers = k, nstart = 50, iter.max = 1000)
  km_metrics <- calc_metrics(km$cluster, datos_scaled, dist_matrix)

  # Hierarchical
  hc <- hclust(dist_matrix, method = "ward.D2")
  hc_clusters <- cutree(hc, k = k)
  hc_metrics <- calc_metrics(hc_clusters, datos_scaled, dist_matrix)

  # GMM
  library(mclust)
  set.seed(2025)
  gmm <- Mclust(datos_scaled, G = k, verbose = FALSE)
  gmm_metrics <- calc_metrics(gmm$classification, datos_scaled, dist_matrix)

  # Spectral
  library(kernlab)
  set.seed(2025)
  spec <- specc(datos_scaled, centers = k)
  spec_metrics <- calc_metrics(spec@.Data, datos_scaled, dist_matrix)

  # Add rows
  all_metrics <- list(km_metrics, hc_metrics, gmm_metrics, spec_metrics)

  for (i in 1:4) {
    m <- all_metrics[[i]]
    results <- rbind(results, data.frame(
      Method = methods[i],
      k = k,
      Silhouette = round(m$silhouette, 3),
      Dunn = round(m$dunn, 3),
      CalinskiHarabasz = round(m$ch, 1),
      Gamma = round(m$gamma, 3),
      Entropy = round(m$entropy, 3),
      DaviesBouldin = round(m$davies_bouldin, 3),
      stringsAsFactors = FALSE
    ))
  }

  cat(" done\n")
}

cat("\nMetrics summary:\n")
print(results)

# -----------------------------------------------------------------------------
# EXTRACT CLUSTER ASSIGNMENTS FOR k=4
# -----------------------------------------------------------------------------

cat("\n--- Extracting cluster assignments (k=4) ---\n")

# Load results from main pipeline (k=4)
kmeans_res <- readRDS("output/data/kmeans_results.rds")
hier_res <- readRDS("output/data/hierarchical_results.rds")
gmm_res <- readRDS("output/data/gmm_results.rds")
spectral_res <- readRDS("output/data/spectral_results.rds")

assignments_k4 <- data.frame(
  ID = 1:nrow(datos_scaled),
  `K-means` = kmeans_res$cluster,
  Hierarchical = hier_res$cluster,
  GMM = gmm_res$cluster,
  Spectral = spectral_res$cluster,
  check.names = FALSE
)

cat("Extracted assignments for 4 methods, n =", nrow(assignments_k4), "\n")

# -----------------------------------------------------------------------------
# CALCULATE ARI MATRIX
# -----------------------------------------------------------------------------

cat("\n--- Calculating ARI matrix ---\n")

ari_matrix <- matrix(NA, nrow = 4, ncol = 4,
                     dimnames = list(methods, methods))

for (i in 1:4) {
  for (j in 1:4) {
    ari_matrix[i, j] <- round(adjustedRandIndex(
      assignments_k4[[methods[i]]],
      assignments_k4[[methods[j]]]
    ), 3)
  }
}

cat("ARI Matrix:\n")
print(ari_matrix)

ari_values <- ari_matrix[lower.tri(ari_matrix)]
mean_ari <- round(mean(ari_values, na.rm = TRUE), 3)
cat("\nMean ARI (off-diagonal):", mean_ari, "\n")

# -----------------------------------------------------------------------------
# SAVE OUTPUTS
# -----------------------------------------------------------------------------

cat("\n--- Saving outputs ---\n")

# Save metrics as CSV and RDS
write.csv(results, "output/tables/tableS1_validation_metrics.csv", row.names = FALSE)
cat("Saved: output/tables/tableS1_validation_metrics.csv\n")

saveRDS(results, "output/data/tableS1_validation_metrics.rds")
cat("Saved: output/data/tableS1_validation_metrics.rds\n")

# Save ARI matrix
saveRDS(ari_matrix, "output/data/ari_matrix_k4.rds")
cat("Saved: output/data/ari_matrix_k4.rds\n")

# Save cluster assignments
saveRDS(assignments_k4, "output/data/cluster_assignments_k4.rds")
cat("Saved: output/data/cluster_assignments_k4.rds\n")

cat("\n=== Metrics extraction complete ===\n")
