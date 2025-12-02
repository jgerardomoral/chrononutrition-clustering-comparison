# ===================================================================
# 08_COMPARE_METHODS.R - Compare Clustering Methods (Table 6)
# ===================================================================
# Compares all 4 clustering methods: K-means, Hierarchical, GMM, Spectral
# Output: Validation metrics table, concordance analysis, ARI matrix
# ===================================================================

library(mclust)      # For adjustedRandIndex
library(cluster)
library(fpc)         # For cluster.stats (Dunn index, Gamma, Entropy)
library(clusterSim)  # For Davies-Bouldin index

cat("=== COMPARING CLUSTERING METHODS ===\n\n")

# Load all clustering results
kmeans_res <- readRDS("output/data/kmeans_results.rds")
hier_res <- readRDS("output/data/hierarchical_results.rds")
gmm_res <- readRDS("output/data/gmm_results.rds")
spectral_res <- readRDS("output/data/spectral_results.rds")

# Load prepared data for additional metrics
prepared <- readRDS("output/data/prepared_data.rds")
datos_scaled <- as.matrix(prepared$datos_scaled)
dist_matrix <- dist(datos_scaled)

# -------------------------------------------------------------------
# 1. Compile cluster assignments
# -------------------------------------------------------------------

cat("1. Compiling cluster assignments...\n")

clusters_all <- data.frame(
  kmeans = kmeans_res$cluster,
  hierarchical = hier_res$cluster,
  gmm = gmm_res$cluster,
  spectral = spectral_res$cluster
)

cat("   Total observations:", nrow(clusters_all), "\n\n")

# -------------------------------------------------------------------
# 2. Calculate validation metrics for each method
# -------------------------------------------------------------------

cat("2. Calculating validation metrics...\n\n")

# Function to calculate comprehensive metrics (6 metrics as per manuscript Table 6)
# Reference: Ikotun et al. (2025) - multiple indices recommended for cluster validation
calc_metrics <- function(clusters, data_scaled, dist_mat) {
  stats <- cluster.stats(dist_mat, clusters)
  db <- index.DB(data_scaled, clusters)  # Davies-Bouldin from clusterSim

  list(
    silhouette = stats$avg.silwidth,   # Higher = better (cluster compactness)
    dunn = stats$dunn,                 # Higher = better (inter/intra ratio)
    db_index = db$DB,                  # Lower = better (Davies-Bouldin)
    ch_index = stats$ch,               # Higher = better (Calinski-Harabasz)
    gamma = stats$pearsongamma,        # Higher = better (Pearson Gamma statistic)
    entropy = stats$entropy            # Lower = better (assignment certainty)
  )
}

# Calculate for each method
metrics <- list()
metrics$kmeans <- calc_metrics(kmeans_res$cluster, datos_scaled, dist_matrix)
metrics$hierarchical <- calc_metrics(hier_res$cluster, datos_scaled, dist_matrix)
metrics$gmm <- calc_metrics(gmm_res$cluster, datos_scaled, dist_matrix)
metrics$spectral <- calc_metrics(spectral_res$cluster, datos_scaled, dist_matrix)

# Create comparison table with 6 validation metrics (matching manuscript Table 6)
comparison_table <- data.frame(
  Method = c("K-means", "Hierarchical (Ward)", "GMM", "Spectral"),
  Silhouette = round(c(
    metrics$kmeans$silhouette,
    metrics$hierarchical$silhouette,
    metrics$gmm$silhouette,
    metrics$spectral$silhouette
  ), 3),
  Dunn = round(c(
    metrics$kmeans$dunn,
    metrics$hierarchical$dunn,
    metrics$gmm$dunn,
    metrics$spectral$dunn
  ), 3),
  DB_Index = round(c(
    metrics$kmeans$db_index,
    metrics$hierarchical$db_index,
    metrics$gmm$db_index,
    metrics$spectral$db_index
  ), 3),
  CH_Index = round(c(
    metrics$kmeans$ch_index,
    metrics$hierarchical$ch_index,
    metrics$gmm$ch_index,
    metrics$spectral$ch_index
  ), 1),
  Gamma = round(c(
    metrics$kmeans$gamma,
    metrics$hierarchical$gamma,
    metrics$gmm$gamma,
    metrics$spectral$gamma
  ), 3),
  Entropy = round(c(
    metrics$kmeans$entropy,
    metrics$hierarchical$entropy,
    metrics$gmm$entropy,
    metrics$spectral$entropy
  ), 3)
)

cat("   Validation Metrics Summary:\n")
print(comparison_table, row.names = FALSE)

# -------------------------------------------------------------------
# 3. Calculate Adjusted Rand Index (ARI) between methods
# -------------------------------------------------------------------

cat("\n3. Calculating Adjusted Rand Index (concordance)...\n\n")

methods <- c("kmeans", "hierarchical", "gmm", "spectral")
ari_matrix <- matrix(NA, nrow = 4, ncol = 4,
                     dimnames = list(methods, methods))

for (i in 1:4) {
  for (j in 1:4) {
    ari_matrix[i, j] <- adjustedRandIndex(
      clusters_all[, i],
      clusters_all[, j]
    )
  }
}

cat("   Adjusted Rand Index Matrix:\n")
print(round(ari_matrix, 3))

# -------------------------------------------------------------------
# 4. Identify stable assignments (consensus)
# -------------------------------------------------------------------

cat("\n4. Analyzing agreement patterns...\n")

# Count how many methods agree on each observation
agreement_counts <- apply(clusters_all, 1, function(x) {
  max(table(x))  # Maximum agreement among methods
})

cat("   Agreement distribution:\n")
cat("   4/4 methods agree:", sum(agreement_counts == 4), "observations (",
    round(100*sum(agreement_counts == 4)/nrow(clusters_all), 1), "%)\n")
cat("   3/4 methods agree:", sum(agreement_counts == 3), "observations (",
    round(100*sum(agreement_counts == 3)/nrow(clusters_all), 1), "%)\n")
cat("   2/4 methods agree:", sum(agreement_counts == 2), "observations (",
    round(100*sum(agreement_counts == 2)/nrow(clusters_all), 1), "%)\n")

# -------------------------------------------------------------------
# 5. Cluster size comparison
# -------------------------------------------------------------------

cat("\n5. Cluster size comparison:\n\n")

size_table <- data.frame(
  Cluster = 1:4,
  Kmeans = as.numeric(table(factor(kmeans_res$cluster, levels = 1:4))),
  Hierarchical = as.numeric(table(factor(hier_res$cluster, levels = 1:4))),
  GMM = as.numeric(table(factor(gmm_res$cluster, levels = 1:4))),
  Spectral = as.numeric(table(factor(spectral_res$cluster, levels = 1:4)))
)

print(size_table, row.names = FALSE)

# -------------------------------------------------------------------
# 6. Save results
# -------------------------------------------------------------------

cat("\n6. Saving results...\n")

comparison_results <- list(
  clusters_all = clusters_all,
  metrics_table = comparison_table,
  ari_matrix = ari_matrix,
  agreement_counts = agreement_counts,
  size_table = size_table,
  detailed_metrics = metrics
)

saveRDS(comparison_results, "output/data/comparison_results.rds")

# Save as CSV for easy viewing
write.csv(comparison_table, "output/tables/table6_validation_metrics.csv",
          row.names = FALSE)
write.csv(ari_matrix, "output/tables/table6_ari_matrix.csv")

cat("   Saved: output/comparison_results.rds\n")
cat("   Saved: output/tables/table6_validation_metrics.csv\n")
cat("   Saved: output/tables/table6_ari_matrix.csv\n\n")

# -------------------------------------------------------------------
# 7. Generate comparison figure
# -------------------------------------------------------------------

# Note: Method comparison figures removed - see Figure S2 in supplementary
# for comprehensive validation metrics visualization
cat("\n")

cat("=== METHOD COMPARISON COMPLETE ===\n")
cat("\nBest performing method by metric:\n")
cat("  Silhouette (higher=better):", comparison_table$Method[which.max(comparison_table$Silhouette)], "\n")
cat("  Dunn (higher=better):", comparison_table$Method[which.max(comparison_table$Dunn)], "\n")
cat("  Davies-Bouldin (lower=better):", comparison_table$Method[which.min(comparison_table$DB_Index)], "\n")
cat("  Calinski-Harabasz (higher=better):", comparison_table$Method[which.max(comparison_table$CH_Index)], "\n")
cat("  Gamma (higher=better):", comparison_table$Method[which.max(comparison_table$Gamma)], "\n")
cat("  Entropy (lower=better):", comparison_table$Method[which.min(comparison_table$Entropy)], "\n")
