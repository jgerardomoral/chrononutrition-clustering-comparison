# ===================================================================
# 05_CLUSTERING_HIERARCHICAL.R - Hierarchical Clustering (Ward, k=4)
# ===================================================================
# Performs hierarchical clustering using Ward's method
# Output: Cluster assignments, dendrogram, and validation metrics
# ===================================================================

library(cluster)
library(factoextra)

cat("=== HIERARCHICAL CLUSTERING (WARD) ===\n\n")

# Load prepared data
prepared <- readRDS("output/data/prepared_data.rds")
datos_scaled <- as.matrix(prepared$datos_scaled)
datos_clustering <- prepared$datos_clustering

cat("Working with n =", nrow(datos_scaled), "observations\n\n")

# -------------------------------------------------------------------
# 1. Calculate distance matrix and hierarchical clustering
# -------------------------------------------------------------------

cat("1. Computing distance matrix and hierarchical clustering...\n")

# Euclidean distance
dist_matrix <- dist(datos_scaled, method = "euclidean")

# Ward's method (minimizes within-cluster variance)
hc_result <- hclust(dist_matrix, method = "ward.D2")

cat("   Method: Ward.D2 (Ward's minimum variance)\n")
cat("   Distance: Euclidean\n\n")

# -------------------------------------------------------------------
# 2. Cut dendrogram at k=4
# -------------------------------------------------------------------

cat("2. Cutting dendrogram at k=4...\n")

clusters_hc <- cutree(hc_result, k = 4)

cat("   Cluster sizes:\n")
for (i in 1:4) {
  cat("   Cluster", i, ":", sum(clusters_hc == i), "participants\n")
}

# -------------------------------------------------------------------
# 3. Calculate validation metrics
# -------------------------------------------------------------------

cat("\n3. Validation metrics...\n")

# Silhouette
sil <- silhouette(clusters_hc, dist_matrix)
avg_sil <- mean(sil[, 3])
cat("   Average silhouette width:", round(avg_sil, 3), "\n")

# Cophenetic correlation (measures how well dendrogram preserves distances)
coph <- cor(dist_matrix, cophenetic(hc_result))
cat("   Cophenetic correlation:", round(coph, 3), "\n")

# -------------------------------------------------------------------
# 4. Cluster centers (original scale)
# -------------------------------------------------------------------

cat("\n4. Cluster centers (original scale):\n\n")

# Calculate cluster means in original scale
datos_raw <- prepared$datos_clustering[, prepared$vars_clustering]

centers_original <- matrix(NA, nrow = 4, ncol = length(prepared$vars_clustering))
colnames(centers_original) <- prepared$vars_clustering

for (i in 1:4) {
  cluster_data <- datos_raw[clusters_hc == i, ]
  centers_original[i, ] <- colMeans(cluster_data)
}

for (i in 1:4) {
  cat("   Cluster", i, ":\n")
  for (var in prepared$vars_clustering) {
    cat("     ", var, ":", round(centers_original[i, var], 2), "h\n")
  }
  cat("\n")
}

# -------------------------------------------------------------------
# 5. Save results
# -------------------------------------------------------------------

cat("5. Saving results...\n")

hierarchical_results <- list(
  model = hc_result,
  cluster = clusters_hc,
  centers_original = centers_original,
  silhouette = avg_sil,
  cophenetic_corr = coph,
  dist_matrix = dist_matrix
)

saveRDS(hierarchical_results, "output/data/hierarchical_results.rds")

# Add cluster assignments to data
datos_with_clusters <- datos_clustering
datos_with_clusters$cluster_hierarchical <- clusters_hc
saveRDS(datos_with_clusters, "output/data/datos_with_hierarchical.rds")

# Note: Individual cluster plots removed - see Figure S1 (PCA composite) in supplementary

cat("   Saved: output/hierarchical_results.rds\n")
cat("   Saved: output/datos_with_hierarchical.rds\n")

cat("=== HIERARCHICAL CLUSTERING COMPLETE ===\n")
