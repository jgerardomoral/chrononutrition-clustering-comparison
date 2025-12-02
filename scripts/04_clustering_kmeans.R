# ===================================================================
# 04_CLUSTERING_KMEANS.R - K-means Clustering (k=4)
# ===================================================================
# Performs K-means clustering on standardized meal timing data
# Output: Cluster assignments and validation metrics
# ===================================================================

library(cluster)
library(factoextra)

cat("=== K-MEANS CLUSTERING ===\n\n")

# Load prepared data
prepared <- readRDS("output/data/prepared_data.rds")
datos_scaled <- as.matrix(prepared$datos_scaled)
datos_clustering <- prepared$datos_clustering

cat("Working with n =", nrow(datos_scaled), "observations\n")
cat("Variables:", paste(prepared$vars_clustering, collapse = ", "), "\n\n")

# -------------------------------------------------------------------
# 1. Run K-means with k=4
# -------------------------------------------------------------------

cat("1. Running K-means (k=4)...\n")

set.seed(2025)
km_result <- kmeans(datos_scaled, centers = 4, nstart = 50, iter.max = 1000)

cat("   Cluster sizes:\n")
for (i in 1:4) {
  cat("   Cluster", i, ":", km_result$size[i], "participants\n")
}

# -------------------------------------------------------------------
# 2. Calculate validation metrics
# -------------------------------------------------------------------

cat("\n2. Validation metrics...\n")

# Silhouette
sil <- silhouette(km_result$cluster, dist(datos_scaled))
avg_sil <- mean(sil[, 3])
cat("   Average silhouette width:", round(avg_sil, 3), "\n")

# Within-cluster sum of squares
cat("   Total within-cluster SS:", round(km_result$tot.withinss, 2), "\n")
cat("   Between/Total SS ratio:", round(km_result$betweenss/km_result$totss, 3), "\n")

# -------------------------------------------------------------------
# 3. Cluster centers (original scale)
# -------------------------------------------------------------------

cat("\n3. Cluster centers (original scale):\n\n")

# De-standardize centers
centers_original <- t(apply(km_result$centers, 1, function(x) {
  x * prepared$scaling_params$scale + prepared$scaling_params$center
}))
colnames(centers_original) <- prepared$vars_clustering

for (i in 1:4) {
  cat("   Cluster", i, ":\n")
  for (var in prepared$vars_clustering) {
    cat("     ", var, ":", round(centers_original[i, var], 2), "h\n")
  }
  cat("\n")
}

# -------------------------------------------------------------------
# 4. Save results
# -------------------------------------------------------------------

cat("4. Saving results...\n")

kmeans_results <- list(
  model = km_result,
  cluster = km_result$cluster,
  centers_scaled = km_result$centers,
  centers_original = centers_original,
  silhouette = avg_sil,
  validation = list(
    tot_withinss = km_result$tot.withinss,
    betweenss = km_result$betweenss,
    totss = km_result$totss
  )
)

saveRDS(kmeans_results, "output/data/kmeans_results.rds")

# Add cluster assignments to data
datos_with_clusters <- datos_clustering
datos_with_clusters$cluster_kmeans <- km_result$cluster
saveRDS(datos_with_clusters, "output/data/datos_with_kmeans.rds")

# Note: Individual cluster plots removed - see Figure S1 (PCA composite) in supplementary

cat("   Saved: output/kmeans_results.rds\n")
cat("   Saved: output/datos_with_kmeans.rds\n")

cat("=== K-MEANS CLUSTERING COMPLETE ===\n")
