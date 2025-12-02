# ===================================================================
# 07_CLUSTERING_SPECTRAL.R - Spectral Clustering (k=4)
# ===================================================================
# Performs spectral clustering using RBF kernel
# Output: Cluster assignments and validation metrics
# ===================================================================

library(kernlab)
library(cluster)
library(factoextra)

cat("=== SPECTRAL CLUSTERING ===\n\n")

# Load prepared data
prepared <- readRDS("output/data/prepared_data.rds")
datos_scaled <- as.matrix(prepared$datos_scaled)
datos_clustering <- prepared$datos_clustering

cat("Working with n =", nrow(datos_scaled), "observations\n\n")

# -------------------------------------------------------------------
# 1. Run spectral clustering with k=4
# -------------------------------------------------------------------

cat("1. Running spectral clustering (k=4)...\n")

# Note: Spectral clustering uses seed=2025 (same as other methods for consistency)
set.seed(2025)
spec_result <- specc(datos_scaled, centers = 4, kernel = "rbfdot")

clusters_spec <- spec_result@.Data

cat("   Kernel: Radial Basis Function (RBF)\n\n")

cat("   Cluster sizes:\n")
for (i in 1:4) {
  cat("   Cluster", i, ":", sum(clusters_spec == i), "participants\n")
}

# -------------------------------------------------------------------
# 2. Calculate validation metrics
# -------------------------------------------------------------------

cat("\n2. Validation metrics...\n")

# Silhouette
sil <- silhouette(clusters_spec, dist(datos_scaled))
avg_sil <- mean(sil[, 3])
cat("   Average silhouette width:", round(avg_sil, 3), "\n")

# Within-cluster sum of squares (calculated manually)
wcss <- 0
for (i in 1:4) {
  cluster_data <- datos_scaled[clusters_spec == i, , drop = FALSE]
  center <- colMeans(cluster_data)
  wcss <- wcss + sum(apply(cluster_data, 1, function(x) sum((x - center)^2)))
}
cat("   Total within-cluster SS:", round(wcss, 2), "\n")

# -------------------------------------------------------------------
# 3. Cluster centers (original scale)
# -------------------------------------------------------------------

cat("\n3. Cluster centers (original scale):\n\n")

# Calculate cluster means in original scale
datos_raw <- prepared$datos_clustering[, prepared$vars_clustering]

centers_original <- matrix(NA, nrow = 4, ncol = length(prepared$vars_clustering))
colnames(centers_original) <- prepared$vars_clustering

for (i in 1:4) {
  cluster_data <- datos_raw[clusters_spec == i, ]
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
# 4. Evaluate different sigma values
# -------------------------------------------------------------------

cat("4. Evaluating different kernel widths (sigma)...\n")

sigmas <- c(0.5, 1, 2, 5)
sil_by_sigma <- numeric(length(sigmas))

for (j in seq_along(sigmas)) {
  set.seed(2025)
  tryCatch({
    spec_temp <- specc(datos_scaled, centers = 4,
                       kpar = list(sigma = sigmas[j]))
    sil_temp <- silhouette(spec_temp@.Data, dist(datos_scaled))
    sil_by_sigma[j] <- mean(sil_temp[, 3])
    cat("   sigma =", sigmas[j], ": silhouette =", round(sil_by_sigma[j], 3), "\n")
  }, error = function(e) {
    cat("   sigma =", sigmas[j], ": error\n")
    sil_by_sigma[j] <- NA
  })
}

# -------------------------------------------------------------------
# 5. Save results
# -------------------------------------------------------------------

cat("\n5. Saving results...\n")

spectral_results <- list(
  model = spec_result,
  cluster = clusters_spec,
  centers_original = centers_original,
  silhouette = avg_sil,
  wcss = wcss,
  sigma_comparison = data.frame(sigma = sigmas, silhouette = sil_by_sigma)
)

saveRDS(spectral_results, "output/data/spectral_results.rds")

# Add cluster assignments to data
datos_with_clusters <- datos_clustering
datos_with_clusters$cluster_spectral <- clusters_spec
saveRDS(datos_with_clusters, "output/data/datos_with_spectral.rds")

# Note: Individual cluster plots removed - see Figure S1 (PCA composite) in supplementary

cat("   Saved: output/spectral_results.rds\n")
cat("   Saved: output/datos_with_spectral.rds\n")

cat("=== SPECTRAL CLUSTERING COMPLETE ===\n")
