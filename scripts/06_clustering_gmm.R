# ===================================================================
# 06_CLUSTERING_GMM.R - Gaussian Mixture Models (k=4)
# ===================================================================
# Performs model-based clustering using Gaussian Mixture Models
# Output: Cluster assignments, probabilities, and validation metrics
# ===================================================================

library(mclust)
library(cluster)

cat("=== GAUSSIAN MIXTURE MODEL CLUSTERING ===\n\n")

# Load prepared data
prepared <- readRDS("output/data/prepared_data.rds")
datos_scaled <- as.matrix(prepared$datos_scaled)
datos_clustering <- prepared$datos_clustering

cat("Working with n =", nrow(datos_scaled), "observations\n\n")

# -------------------------------------------------------------------
# 1. Fit GMM with k=4
# -------------------------------------------------------------------

cat("1. Fitting Gaussian Mixture Model (k=4)...\n")

set.seed(2025)
gmm_result <- Mclust(datos_scaled, G = 4)

cat("   Model selected:", gmm_result$modelName, "\n")
cat("   Log-likelihood:", round(gmm_result$loglik, 2), "\n")
cat("   BIC:", round(gmm_result$bic, 2), "\n\n")

cat("   Cluster sizes:\n")
for (i in 1:4) {
  cat("   Cluster", i, ":", sum(gmm_result$classification == i), "participants\n")
}

# -------------------------------------------------------------------
# 2. Calculate validation metrics
# -------------------------------------------------------------------

cat("\n2. Validation metrics...\n")

# Silhouette
sil <- silhouette(gmm_result$classification, dist(datos_scaled))
avg_sil <- mean(sil[, 3])
cat("   Average silhouette width:", round(avg_sil, 3), "\n")

# Entropy-based uncertainty
entropy <- mean(-rowSums(gmm_result$z * log(gmm_result$z + 1e-10)))
cat("   Average classification entropy:", round(entropy, 3), "\n")

# Average posterior probability (confidence)
max_probs <- apply(gmm_result$z, 1, max)
avg_prob <- mean(max_probs)
cat("   Average classification probability:", round(avg_prob, 3), "\n")

# -------------------------------------------------------------------
# 3. Cluster centers (original scale)
# -------------------------------------------------------------------

cat("\n3. Cluster centers (original scale):\n\n")

# De-standardize means
centers_original <- t(apply(gmm_result$parameters$mean, 2, function(x) {
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
# 4. Compare different covariance models (BIC)
# -------------------------------------------------------------------

cat("4. Model comparison (BIC for different covariance structures)...\n")

set.seed(2025)
gmm_all <- Mclust(datos_scaled, G = 4, modelNames = c("EII", "VII", "EEI", "VVI", "EEE", "VVV"))

cat("   Model BIC values:\n")
bic_vals <- gmm_all$BIC[1, ]  # Only 1 row when G=4 is specified
bic_vals <- bic_vals[!is.na(bic_vals)]
for (model in names(bic_vals)) {
  cat("   ", model, ":", round(bic_vals[model], 2), "\n")
}

# -------------------------------------------------------------------
# 5. Save results
# -------------------------------------------------------------------

cat("\n5. Saving results...\n")

gmm_results <- list(
  model = gmm_result,
  cluster = gmm_result$classification,
  probabilities = gmm_result$z,
  centers_original = centers_original,
  silhouette = avg_sil,
  entropy = entropy,
  avg_probability = avg_prob,
  bic = gmm_result$bic,
  model_name = gmm_result$modelName
)

saveRDS(gmm_results, "output/data/gmm_results.rds")

# Add cluster assignments to data
datos_with_clusters <- datos_clustering
datos_with_clusters$cluster_gmm <- gmm_result$classification
saveRDS(datos_with_clusters, "output/data/datos_with_gmm.rds")

# Note: Individual cluster plots removed - see Figure S1 (PCA composite) in supplementary

cat("   Saved: output/gmm_results.rds\n")
cat("   Saved: output/datos_with_gmm.rds\n")

cat("=== GMM CLUSTERING COMPLETE ===\n")
