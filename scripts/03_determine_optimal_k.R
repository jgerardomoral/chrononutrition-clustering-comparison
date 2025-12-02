# ===================================================================
# 03_DETERMINE_OPTIMAL_K.R - Optimal Number of Clusters
# ===================================================================
# Uses multiple methods to determine optimal k for clustering
# Methods: Elbow, Silhouette, Gap statistic, NbClust
# ===================================================================

library(cluster)
library(factoextra)
library(NbClust)

cat("=== DETERMINING OPTIMAL K ===\n\n")

# Load prepared data
prepared <- readRDS("output/data/prepared_data.rds")
datos_scaled <- prepared$datos_scaled

cat("Working with n =", nrow(datos_scaled), "observations\n\n")

# -------------------------------------------------------------------
# 1. Elbow Method (WSS)
# -------------------------------------------------------------------

cat("1. Elbow method (Within-cluster Sum of Squares)...\n")

wss <- sapply(2:8, function(k) {
  set.seed(2025)
  kmeans(datos_scaled, centers = k, nstart = 50)$tot.withinss
})

cat("   WSS values:\n")
for (k in 2:8) {
  cat("   k =", k, ":", round(wss[k-1], 2), "\n")
}

# -------------------------------------------------------------------
# 2. Silhouette Method
# -------------------------------------------------------------------

cat("\n2. Silhouette method...\n")

sil_width <- sapply(2:8, function(k) {
  set.seed(2025)
  km <- kmeans(datos_scaled, centers = k, nstart = 50)
  ss <- silhouette(km$cluster, dist(datos_scaled))
  mean(ss[, 3])
})

cat("   Average silhouette width:\n")
for (k in 2:8) {
  cat("   k =", k, ":", round(sil_width[k-1], 3), "\n")
}

optimal_sil <- which.max(sil_width) + 1
cat("   Optimal k (silhouette):", optimal_sil, "\n")

# -------------------------------------------------------------------
# 3. Gap Statistic
# -------------------------------------------------------------------

cat("\n3. Gap statistic...\n")

set.seed(2025)
gap_stat <- clusGap(datos_scaled, FUN = kmeans, nstart = 50, K.max = 8, B = 50)

cat("   Gap values:\n")
for (k in 1:7) {
  cat("   k =", k+1, ":", round(gap_stat$Tab[k+1, "gap"], 3), "\n")
}

# -------------------------------------------------------------------
# 4. NbClust indices (individual calculations)
# -------------------------------------------------------------------

cat("\n4. NbClust indices...\n")

# Calculate each index separately to avoid vectorization issues
nb_results <- list()

set.seed(2025)
tryCatch({
  nb_sil <- NbClust(datos_scaled, min.nc = 2, max.nc = 6, method = "kmeans", index = "silhouette")
  nb_results$silhouette <- nb_sil$Best.nc[1]
  cat("   - Silhouette: best k =", nb_results$silhouette, "\n")
}, error = function(e) cat("   - Silhouette: calculation failed\n"))

set.seed(2025)
tryCatch({
  nb_dunn <- NbClust(datos_scaled, min.nc = 2, max.nc = 6, method = "kmeans", index = "dunn")
  nb_results$dunn <- nb_dunn$Best.nc[1]
  cat("   - Dunn: best k =", nb_results$dunn, "\n")
}, error = function(e) cat("   - Dunn: calculation failed\n"))

set.seed(2025)
tryCatch({
  nb_ch <- NbClust(datos_scaled, min.nc = 2, max.nc = 6, method = "kmeans", index = "ch")
  nb_results$ch <- nb_ch$Best.nc[1]
  cat("   - Calinski-Harabasz: best k =", nb_results$ch, "\n")
}, error = function(e) cat("   - Calinski-Harabasz: calculation failed\n"))

nb <- list(Best.nc = nb_results)

# -------------------------------------------------------------------
# 5. Save results and generate plots
# -------------------------------------------------------------------

cat("\n5. Saving results...\n")

if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)

# Save optimal k results
optimal_k_results <- list(
  wss = data.frame(k = 2:8, wss = wss),
  silhouette = data.frame(k = 2:8, avg_sil = sil_width),
  gap = gap_stat,
  nbclust = nb,
  recommended_k = 4  # Based on analysis in the paper
)

saveRDS(optimal_k_results, "output/data/optimal_k_results.rds")

# Note: Individual plots removed - see Figure S2 in supplementary materials
# for comprehensive k-selection visualization

cat("   ✓ Saved: output/optimal_k_results.rds\n")

cat("=== OPTIMAL K DETERMINATION COMPLETE ===\n")
cat("Recommended k = 4 (based on paper analysis)\n")
