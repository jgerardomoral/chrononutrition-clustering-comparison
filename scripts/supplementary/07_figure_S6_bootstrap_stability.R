# ============================================================================
# Figure S6: Bootstrap Stability Analysis
# Purpose: Show cluster stability across bootstrap resamples for k=2-5
# Adapted for Zenodo repository - uses data from pipeline
# Original: manuscript/scripts/supplementary/07_figure_S5_bootstrap_stability.R
# ============================================================================

cat("=== Generating Figure S6: Bootstrap Stability Analysis ===\n")

# Create output directories
dir.create("output/figures/supplementary", recursive = TRUE, showWarnings = FALSE)

# Load prepared data
prepared <- readRDS("output/data/prepared_data.rds")
datos_scaled <- as.matrix(prepared$datos_scaled)

# Load k=4 results for comparison baseline
kmeans_res <- readRDS("output/data/kmeans_results.rds")

# Set parameters
n_boot <- 500
k_values <- 2:5
set.seed(123)

# Function to calculate ARI (base R implementation)
calculate_ari <- function(cluster1, cluster2) {
  # Create contingency table
  n <- length(cluster1)
  tab <- table(cluster1, cluster2)

  # Calculate sums
  sum_row <- rowSums(tab)
  sum_col <- colSums(tab)

  # Calculate index
  sum_nij2 <- sum(tab * (tab - 1)) / 2
  sum_ai2 <- sum(sum_row * (sum_row - 1)) / 2
  sum_bj2 <- sum(sum_col * (sum_col - 1)) / 2
  n_comb <- n * (n - 1) / 2

  expected <- (sum_ai2 * sum_bj2) / n_comb
  max_index <- (sum_ai2 + sum_bj2) / 2

  if (max_index == expected) return(1)

  ari <- (sum_nij2 - expected) / (max_index - expected)
  return(ari)
}

# Function to perform k-means and return cluster assignment
perform_kmeans <- function(data, k, nstart = 25) {
  result <- kmeans(data, centers = k, nstart = nstart)
  return(result$cluster)
}

cat("Running bootstrap stability analysis (", n_boot, " resamples)...\n")

# Store results
stability_results <- data.frame(
  k = integer(),
  boot_id = integer(),
  ari = numeric()
)

# For each k value
for (k in k_values) {
  cat("  k =", k, "...\n")

  # Get original clustering
  set.seed(2025)
  original_clusters <- kmeans(datos_scaled, centers = k, nstart = 50, iter.max = 1000)$cluster

  # Bootstrap resamples
  for (b in 1:n_boot) {
    # Sample with replacement
    boot_indices <- sample(1:nrow(datos_scaled), replace = TRUE)
    boot_data <- datos_scaled[boot_indices, ]

    # Perform clustering on bootstrap sample
    boot_clusters <- perform_kmeans(boot_data, k)

    # Calculate ARI between original and bootstrap (only for overlapping samples)
    # Use unique indices to avoid duplicate comparisons
    unique_indices <- unique(boot_indices)
    orig_sub <- original_clusters[unique_indices]

    # Map bootstrap clusters back to original sample order
    boot_map <- rep(NA, nrow(datos_scaled))
    for (i in seq_along(boot_indices)) {
      idx <- boot_indices[i]
      if (is.na(boot_map[idx])) {
        boot_map[idx] <- boot_clusters[i]
      }
    }
    boot_sub <- boot_map[unique_indices]

    # Calculate ARI
    ari <- calculate_ari(orig_sub, boot_sub)

    stability_results <- rbind(stability_results, data.frame(
      k = k,
      boot_id = b,
      ari = ari
    ))
  }
}

# Calculate summary statistics per k
stability_summary <- aggregate(ari ~ k, data = stability_results,
                                FUN = function(x) c(mean = mean(x),
                                                    sd = sd(x),
                                                    q25 = quantile(x, 0.25),
                                                    q75 = quantile(x, 0.75)))
stability_summary <- do.call(data.frame, stability_summary)
names(stability_summary) <- c("k", "mean", "sd", "q25", "q75")

# Create figure
png("output/figures/supplementary/figS6_bootstrap_stability.png",
    width = 10, height = 5, units = "in", res = 300)

par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

# Color palette
colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")

# Panel A: Boxplot of ARI by k
boxplot(ari ~ k, data = stability_results,
        col = colors,
        main = "A) Bootstrap Stability by k",
        xlab = "Number of clusters (k)",
        ylab = "Adjusted Rand Index (ARI)",
        las = 1,
        ylim = c(0, 1),
        cex.main = 1.2,
        cex.lab = 1.1)

# Add mean points
points(1:4, stability_summary$mean, pch = 18, col = "white", cex = 2)
points(1:4, stability_summary$mean, pch = 18, col = "black", cex = 1.5)

# Add reference lines
abline(h = 0.8, lty = 2, col = "gray40")
abline(h = 0.6, lty = 3, col = "gray60")

# Add legend
legend("bottomleft",
       legend = c("Excellent (>0.8)", "Good (>0.6)"),
       lty = c(2, 3),
       col = c("gray40", "gray60"),
       bty = "n",
       cex = 0.9)

# Panel B: Mean ARI with error bars
plot(1:4, stability_summary$mean,
     type = "b",
     pch = 19,
     col = colors,
     cex = 2,
     main = "B) Mean Stability Index",
     xlab = "Number of clusters (k)",
     ylab = "Mean ARI +/- SD",
     las = 1,
     ylim = c(0, 1.1),
     xaxt = "n",
     cex.main = 1.2,
     cex.lab = 1.1)

axis(1, at = 1:4, labels = k_values)

# Add error bars
arrows(1:4, stability_summary$mean - stability_summary$sd,
       1:4, stability_summary$mean + stability_summary$sd,
       angle = 90, code = 3, length = 0.1, col = colors, lwd = 2)

# Highlight k=4
points(3, stability_summary$mean[3], pch = 19, col = "#984EA3", cex = 3)
points(3, stability_summary$mean[3], pch = 1, col = "black", cex = 3.5, lwd = 2)

# Add reference lines
abline(h = 0.8, lty = 2, col = "gray40")
abline(h = 0.6, lty = 3, col = "gray60")

# Add stability interpretation
text(1:4, stability_summary$mean + stability_summary$sd + 0.08,
     sprintf("%.2f", stability_summary$mean),
     col = colors, cex = 0.9, font = 2)

dev.off()

cat("Saved: output/figures/supplementary/figS6_bootstrap_stability.png\n")

# Save data for reproducibility
saveRDS(list(
  stability_results = stability_results,
  stability_summary = stability_summary,
  n_bootstrap = n_boot,
  k_values = k_values
), "output/data/bootstrap_stability_k2_to_k5.rds")

# Print summary
cat("\n=== Bootstrap Stability Summary ===\n")
for (i in 1:nrow(stability_summary)) {
  cat(sprintf("k=%d: ARI = %.3f +/- %.3f (IQR: %.3f - %.3f)\n",
              stability_summary$k[i],
              stability_summary$mean[i],
              stability_summary$sd[i],
              stability_summary$q25[i],
              stability_summary$q75[i]))
}

cat("\n=== Figure S6 complete ===\n")
