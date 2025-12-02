# ============================================================================
# Figure S1: PCA Biplot Composite (16 panels: 4 k-values x 4 methods)
# Purpose: Visualize cluster separation across all k values and methods
# Adapted for Zenodo repository - generates data from scratch using pipeline results
# Original: manuscript/scripts/supplementary/04_figure_S1_pca_composite.R
# ============================================================================

cat("=== Generating Figure S1: PCA Biplot Composite (16 panels) ===\n")

# Create output directories
dir.create("output/figures/supplementary", recursive = TRUE, showWarnings = FALSE)

# Load packages
library(cluster)
library(mclust)
library(kernlab)

# Load prepared data
prepared <- readRDS("output/data/prepared_data.rds")
datos_scaled <- as.matrix(prepared$datos_scaled)

# Perform PCA
pca_result <- prcomp(datos_scaled, scale. = FALSE)  # Already scaled

# Calculate variance explained
var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100

# Store PCA coordinates
pca_coords <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2]
)

# Define methods and k values
methods <- c("kmeans", "hierarchical", "gmm", "spectral")
method_labels <- c("K-means", "Hierarchical", "GMM", "Spectral")
k_values <- c(2, 3, 4, 5)

# Calculate distance matrix for hierarchical
dist_matrix <- dist(datos_scaled)

# Function to get clusters for all methods and k values
get_clusters <- function(datos, k, method) {
  set.seed(2025)

  if (method == "kmeans") {
    result <- kmeans(datos, centers = k, nstart = 50, iter.max = 1000)
    return(result$cluster)
  } else if (method == "hierarchical") {
    hc <- hclust(dist(datos), method = "ward.D2")
    return(cutree(hc, k = k))
  } else if (method == "gmm") {
    gmm <- Mclust(datos, G = k, verbose = FALSE)
    return(gmm$classification)
  } else if (method == "spectral") {
    spec <- specc(datos, centers = k)
    return(spec@.Data)
  }
}

# Color palettes for different k values
get_colors <- function(k) {
  base_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")
  return(base_colors[1:k])
}

# Store all cluster assignments
all_clusters <- list()
for (k in k_values) {
  all_clusters[[paste0("k", k)]] <- list()
  for (m in methods) {
    cat("  Computing", m, "k =", k, "...\n")
    all_clusters[[paste0("k", k)]][[m]] <- get_clusters(datos_scaled, k, m)
  }
}

# Create figure (4 rows x 4 columns = 16 panels)
# Size: 14x16 inches for good legibility
png("output/figures/supplementary/figS1_pca_composite_16panel.png",
    width = 14, height = 16, units = "in", res = 300)

# Set up layout with space for row labels
layout(matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE),
       heights = c(0.5, rep(4, 4)),
       widths = rep(4, 4))

# Add column headers (method names)
par(mar = c(0, 0, 0, 0))
for (m in method_labels) {
  plot.new()
  text(0.5, 0.5, m, cex = 1.8, font = 2)
}

# Generate panels
for (k_idx in seq_along(k_values)) {
  k <- k_values[k_idx]
  k_name <- paste0("k", k)
  is_selected <- (k == 4)  # k=4 is the selected solution

  for (m_idx in seq_along(methods)) {
    m <- methods[m_idx]
    m_label <- method_labels[m_idx]

    # Get cluster assignments
    cluster <- all_clusters[[k_name]][[m]]
    colors <- get_colors(k)

    # Set margins - wider for left column (row labels)
    if (m_idx == 1) {
      par(mar = c(4, 6, 3, 1))
    } else {
      par(mar = c(4, 4, 3, 1))
    }

    # Set background for k=4 row (selected)
    if (is_selected) {
      par(bg = "#F5F5F5")
    } else {
      par(bg = "white")
    }

    # Plot points
    plot(pca_coords$PC1, pca_coords$PC2,
         col = colors[cluster],
         pch = 19,
         cex = 0.8,
         xlab = "",
         ylab = "",
         las = 1,
         cex.axis = 0.9,
         bty = if (is_selected) "o" else "l",
         lwd = if (is_selected) 3 else 1)

    # Add box with thicker line for k=4
    if (is_selected) {
      box(lwd = 3, col = "#2E7D32")  # Dark green for emphasis
    }

    # Add axis labels only on edges
    if (k_idx == 4) {
      mtext(sprintf("PC1 (%.1f%%)", var_explained[1]),
            side = 1, line = 2.5, cex = 0.7)
    }
    if (m_idx == 1) {
      mtext(sprintf("PC2 (%.1f%%)", var_explained[2]),
            side = 2, line = 3.5, cex = 0.7)
    }

    # Add row label (k value) on left side
    if (m_idx == 1) {
      row_label <- if (is_selected) {
        paste0("k=", k, " (SELECTED)")
      } else {
        paste0("k=", k)
      }
      mtext(row_label, side = 2, line = 5, cex = 0.9,
            font = if (is_selected) 2 else 1,
            col = if (is_selected) "#2E7D32" else "black")
    }

    # Add cluster centers
    centers <- aggregate(cbind(PC1, PC2) ~ cluster,
                         data = data.frame(PC1 = pca_coords$PC1,
                                           PC2 = pca_coords$PC2,
                                           cluster = cluster),
                         FUN = mean)

    points(centers$PC1, centers$PC2,
           pch = 4, cex = 2, lwd = 2, col = "black")

    # Add loading vectors (only on first row and scaled)
    if (k_idx == 1) {
      loadings <- pca_result$rotation[, 1:2]
      scale_factor <- 2.5

      arrows(0, 0,
             loadings[, 1] * scale_factor,
             loadings[, 2] * scale_factor,
             col = "gray40", lwd = 1.5, length = 0.08)

      var_names <- c("BF", "L", "D")  # Abbreviated: Breakfast, Lunch, Dinner
      text(loadings[, 1] * scale_factor * 1.15,
           loadings[, 2] * scale_factor * 1.15,
           labels = var_names,
           col = "gray30", cex = 0.7, font = 2)
    }

    # Add cluster legend (compact)
    cluster_n <- table(cluster)
    legend_text <- sprintf("C%d:%d", 1:k, cluster_n)

    legend("topright",
           legend = legend_text,
           col = colors,
           pch = 19,
           pt.cex = 0.8,
           bty = "n",
           cex = 0.6,
           ncol = if (k > 3) 2 else 1)

    # Add grid lines
    abline(h = 0, v = 0, lty = 3, col = "gray70")

    # Reset background
    par(bg = "white")
  }
}

dev.off()

cat("Saved: output/figures/supplementary/figS1_pca_composite_16panel.png\n")

# Save data for reproducibility
saveRDS(list(
  pca_result = pca_result,
  var_explained = var_explained,
  pca_coords = pca_coords,
  loadings = pca_result$rotation,
  cluster_assignments = all_clusters
), "output/data/pca_composite_data.rds")

# Print summary
cat("\n=== PCA Summary ===\n")
cat(sprintf("PC1 explains %.1f%% of variance\n", var_explained[1]))
cat(sprintf("PC2 explains %.1f%% of variance\n", var_explained[2]))
cat(sprintf("Total (PC1+PC2): %.1f%%\n", sum(var_explained[1:2])))

cat("\n=== Cluster sizes by k and method ===\n")
for (k in k_values) {
  k_name <- paste0("k", k)
  cat(sprintf("\nk=%d:\n", k))
  for (m_idx in seq_along(methods)) {
    m <- methods[m_idx]
    cluster <- all_clusters[[k_name]][[m]]
    sizes <- paste(table(cluster), collapse=", ")
    cat(sprintf("  %s: n=%s\n", method_labels[m_idx], sizes))
  }
}

cat("\n=== PCA Loadings ===\n")
rownames(pca_result$rotation) <- c("Breakfast", "Lunch", "Dinner")
print(round(pca_result$rotation[, 1:2], 3))

cat("\n=== Figure S1 complete ===\n")
