# ============================================================================
# Figure S4: Silhouette Composite (16 panels: 4 k-values x 4 methods)
# Purpose: Show cluster quality via silhouette analysis across all k values
# Adapted for Zenodo repository - uses data from 04_figure_S3_pca_composite.R
# Original: manuscript/scripts/supplementary/06_figure_S4_silhouette_composite.R
# ============================================================================

cat("=== Generating Figure S4: Silhouette Composite (16 panels) ===\n")

# Load required packages
library(cluster)
library(mclust)
library(kernlab)

# Create output directories
dir.create("output/figures/supplementary", recursive = TRUE, showWarnings = FALSE)

# Load prepared data
prepared <- readRDS("output/data/prepared_data.rds")
datos_scaled <- as.matrix(prepared$datos_scaled)

# Calculate distance matrix
dist_matrix <- dist(datos_scaled)

# Define methods and k values
methods <- c("kmeans", "hierarchical", "gmm", "spectral")
method_labels <- c("K-means", "Hierarchical", "GMM", "Spectral")
k_values <- c(2, 3, 4, 5)

# Function to get clusters
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

# Store silhouette results for summary
all_silhouettes <- list()

# Create figure (4 rows x 4 columns = 16 panels)
# Size: 12x14 inches (reduced for better page fit)
png("output/figures/supplementary/figS4_silhouette_composite_16panel.png",
    width = 12, height = 14, units = "in", res = 300)

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

    cat("  Computing silhouette for", m, "k =", k, "...\n")

    # Get cluster assignments
    cluster <- get_clusters(datos_scaled, k, m)
    colors <- get_colors(k)

    # Calculate silhouette
    sil <- silhouette(cluster, dist_matrix)
    sil_avg <- mean(sil[, 3])

    # Store for summary
    all_silhouettes[[paste(k, m, sep = "_")]] <- list(
      sil = sil,
      avg = sil_avg,
      k = k,
      method = m_label
    )

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

    # Sort by cluster and then by silhouette width
    sil_sorted <- sil[order(sil[, 1], -sil[, 3]), ]

    # Create silhouette barplot
    n <- nrow(sil_sorted)

    # Plot background
    plot(NULL, xlim = c(min(-0.2, min(sil_sorted[, 3])), 1),
         ylim = c(0, n + 1),
         xlab = "", ylab = "",
         las = 1, cex.axis = 0.9,
         bty = if (is_selected) "o" else "l",
         xaxt = "n", yaxt = "n")

    # Add x-axis
    axis(1, at = seq(-0.2, 1, 0.2), cex.axis = 0.8)

    # Add zero line
    abline(v = 0, lty = 2, col = "gray50")

    # Add silhouette bars
    cluster_sorted <- sil_sorted[, 1]
    sil_width_sorted <- sil_sorted[, 3]

    # Draw bars colored by cluster
    for (i in 1:n) {
      rect(0, i - 0.4, sil_width_sorted[i], i + 0.4,
           col = colors[cluster_sorted[i]],
           border = NA)
    }

    # Add cluster separators and labels
    cluster_boundaries <- c(0, cumsum(table(sil_sorted[, 1])))
    for (c in 1:k) {
      y_mid <- (cluster_boundaries[c] + cluster_boundaries[c + 1]) / 2
      # Add cluster separator line
      if (c < k) {
        abline(h = cluster_boundaries[c + 1] + 0.5, col = "gray30", lwd = 1)
      }
    }

    # Add box with thicker line for k=4
    if (is_selected) {
      box(lwd = 3, col = "#2E7D32")  # Dark green for emphasis
    }

    # Add axis labels only on edges
    if (k_idx == 4) {
      mtext("Silhouette Width", side = 1, line = 2.5, cex = 0.7)
    }
    if (m_idx == 1) {
      mtext("Observations", side = 2, line = 2.5, cex = 0.7)
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

    # Add average silhouette width annotation
    avg_text <- sprintf("Avg: %.3f", sil_avg)
    text(0.75, n * 0.95, avg_text, cex = 0.9, font = 2,
         col = if (sil_avg > 0.4) "#2E7D32" else if (sil_avg > 0.25) "#FF8C00" else "#DC143C")

    # Add cluster sizes in corner
    cluster_n <- table(cluster)
    legend_text <- sprintf("C%d: %d", 1:k, cluster_n)

    legend("bottomright",
           legend = legend_text,
           fill = colors,
           border = NA,
           bty = "n",
           cex = 0.6,
           ncol = if (k > 3) 2 else 1)

    # Reset background
    par(bg = "white")
  }
}

dev.off()

cat("Saved: output/figures/supplementary/figS4_silhouette_composite_16panel.png\n")

# Save data for reproducibility
saveRDS(list(
  all_silhouettes = all_silhouettes,
  k_values = k_values,
  methods = methods,
  method_labels = method_labels
), "output/data/silhouette_composite_data.rds")

# Print summary table
cat("\n=== Silhouette Summary by k and Method ===\n")
cat(sprintf("%-12s %4s %10s %10s %10s %10s\n",
            "Method", "k", "Avg Sil", "Min", "Max", "Neg%"))
cat(paste(rep("-", 60), collapse = ""), "\n")

for (k in k_values) {
  for (m_idx in seq_along(methods)) {
    m <- methods[m_idx]
    key <- paste(k, m, sep = "_")
    sil_data <- all_silhouettes[[key]]
    sil <- sil_data$sil

    avg_sil <- mean(sil[, 3])
    min_sil <- min(sil[, 3])
    max_sil <- max(sil[, 3])
    neg_pct <- 100 * sum(sil[, 3] < 0) / nrow(sil)

    row_marker <- if (k == 4) "*" else " "
    cat(sprintf("%s%-11s %4d %10.3f %10.3f %10.3f %9.1f%%\n",
                row_marker, method_labels[m_idx], k, avg_sil, min_sil, max_sil, neg_pct))
  }
  cat("\n")
}

cat("* k=4 is the selected solution\n")

# Summary by k
cat("\n=== Average Silhouette by k (across methods) ===\n")
for (k in k_values) {
  avg_by_k <- mean(sapply(methods, function(m) {
    all_silhouettes[[paste(k, m, sep = "_")]]$avg
  }))
  marker <- if (k == 4) " (SELECTED)" else ""
  cat(sprintf("k=%d: %.3f%s\n", k, avg_by_k, marker))
}

cat("\n=== Figure S4 complete ===\n")
