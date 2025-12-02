# ============================================================================
# Figure S5: Participant Flow Diagram (Method Concordance)
# Purpose: Visualize participant flow between cluster assignments across methods
# Adapted for Zenodo repository - uses cluster assignments from pipeline
# Original: manuscript/scripts/supplementary/14_figure_S10_sankey_flow.R
# ============================================================================

cat("=== Generating Figure S5: Participant Flow Diagram ===\n")

# Create output directories
dir.create("output/figures/supplementary", recursive = TRUE, showWarnings = FALSE)

# Load cluster assignments from pipeline (k=4)
kmeans_res <- readRDS("output/data/kmeans_results.rds")
hier_res <- readRDS("output/data/hierarchical_results.rds")
gmm_res <- readRDS("output/data/gmm_results.rds")
spectral_res <- readRDS("output/data/spectral_results.rds")

# Create cluster assignments dataframe
cluster_assignments <- data.frame(
  ID = 1:length(kmeans_res$cluster),
  kmeans = kmeans_res$cluster,
  hierarchical = hier_res$cluster,
  gmm = gmm_res$cluster,
  spectral = spectral_res$cluster
)

# Color palette for clusters
cluster_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")

# Method names
methods <- c("kmeans", "hierarchical", "gmm", "spectral")
method_labels <- c("K-means", "Hierarchical", "GMM", "Spectral")

# Create figure
png("output/figures/supplementary/figS5_participant_flow.png",
    width = 14, height = 10, units = "in", res = 300)

# Set up layout
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

# Panel A: K-means vs Hierarchical
method1 <- "kmeans"
method2 <- "hierarchical"
tab <- table(cluster_assignments[[method1]], cluster_assignments[[method2]])

# Normalize by row
prop_tab <- prop.table(tab, margin = 1) * 100

barplot(t(prop_tab),
        beside = TRUE,
        col = cluster_colors,
        main = "A) K-means vs Hierarchical",
        xlab = "K-means Cluster",
        ylab = "Proportion (%)",
        las = 1,
        legend.text = paste("H", 1:4),
        args.legend = list(x = "topright", bty = "n", title = "Hierarchical"))

# Panel B: K-means vs GMM
method2 <- "gmm"
tab <- table(cluster_assignments[[method1]], cluster_assignments[[method2]])
prop_tab <- prop.table(tab, margin = 1) * 100

barplot(t(prop_tab),
        beside = TRUE,
        col = cluster_colors,
        main = "B) K-means vs GMM",
        xlab = "K-means Cluster",
        ylab = "Proportion (%)",
        las = 1,
        legend.text = paste("G", 1:4),
        args.legend = list(x = "topright", bty = "n", title = "GMM"))

# Panel C: K-means vs Spectral
method2 <- "spectral"
tab <- table(cluster_assignments[[method1]], cluster_assignments[[method2]])
prop_tab <- prop.table(tab, margin = 1) * 100

barplot(t(prop_tab),
        beside = TRUE,
        col = cluster_colors,
        main = "C) K-means vs Spectral",
        xlab = "K-means Cluster",
        ylab = "Proportion (%)",
        las = 1,
        legend.text = paste("S", 1:4),
        args.legend = list(x = "topright", bty = "n", title = "Spectral"))

# Panel D: Summary concordance matrix
# Calculate overall concordance (ARI matrix)
concordance_matrix <- matrix(0, nrow = 4, ncol = 4)
rownames(concordance_matrix) <- method_labels
colnames(concordance_matrix) <- method_labels

for (i in 1:4) {
  for (j in 1:4) {
    if (i == j) {
      concordance_matrix[i, j] <- 1
    } else {
      m1 <- methods[i]
      m2 <- methods[j]

      # Calculate ARI
      tab <- table(cluster_assignments[[m1]], cluster_assignments[[m2]])
      n <- sum(tab)
      sum_nij2 <- sum(tab * (tab - 1)) / 2
      sum_ai2 <- sum(rowSums(tab) * (rowSums(tab) - 1)) / 2
      sum_bj2 <- sum(colSums(tab) * (colSums(tab) - 1)) / 2
      n_comb <- n * (n - 1) / 2

      expected <- (sum_ai2 * sum_bj2) / n_comb
      max_index <- (sum_ai2 + sum_bj2) / 2

      if (max_index == expected) {
        concordance_matrix[i, j] <- 1
      } else {
        concordance_matrix[i, j] <- (sum_nij2 - expected) / (max_index - expected)
      }
    }
  }
}

# Plot concordance summary
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

# Create text summary
summary_text <- c(
  "D) Concordance Summary (ARI Matrix)",
  "",
  "                 K-means  Hier.   GMM    Spec.",
  sprintf("K-means          1.000   %.3f  %.3f  %.3f",
          concordance_matrix[1, 2], concordance_matrix[1, 3], concordance_matrix[1, 4]),
  sprintf("Hierarchical     %.3f   1.000  %.3f  %.3f",
          concordance_matrix[2, 1], concordance_matrix[2, 3], concordance_matrix[2, 4]),
  sprintf("GMM              %.3f   %.3f  1.000  %.3f",
          concordance_matrix[3, 1], concordance_matrix[3, 2], concordance_matrix[3, 4]),
  sprintf("Spectral         %.3f   %.3f  %.3f  1.000",
          concordance_matrix[4, 1], concordance_matrix[4, 2], concordance_matrix[4, 3]),
  "",
  sprintf("Mean off-diagonal ARI: %.3f", mean(concordance_matrix[lower.tri(concordance_matrix)])),
  "",
  "Interpretation:",
  "",
  "  ARI > 0.5: Substantial agreement",
  "  ARI 0.3-0.5: Moderate agreement",
  "  ARI < 0.3: Fair agreement",
  "",
  "This indicates that while core patterns",
  "are identifiable across methods, cluster",
  "boundaries vary by algorithm."
)

text(0.05, seq(0.95, 0.05, length.out = length(summary_text)),
     summary_text, adj = 0, cex = 0.85, family = "mono")

title(main = "D) Method Concordance Summary", cex.main = 1.2)

dev.off()

cat("Saved: output/figures/supplementary/figS5_participant_flow.png\n")

# Create a more detailed transition table
transition_table <- data.frame()

for (i in 1:(length(methods) - 1)) {
  for (j in (i + 1):length(methods)) {
    m1 <- methods[i]
    m2 <- methods[j]

    tab <- table(cluster_assignments[[m1]], cluster_assignments[[m2]])

    # For each cluster in method 1, find dominant cluster in method 2
    for (c in 1:4) {
      dominant <- which.max(tab[c, ])
      prop <- tab[c, dominant] / sum(tab[c, ])

      transition_table <- rbind(transition_table, data.frame(
        From_method = method_labels[i],
        From_cluster = c,
        To_method = method_labels[j],
        To_cluster = dominant,
        n = tab[c, dominant],
        proportion = prop
      ))
    }
  }
}

# Save data for reproducibility
saveRDS(list(
  cluster_assignments = cluster_assignments,
  concordance_matrix = concordance_matrix,
  transition_table = transition_table
), "output/data/participant_flow_data.rds")

# Write CSV
write.csv(transition_table, "output/tables/transitions_between_methods.csv", row.names = FALSE)

# Print summary
cat("\n=== Method Concordance Summary ===\n")
cat("\nARI Matrix:\n")
print(round(concordance_matrix, 3))

cat(sprintf("\nMean off-diagonal ARI: %.3f\n",
            mean(concordance_matrix[lower.tri(concordance_matrix)])))

cat("\n=== Figure S5 complete ===\n")
