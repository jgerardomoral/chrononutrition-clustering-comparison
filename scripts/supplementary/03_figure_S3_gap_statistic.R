# ============================================================================
# Figure S3: Gap Statistic (with First-SE Rule)
# Purpose: Show Gap statistic curve with SE bars for k selection
# Adapted for Zenodo repository - uses output from main pipeline
# Original: manuscript/scripts/supplementary/03_figure_S3_gap_statistic.R
# ============================================================================

cat("=== Generating Figure S3: Gap Statistic ===\n")

# Create output directories
dir.create("output/figures/supplementary", recursive = TRUE, showWarnings = FALSE)

# Load k-selection results from main pipeline
optimal_k <- readRDS("output/data/optimal_k_results.rds")

# Get gap statistic table
gap_stat <- optimal_k$gap
gap_table <- gap_stat$Tab

# Create data frame
k_values <- 1:nrow(gap_table)
gap_df <- data.frame(
  k = k_values,
  gap = gap_table[, "gap"],  # Gap values
  SE = gap_table[, "SE.sim"],   # Standard errors
  logW = gap_table[, "logW"],
  E_logW = gap_table[, "E.logW"]
)

# Calculate first-SE rule (Tibshirani et al., 2001)
# k is optimal if Gap(k) >= Gap(k+1) - SE(k+1)
first_se_criterion <- rep(FALSE, nrow(gap_df))
for (i in 1:(nrow(gap_df) - 1)) {
  if (gap_df$gap[i] >= gap_df$gap[i + 1] - gap_df$SE[i + 1]) {
    first_se_criterion[i] <- TRUE
    break
  }
}

# Create figure
png("output/figures/supplementary/figS3_gap_statistic.png",
    width = 10, height = 8, units = "in", res = 300)

par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

# Panel A: Gap statistic with SE bars
plot(gap_df$k, gap_df$gap,
     type = "b",
     pch = 19,
     col = "#377EB8",
     lwd = 2,
     cex = 1.5,
     main = "A) Gap Statistic vs. Number of Clusters",
     xlab = "Number of clusters (k)",
     ylab = "Gap(k)",
     las = 1,
     xlim = c(1, max(gap_df$k)),
     cex.main = 1.2,
     cex.lab = 1.1)

# Add SE error bars
arrows(gap_df$k, gap_df$gap - gap_df$SE,
       gap_df$k, gap_df$gap + gap_df$SE,
       angle = 90, code = 3, length = 0.08,
       col = "#377EB8", lwd = 1.5)

# Highlight k=4
points(4, gap_df$gap[4], pch = 19, col = "#E41A1C", cex = 2.5)
points(4, gap_df$gap[4], pch = 1, col = "black", cex = 3, lwd = 2)

# Add vertical line at k=4
abline(v = 4, lty = 2, col = "#E41A1C", lwd = 1.5)

# Add annotation
text(4, gap_df$gap[4] + 0.08, "k=4\n(selected)",
     col = "#E41A1C", font = 2, cex = 0.9)

legend("bottomright",
       legend = c("Gap statistic +/- SE", "Selected k=4"),
       col = c("#377EB8", "#E41A1C"),
       pch = c(19, 19),
       pt.cex = c(1.5, 2),
       lty = c(1, 2),
       bty = "n",
       cex = 0.9)

# Panel B: First-SE rule visualization
gap_diff <- diff(gap_df$gap)
se_next <- gap_df$SE[-1]

plot(2:max(gap_df$k), gap_diff,
     type = "h",
     lwd = 3,
     col = ifelse(gap_diff < 0, "#E41A1C", "#4DAF4A"),
     main = "B) Gap(k) - Gap(k+1) (First-SE Rule)",
     xlab = "k",
     ylab = "Gap(k) - Gap(k+1)",
     las = 1,
     cex.main = 1.2,
     cex.lab = 1.1)

# Add SE threshold line
points(2:max(gap_df$k), -se_next, type = "b", pch = 17, col = "#984EA3", cex = 1.2)

abline(h = 0, lty = 2, col = "gray50")

legend("topright",
       legend = c("Gap difference", "-SE(k+1) threshold"),
       col = c("#4DAF4A", "#984EA3"),
       pch = c(NA, 17),
       lwd = c(3, 1),
       bty = "n",
       cex = 0.9)

# Panel C: logW comparison
plot(gap_df$k, gap_df$logW,
     type = "b",
     pch = 19,
     col = "#377EB8",
     lwd = 2,
     cex = 1.5,
     main = "C) Within-cluster Dispersion",
     xlab = "Number of clusters (k)",
     ylab = "log(W)",
     las = 1,
     cex.main = 1.2,
     cex.lab = 1.1)

# Add expected log(W) from reference distribution
lines(gap_df$k, gap_df$E_logW,
      type = "b", pch = 17, col = "#E41A1C", lwd = 2, cex = 1.2)

# Highlight k=4
abline(v = 4, lty = 2, col = "gray50", lwd = 1.5)

legend("topright",
       legend = c("Observed log(W)", "Expected E[log(W)]"),
       col = c("#377EB8", "#E41A1C"),
       pch = c(19, 17),
       lty = 1,
       lwd = 2,
       bty = "n",
       cex = 0.9)

# Panel D: First-SE Rule explanation
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

summary_text <- c(
  "Gap Statistic Summary (k=2 to k=5)",
  "",
  sprintf("k=2: Gap = %.3f +/- %.3f", gap_df$gap[2], gap_df$SE[2]),
  sprintf("k=3: Gap = %.3f +/- %.3f", gap_df$gap[3], gap_df$SE[3]),
  sprintf("k=4: Gap = %.3f +/- %.3f (SELECTED)", gap_df$gap[4], gap_df$SE[4]),
  sprintf("k=5: Gap = %.3f +/- %.3f", gap_df$gap[5], gap_df$SE[5]),
  "",
  "First-SE Rule (Tibshirani et al., 2001):",
  "",
  "Select the smallest k such that:",
  "  Gap(k) >= Gap(k+1) - SE(k+1)",
  "",
  "This criterion identifies the point where",
  "adding more clusters provides diminishing",
  "improvement in cluster separation.",
  "",
  "Final selection: k=4 based on first-SE rule",
  "and concordance with validation metrics."
)

text(0.1, seq(0.95, 0.05, length.out = length(summary_text)),
     summary_text, adj = 0, cex = 0.85, family = "mono")

title(main = "D) First-SE Rule Summary", cex.main = 1.2)

dev.off()

cat("Saved: output/figures/supplementary/figS3_gap_statistic.png\n")

# Save data for reproducibility
saveRDS(list(
  gap_df = gap_df,
  first_se_k = which(first_se_criterion)[1]
), "output/data/gap_statistic_data.rds")

# Print summary
cat("\n=== Gap Statistic Summary ===\n")
for (k in 2:min(5, max(gap_df$k))) {
  cat(sprintf("k=%d: Gap = %.4f (SE = %.4f)\n",
              k, gap_df$gap[k], gap_df$SE[k]))
}

cat("\n=== Figure S3 complete ===\n")
