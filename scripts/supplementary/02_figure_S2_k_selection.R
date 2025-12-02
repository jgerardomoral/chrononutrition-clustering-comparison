# =============================================================================
# Script: 02_figure_S2_k_selection.R
# Purpose: Generate Figure S2 - 3x2 panel with k-selection metrics (6 metrics)
# Adapted for Zenodo repository - uses output from 01_extract_all_metrics.R
# Original: manuscript/scripts/supplementary/02_figure_S2_k_selection.R
# =============================================================================

# -----------------------------------------------------------------------------
# SETUP
# -----------------------------------------------------------------------------

# Create output directories
dir.create("output/figures/supplementary", recursive = TRUE, showWarnings = FALSE)

# Load required packages
library(ggplot2)

# Check for patchwork, install if needed
if (!requireNamespace("patchwork", quietly = TRUE)) {
  cat("Installing patchwork package...\n")
  install.packages("patchwork", repos = "https://cloud.r-project.org")
}
library(patchwork)

cat("=== Generating Figure S2: K-Selection Metrics Panel (6 metrics) ===\n")

# -----------------------------------------------------------------------------
# LOAD DATA
# -----------------------------------------------------------------------------

metrics <- readRDS("output/data/all_metrics_k2_to_k5.rds")
cat("Loaded metrics:", nrow(metrics), "rows\n")
cat("Columns:", paste(names(metrics), collapse = ", "), "\n")

# -----------------------------------------------------------------------------
# DESIGN SPECIFICATIONS
# -----------------------------------------------------------------------------

# Color palette for methods (colorblind-friendly)
method_colors <- c(
  "K-means" = "#E41A1C",      # Red
  "Hierarchical" = "#377EB8", # Blue
  "GMM" = "#4DAF4A",          # Green
  "Spectral" = "#984EA3"      # Purple
)

# Publication theme
theme_publication <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, hjust = 0),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
      strip.background = element_rect(fill = "grey90", color = NA),
      strip.text = element_text(face = "bold"),
      plot.margin = margin(10, 10, 10, 10)
    )
}

# -----------------------------------------------------------------------------
# PANEL A: SILHOUETTE COEFFICIENT
# -----------------------------------------------------------------------------

p_silhouette <- ggplot(metrics, aes(x = k, y = Silhouette, color = Method, group = Method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  scale_color_manual(values = method_colors) +
  scale_x_continuous(breaks = 2:5) +
  scale_y_continuous(limits = c(0.1, 0.35)) +
  labs(
    title = "A. Silhouette Coefficient",
    x = "Number of Clusters (k)",
    y = "Average Silhouette Width"
  ) +
  theme_publication() +
  theme(legend.position = "none")

# -----------------------------------------------------------------------------
# PANEL B: CALINSKI-HARABASZ INDEX
# -----------------------------------------------------------------------------

p_ch <- ggplot(metrics, aes(x = k, y = CalinskiHarabasz, color = Method, group = Method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  scale_color_manual(values = method_colors) +
  scale_x_continuous(breaks = 2:5) +
  labs(
    title = "B. Calinski-Harabasz Index",
    x = "Number of Clusters (k)",
    y = "CH Index"
  ) +
  theme_publication() +
  theme(legend.position = "none")

# -----------------------------------------------------------------------------
# PANEL C: DUNN INDEX
# -----------------------------------------------------------------------------

p_dunn <- ggplot(metrics, aes(x = k, y = Dunn, color = Method, group = Method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  scale_color_manual(values = method_colors) +
  scale_x_continuous(breaks = 2:5) +
  labs(
    title = "C. Dunn Index",
    x = "Number of Clusters (k)",
    y = "Dunn Index"
  ) +
  theme_publication() +
  theme(legend.position = "none")

# -----------------------------------------------------------------------------
# PANEL D: GAMMA (PEARSON GAMMA)
# -----------------------------------------------------------------------------

p_gamma <- ggplot(metrics, aes(x = k, y = Gamma, color = Method, group = Method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  scale_color_manual(values = method_colors) +
  scale_x_continuous(breaks = 2:5) +
  labs(
    title = "D. Gamma (Pearson)",
    x = "Number of Clusters (k)",
    y = "Gamma Coefficient"
  ) +
  theme_publication() +
  theme(legend.position = "none")

# -----------------------------------------------------------------------------
# PANEL E: ENTROPY
# -----------------------------------------------------------------------------

p_entropy <- ggplot(metrics, aes(x = k, y = Entropy, color = Method, group = Method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  scale_color_manual(values = method_colors) +
  scale_x_continuous(breaks = 2:5) +
  labs(
    title = "E. Entropy",
    x = "Number of Clusters (k)",
    y = "Entropy"
  ) +
  theme_publication() +
  theme(legend.position = "none")

# -----------------------------------------------------------------------------
# PANEL F: DAVIES-BOULDIN INDEX
# -----------------------------------------------------------------------------

# Note: For Davies-Bouldin, LOWER values indicate better clustering
p_db <- ggplot(metrics, aes(x = k, y = DaviesBouldin, color = Method, group = Method)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 4, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  scale_color_manual(values = method_colors) +
  scale_x_continuous(breaks = 2:5) +
  labs(
    title = "F. Davies-Bouldin Index",
    x = "Number of Clusters (k)",
    y = "Davies-Bouldin Index"
  ) +
  theme_publication() +
  theme(legend.position = "none")

# -----------------------------------------------------------------------------
# COMBINE PANELS (3 rows x 2 columns = 6 panels)
# -----------------------------------------------------------------------------

# Create combined plot with shared legend
combined_plot <- (p_silhouette | p_ch) / (p_dunn | p_gamma) / (p_entropy | p_db) +
  plot_annotation(
    caption = "Dashed vertical line indicates selected k=4 solution. Note: Lower Davies-Bouldin values indicate better clustering."
  ) &
  theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 10))

# Add shared legend at bottom
legend_plot <- ggplot(metrics, aes(x = k, y = Silhouette, color = Method)) +
  geom_point() +
  scale_color_manual(values = method_colors) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 11)
  ) +
  guides(color = guide_legend(nrow = 1))

# Extract legend and combine
if (requireNamespace("cowplot", quietly = TRUE)) {
  final_plot <- combined_plot / cowplot::get_legend(legend_plot) +
    plot_layout(heights = c(1, 1, 1, 0.1))
} else {
  final_plot <- combined_plot
}

# -----------------------------------------------------------------------------
# SAVE FIGURE
# -----------------------------------------------------------------------------

output_path <- "output/figures/supplementary/figS2_k_selection_metrics.png"

ggsave(
  filename = output_path,
  plot = combined_plot,
  width = 9,
  height = 12,  # Reduced for better page fit
  dpi = 300,
  bg = "white"
)

cat("Saved:", output_path, "\n")

# -----------------------------------------------------------------------------
# SUMMARY
# -----------------------------------------------------------------------------

cat("\n=== Metrics Summary ===\n")
cat("6 panels: Silhouette, Calinski-Harabasz, Dunn, Gamma, Entropy, Davies-Bouldin\n")
cat("Layout: 3 rows x 2 columns\n")
cat("Size: 9 x 12 inches, 300 DPI\n")
cat("Note: Davies-Bouldin - lower values indicate better clustering\n")

cat("\n=== Figure S2 generation complete (6 panels) ===\n")
