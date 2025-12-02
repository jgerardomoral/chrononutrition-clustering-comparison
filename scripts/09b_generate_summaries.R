# ===================================================================
# 09B_GENERATE_SUMMARIES.R - Generate Table Summaries for Figure 1
# ===================================================================
# Creates summary statistics (medians) for each cluster and method
# Output: table_summaries.rds with same structure as verified file
# Required for: 10_generate_figure.R (Figure 1 temporal profiles)
# ===================================================================

library(dplyr)
library(readr)

cat("=== GENERATING TABLE SUMMARIES FOR FIGURE 1 ===\n\n")

# Load original data
datos <- read_csv("data/chrono_patterns_n459.csv", show_col_types = FALSE)

# Load clustering results
kmeans_res <- readRDS("output/data/kmeans_results.rds")
hier_res <- readRDS("output/data/hierarchical_results.rds")
gmm_res <- readRDS("output/data/gmm_results.rds")
spectral_res <- readRDS("output/data/spectral_results.rds")

# Prepared data (for ID matching)
prepared <- readRDS("output/data/prepared_data.rds")
datos_clustering <- prepared$datos_clustering

# Filter original data to match clustering sample
datos_filtered <- datos %>%
  filter(id %in% datos_clustering$id)

cat("Working with n =", nrow(datos_filtered), "participants\n\n")

# Function to calculate summaries for a method
calculate_summaries <- function(datos, clusters, method_name) {
  cat("Processing", method_name, "...\n")

  # Add cluster assignment
  datos_with_cluster <- datos %>%
    mutate(cluster = clusters)

  # Calculate medians for each cluster
  summaries <- datos_with_cluster %>%
    group_by(cluster) %>%
    summarise(
      n = n(),
      bf_med = median(breakfast_time, na.rm = TRUE),
      lunch_med = median(lunch_time, na.rm = TRUE),
      dinner_med = median(dinner_time, na.rm = TRUE),
      ew_med = median(eating_window, na.rm = TRUE),
      fast_med = median(overnight_fasting, na.rm = TRUE),
      midpoint_med = median(eating_midpoint, na.rm = TRUE),
      bf_lunch_med = median(breakfast_lunch_interval, na.rm = TRUE),
      lunch_dinner_med = median(lunch_dinner_interval, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(cluster)

  # Calculate eating midpoint for P-code assignment (tiebreaker)
  summaries <- summaries %>%
    mutate(eating_midpoint_calc = (bf_med + dinner_med) / 2)

  # Assign P-codes (1=earliest, 4=latest) with eating midpoint as tiebreaker
  summaries <- summaries %>%
    arrange(bf_med, eating_midpoint_calc) %>%
    mutate(p_bf = row_number()) %>%
    arrange(lunch_med, eating_midpoint_calc) %>%
    mutate(p_lunch = row_number()) %>%
    arrange(dinner_med, eating_midpoint_calc) %>%
    mutate(p_dinner = row_number()) %>%
    arrange(cluster)  # Restore original order

  # Create P-code string
  summaries <- summaries %>%
    mutate(p_code = paste0("P", p_bf, "-P", p_lunch, "-P", p_dinner))

  # ABN nomenclature based on predominance
  summaries <- summaries %>%
    mutate(
      label = case_when(
        # Early: P1 predominant (appears in 2+ meals)
        p_code %in% c("P1-P1-P1", "P2-P1-P1", "P1-P2-P1", "P1-P1-P2") ~ "Early Eaters",

        # Early-Intermediate: P2 predominant
        p_code %in% c("P1-P2-P2", "P2-P2-P2", "P2-P1-P2", "P2-P2-P1",
                      "P3-P2-P2", "P2-P3-P2", "P2-P2-P3") ~ "Early-Intermediate Eaters",

        # Late-Intermediate: P3 predominant
        p_code %in% c("P3-P3-P3", "P4-P3-P3", "P3-P4-P3", "P3-P3-P4",
                      "P2-P3-P3", "P3-P2-P3", "P3-P3-P2",
                      "P1-P3-P3", "P3-P1-P3", "P3-P3-P1") ~ "Intermediate Eaters",

        # Late: P4-P4-P4 only (consistently late)
        p_code == "P4-P4-P4" ~ "Late Eaters",

        # Late (early breakfast): P2-P4-P4 specifically
        p_code == "P2-P4-P4" ~ "Late (early breakfast) Eaters",

        # Fallback patterns
        p_code %in% c("P4-P4-P3", "P4-P3-P4", "P3-P4-P4") ~ "Late-Intermediate Eaters",
        p_code %in% c("P1-P4-P4", "P4-P1-P4", "P4-P4-P1") ~ "Mixed Eaters",

        TRUE ~ "Unclassified"
      )
    ) %>%
    dplyr::select(-eating_midpoint_calc)

  return(summaries)
}

# Calculate summaries for each method
summaries_kmeans <- calculate_summaries(datos_filtered, kmeans_res$cluster, "K-means")
summaries_hier <- calculate_summaries(datos_filtered, hier_res$cluster, "Hierarchical")
summaries_gmm <- calculate_summaries(datos_filtered, gmm_res$cluster, "GMM")
summaries_spectral <- calculate_summaries(datos_filtered, spectral_res$cluster, "Spectral")

# Combine into list
table_summaries <- list(
  kmeans = summaries_kmeans,
  hierarchical = summaries_hier,
  gmm = summaries_gmm,
  spectral = summaries_spectral
)

# Print summaries
cat("\n=== K-means Summary ===\n")
print(summaries_kmeans %>% dplyr::select(cluster, n, bf_med, lunch_med, dinner_med, p_code, label))

cat("\n=== Hierarchical Summary ===\n")
print(summaries_hier %>% dplyr::select(cluster, n, bf_med, lunch_med, dinner_med, p_code, label))

cat("\n=== GMM Summary ===\n")
print(summaries_gmm %>% dplyr::select(cluster, n, bf_med, lunch_med, dinner_med, p_code, label))

cat("\n=== Spectral Summary ===\n")
print(summaries_spectral %>% dplyr::select(cluster, n, bf_med, lunch_med, dinner_med, p_code, label))

# Save
saveRDS(table_summaries, "output/data/table_summaries.rds")
cat("\nSaved: output/data/table_summaries.rds\n")

cat("\n=== TABLE SUMMARIES GENERATION COMPLETE ===\n")
