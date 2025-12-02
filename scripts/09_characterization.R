# ===================================================================
# 09_CHARACTERIZATION.R - Cluster Characterization (Tables 2-6)
# ===================================================================
# Characterizes clusters by sociodemographic, anthropometric, and
# dietary variables for ALL 4 clustering methods.
# Output: Tables 2-6 with cluster profiles and statistical comparisons
# ===================================================================

library(dplyr)
library(readr)
library(tidyr)

cat("=== CLUSTER CHARACTERIZATION (4 METHODS) ===\n\n")

# Load data and all clustering results
datos <- read_csv("data/chrono_patterns_n459.csv", show_col_types = FALSE)
prepared <- readRDS("output/data/prepared_data.rds")
datos_clustering <- prepared$datos_clustering

# Load all 4 clustering results
kmeans_res <- readRDS("output/data/kmeans_results.rds")
hier_res <- readRDS("output/data/hierarchical_results.rds")
gmm_res <- readRDS("output/data/gmm_results.rds")
spectral_res <- readRDS("output/data/spectral_results.rds")

# Add cluster assignments to data
datos_char <- datos %>%
  filter(id %in% datos_clustering$id) %>%
  mutate(
    cluster_kmeans = kmeans_res$cluster,
    cluster_hier = hier_res$cluster,
    cluster_gmm = gmm_res$cluster,
    cluster_spectral = spectral_res$cluster
  )

cat("Characterizing n =", nrow(datos_char), "participants across 4 methods\n\n")

# Helper functions
format_median_iqr <- function(x) {
  med <- median(x, na.rm = TRUE)
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  sprintf("%.1f [%.1f, %.1f]", med, q1, q3)
}

format_time_median_iqr <- function(x) {
  med <- median(x, na.rm = TRUE)
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  # Convert decimal hours to HH:MM
  to_time <- function(h) {
    hours <- floor(h)
    mins <- round((h - hours) * 60)
    sprintf("%02d:%02d", hours, mins)
  }
  sprintf("%s [%s, %s]", to_time(med), to_time(q1), to_time(q3))
}

format_n_pct <- function(n, total) {
  sprintf("%d (%.1f)", n, 100*n/total)
}

# -------------------------------------------------------------------
# TABLE 2: Meal Timing Patterns by Method
# -------------------------------------------------------------------

cat("=== TABLE 2: Meal Timing Patterns ===\n\n")

methods <- list(
  "K-means" = "cluster_kmeans",
  "Hierarchical" = "cluster_hier",
  "GMM" = "cluster_gmm",
  "Spectral" = "cluster_spectral"
)

table2_results <- list()

for (method_name in names(methods)) {
  cluster_var <- methods[[method_name]]

  cat(method_name, ":\n")

  # Calculate median [Q1, Q3] for each cluster
  result <- datos_char %>%
    group_by(cluster = .data[[cluster_var]]) %>%
    summarise(
      n = n(),
      pct = round(100 * n() / nrow(datos_char), 1),
      breakfast = format_time_median_iqr(breakfast_time),
      lunch = format_time_median_iqr(lunch_time),
      dinner = format_time_median_iqr(dinner_time),
      .groups = "drop"
    ) %>%
    mutate(method = method_name)

  # Kruskal-Wallis tests
  kw_bf <- kruskal.test(datos_char$breakfast_time ~ datos_char[[cluster_var]])
  kw_lu <- kruskal.test(datos_char$lunch_time ~ datos_char[[cluster_var]])
  kw_di <- kruskal.test(datos_char$dinner_time ~ datos_char[[cluster_var]])

  table2_results[[method_name]] <- result

  for (i in 1:nrow(result)) {
    cat("  C", result$cluster[i], " (n=", result$n[i], ", ", result$pct[i], "%):",
        " BF=", result$breakfast[i],
        " LU=", result$lunch[i],
        " DI=", result$dinner[i], "\n", sep = "")
  }
  cat("  p-values: BF=", format.pval(kw_bf$p.value, digits = 3),
      " LU=", format.pval(kw_lu$p.value, digits = 3),
      " DI=", format.pval(kw_di$p.value, digits = 3), "\n\n")
}

table2_df <- bind_rows(table2_results)
write.csv(table2_df, "output/tables/table2_meal_timing.csv", row.names = FALSE)
cat("Saved: output/tables/table2_meal_timing.csv\n\n")

# -------------------------------------------------------------------
# TABLE 4: Sociodemographic Characteristics by Method
# -------------------------------------------------------------------

cat("=== TABLE 4: Sociodemographic Characteristics ===\n\n")

table4_results <- list()

for (method_name in names(methods)) {
  cluster_var <- methods[[method_name]]

  cat(method_name, ":\n")

  # Age (continuous)
  age_result <- datos_char %>%
    group_by(cluster = .data[[cluster_var]]) %>%
    summarise(
      n = n(),
      age = format_median_iqr(age),
      .groups = "drop"
    )

  kw_age <- kruskal.test(datos_char$age ~ datos_char[[cluster_var]])
  cat("  Age p-value:", format.pval(kw_age$p.value, digits = 3), "\n")

  # Sex (categorical)
  sex_table <- table(datos_char[[cluster_var]], datos_char$sex)
  chi_sex <- chisq.test(sex_table)
  cat("  Sex p-value:", format.pval(chi_sex$p.value, digits = 3), "\n")

  # Academic program
  prog_table <- table(datos_char[[cluster_var]], datos_char$academic_program)
  chi_prog <- chisq.test(prog_table)
  cat("  Academic program p-value:", format.pval(chi_prog$p.value, digits = 3), "\n")

  # Employment
  emp_table <- table(datos_char[[cluster_var]], datos_char$employment)
  chi_emp <- chisq.test(emp_table)
  cat("  Employment p-value:", format.pval(chi_emp$p.value, digits = 3), "\n\n")

  # Build result dataframe
  result <- datos_char %>%
    group_by(cluster = .data[[cluster_var]]) %>%
    summarise(
      n = n(),
      pct = round(100 * n() / nrow(datos_char), 1),
      age = format_median_iqr(age),
      female_n = sum(sex == "Female"),
      female_pct = round(100 * sum(sex == "Female") / n(), 1),
      .groups = "drop"
    ) %>%
    mutate(
      method = method_name,
      p_age = format.pval(kw_age$p.value, digits = 3),
      p_sex = format.pval(chi_sex$p.value, digits = 3),
      p_program = format.pval(chi_prog$p.value, digits = 3),
      p_employment = format.pval(chi_emp$p.value, digits = 3)
    )

  table4_results[[method_name]] <- result
}

table4_df <- bind_rows(table4_results)
write.csv(table4_df, "output/tables/table4_sociodemographic.csv", row.names = FALSE)
cat("Saved: output/tables/table4_sociodemographic.csv\n\n")

# -------------------------------------------------------------------
# TABLE 5: Chrononutrition Characteristics by Method
# -------------------------------------------------------------------

cat("=== TABLE 5: Chrononutrition Characteristics ===\n\n")

table5_results <- list()

for (method_name in names(methods)) {
  cluster_var <- methods[[method_name]]

  cat(method_name, ":\n")

  result <- datos_char %>%
    group_by(cluster = .data[[cluster_var]]) %>%
    summarise(
      n = n(),
      pct = round(100 * n() / nrow(datos_char), 1),
      eating_window = format_median_iqr(eating_window),
      overnight_fasting = format_median_iqr(overnight_fasting),
      eating_midpoint = format_time_median_iqr(eating_midpoint),
      breakfast_lunch_interval = format_median_iqr(breakfast_lunch_interval),
      lunch_dinner_interval = format_median_iqr(lunch_dinner_interval),
      .groups = "drop"
    ) %>%
    mutate(method = method_name)

  # Kruskal-Wallis tests
  kw_ew <- kruskal.test(datos_char$eating_window ~ datos_char[[cluster_var]])
  kw_of <- kruskal.test(datos_char$overnight_fasting ~ datos_char[[cluster_var]])
  kw_em <- kruskal.test(datos_char$eating_midpoint ~ datos_char[[cluster_var]])
  kw_bl <- kruskal.test(datos_char$breakfast_lunch_interval ~ datos_char[[cluster_var]])
  kw_ld <- kruskal.test(datos_char$lunch_dinner_interval ~ datos_char[[cluster_var]])

  result$p_eating_window <- format.pval(kw_ew$p.value, digits = 3)
  result$p_overnight_fasting <- format.pval(kw_of$p.value, digits = 3)
  result$p_eating_midpoint <- format.pval(kw_em$p.value, digits = 3)
  result$p_bf_lunch <- format.pval(kw_bl$p.value, digits = 3)
  result$p_lunch_dinner <- format.pval(kw_ld$p.value, digits = 3)

  table5_results[[method_name]] <- result

  cat("  p-values: EW=", format.pval(kw_ew$p.value, digits = 3),
      " OF=", format.pval(kw_of$p.value, digits = 3),
      " EM=", format.pval(kw_em$p.value, digits = 3),
      " BL=", format.pval(kw_bl$p.value, digits = 3),
      " LD=", format.pval(kw_ld$p.value, digits = 3), "\n\n")
}

table5_df <- bind_rows(table5_results)
write.csv(table5_df, "output/tables/table5_chrononutrition.csv", row.names = FALSE)
cat("Saved: output/tables/table5_chrononutrition.csv\n\n")

# -------------------------------------------------------------------
# TABLE 6: Anthropometric and Diet Quality by Method
# -------------------------------------------------------------------

cat("=== TABLE 6: Anthropometric and Diet Quality ===\n\n")

table6_results <- list()

for (method_name in names(methods)) {
  cluster_var <- methods[[method_name]]

  cat(method_name, ":\n")

  result <- datos_char %>%
    group_by(cluster = .data[[cluster_var]]) %>%
    summarise(
      n = n(),
      pct = round(100 * n() / nrow(datos_char), 1),
      bmi = format_median_iqr(bmi),
      waist = format_median_iqr(waist_circumference),
      .groups = "drop"
    ) %>%
    mutate(method = method_name)

  # Kruskal-Wallis tests
  kw_bmi <- kruskal.test(datos_char$bmi ~ datos_char[[cluster_var]])
  kw_waist <- kruskal.test(datos_char$waist_circumference ~ datos_char[[cluster_var]])

  # Food intake quality
  fiq_table <- table(datos_char[[cluster_var]], datos_char$food_intake_quality)
  chi_fiq <- chisq.test(fiq_table)

  result$p_bmi <- format.pval(kw_bmi$p.value, digits = 3)
  result$p_waist <- format.pval(kw_waist$p.value, digits = 3)
  result$p_food_quality <- format.pval(chi_fiq$p.value, digits = 3)

  # Add food quality percentages
  fiq_by_cluster <- datos_char %>%
    group_by(cluster = .data[[cluster_var]], food_intake_quality) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(cluster) %>%
    mutate(pct = round(100 * count / sum(count), 1)) %>%
    ungroup()

  table6_results[[method_name]] <- result

  cat("  BMI p-value:", format.pval(kw_bmi$p.value, digits = 3),
      " Waist p-value:", format.pval(kw_waist$p.value, digits = 3),
      " Food quality p-value:", format.pval(chi_fiq$p.value, digits = 3), "\n\n")
}

table6_df <- bind_rows(table6_results)
write.csv(table6_df, "output/tables/table6_anthropometric_diet.csv", row.names = FALSE)
cat("Saved: output/tables/table6_anthropometric_diet.csv\n\n")

# -------------------------------------------------------------------
# Chronotype by Method
# -------------------------------------------------------------------

cat("=== CHRONOTYPE BY METHOD ===\n\n")

for (method_name in names(methods)) {
  cluster_var <- methods[[method_name]]

  chrono_table <- table(datos_char[[cluster_var]], datos_char$chronotype)
  chi_chrono <- chisq.test(chrono_table)

  cat(method_name, "- Chronotype p-value:", format.pval(chi_chrono$p.value, digits = 3), "\n")
}

# -------------------------------------------------------------------
# Summary Output
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Save characterization results for figure generation
# -------------------------------------------------------------------

characterization_results <- list(
  data_with_clusters = datos_char,
  table2 = table2_results,
  table4 = table4_results,
  table5 = table5_results,
  table6 = table6_results
)

saveRDS(characterization_results, "output/data/characterization_results.rds")
cat("Saved: output/data/characterization_results.rds\n\n")

cat("=== CHARACTERIZATION COMPLETE ===\n")
cat("Tables generated:\n")
cat("  - output/tables/table2_meal_timing.csv\n")
cat("  - output/tables/table4_sociodemographic.csv\n")
cat("  - output/tables/table5_chrononutrition.csv\n")
cat("  - output/tables/table6_anthropometric_diet.csv\n")
cat("  - output/data/characterization_results.rds\n")
