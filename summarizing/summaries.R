# Text Quality Analysis Script


# Load required libraries
suppressMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(corrr)
  library(tidyr)
  library(purrr)
  library(broom)
})

cat("All required libraries loaded successfully.\n")

# Set working directories
data_path <- "C:/yourpathto/text_quality_analysis_results.xlsx"
output_path <- "C:/yourpath"

# Create output directory if it doesn't exist
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# Set publication-ready theme
publication_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold", margin = margin(b = 20)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 15)),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.grid.minor = element_line(color = "grey95", size = 0.3),
    strip.text = element_text(size = 13, face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  )

# Read the data
cat("Reading data...\n")

# Define multiple possible data paths
possible_paths <- c(
  data_path,
  "yourpath/paste.txt",
  "paste.txt"
)

data <- NULL
for (path in possible_paths) {
  if (file.exists(path)) {
    cat("Trying to read from:", path, "\n")
    tryCatch({
      if (grepl("\\.xlsx$", path)) {
        data <- read_excel(path)
      } else {
        # Try tab-separated first, then comma-separated
        data <- read.table(path, header = TRUE, sep = "\t", stringsAsFactors = FALSE, quote = "")
        if (ncol(data) == 1) {
          # If only one column, try comma separation
          data <- read.table(path, header = TRUE, sep = ",", stringsAsFactors = FALSE, quote = "")
        }
      }
      cat("Successfully read data from:", path, "\n")
      break
    }, error = function(e) {
      cat("Error reading", path, ":", e$message, "\n")
    })
  }
}

if (is.null(data)) {
  stop("Could not read data from any of the specified paths")
}

# Display basic info about the data
cat("Data dimensions:", dim(data), "\n")
cat("Columns:", colnames(data), "\n")

# Check if required columns exist
required_cols <- c("method", "coherence_mean", "diversity_mean", "coherence_variance", "diversity_variance", "human_mean")
missing_cols <- required_cols[!required_cols %in% colnames(data)]

if (length(missing_cols) > 0) {
  cat("Missing required columns:", paste(missing_cols, collapse = ", "), "\n")
  cat("Available columns:", paste(colnames(data), collapse = ", "), "\n")
  stop("Required columns are missing from the data")
}

# Check for id_dataset column or create it
if (!"id_dataset" %in% colnames(data)) {
  if ("id" %in% colnames(data)) {
    data$id_dataset <- data$id
    cat("Using 'id' column as 'id_dataset'\n")
  } else {
    # Create a single group if no ID column exists
    data$id_dataset <- 1
    cat("No ID column found, treating all data as single group\n")
  }
}

cat("Unique methods:", unique(data$method), "\n")
cat("Unique id_dataset values:", length(unique(data$id_dataset)), "\n")
cat("Sample of data:\n")
print(head(data[, c("id_dataset", "method", "coherence_mean", "diversity_mean", "human_mean")]))

# ============================================================================
# ANALYSIS 1: Normalized differences from Human baseline
# ============================================================================

cat("\n=== ANALYSIS 1: Normalized differences from Human baseline ===\n")

# Function to calculate normalized differences within each group
calculate_normalized_differences <- function(group_data) {
  # Check if group_data is valid
  if (is.null(group_data) || nrow(group_data) == 0) {
    return(NULL)
  }
  
  # Find human baseline
  human_rows <- group_data[group_data$method == "Human", ]
  
  if (nrow(human_rows) == 0) {
    cat("Warning: No Human baseline found for group with id_dataset:", 
        unique(group_data$id_dataset)[1], "\n")
    return(NULL)
  }
  
  # Use first human row if multiple exist
  human_row <- human_rows[1, ]
  
  # Get non-human methods
  non_human_data <- group_data[group_data$method != "Human", ]
  
  if (nrow(non_human_data) == 0) {
    cat("Warning: No non-human methods found for group with id_dataset:", 
        unique(group_data$id_dataset)[1], "\n")
    return(NULL)
  }
  
  cat("Processing group", unique(group_data$id_dataset)[1], 
      "with", nrow(non_human_data), "non-human methods\n")
  
  # Calculate raw differences
  coherence_diff <- non_human_data$coherence_mean - human_row$coherence_mean
  diversity_diff <- non_human_data$diversity_mean - human_row$diversity_mean
  coherence_var_diff <- non_human_data$coherence_variance - human_row$coherence_variance
  diversity_var_diff <- non_human_data$diversity_variance - human_row$diversity_variance
  
  # Length normalization (by text length if available, otherwise by 1)
  if ("length" %in% colnames(non_human_data)) {
    length_norm <- non_human_data$length
    length_values <- non_human_data$length
  } else {
    length_norm <- rep(1, nrow(non_human_data))
    length_values <- rep(NA, nrow(non_human_data))
  }
  
  # Calculate normalized differences
  result <- data.frame(
    id_dataset = non_human_data$id_dataset,
    method = non_human_data$method,
    human_mean = non_human_data$human_mean,
    coherence_diff_norm = coherence_diff / length_norm,
    diversity_diff_norm = diversity_diff / length_norm,
    coherence_var_diff_norm = coherence_var_diff / length_norm,
    diversity_var_diff_norm = diversity_var_diff / length_norm,
    length = length_values,
    stringsAsFactors = FALSE
  )
  
  return(result)
}

# Apply to each group
cat("Calculating normalized differences for each group...\n")

# Check if we have Human baselines
human_count <- sum(data$method == "Human")
cat("Found", human_count, "Human baseline entries\n")

if (human_count == 0) {
  cat("Warning: No Human baselines found. Analysis 1 will be skipped.\n")
  analysis1_data <- NULL
} else {
  # Apply function to each group and combine results
  analysis1_results <- data %>%
    group_by(id_dataset) %>%
    group_split() %>%
    map(calculate_normalized_differences) %>%
    compact()  # Remove NULL results
  
  if (length(analysis1_results) > 0) {
    analysis1_data <- bind_rows(analysis1_results)
    cat("Analysis 1 data created with", nrow(analysis1_data), "rows\n")
  } else {
    cat("No valid groups found for Analysis 1\n")
    analysis1_data <- NULL
  }
}

# Z-score normalization across all differences
if (!is.null(analysis1_data) && nrow(analysis1_data) > 1) {
  cat("Applying z-score normalization...\n")
  
  analysis1_data <- analysis1_data %>%
    mutate(
      coherence_diff_z = as.numeric(scale(coherence_diff_norm)),
      diversity_diff_z = as.numeric(scale(diversity_diff_norm)),
      coherence_var_diff_z = as.numeric(scale(coherence_var_diff_norm)),
      diversity_var_diff_z = as.numeric(scale(diversity_var_diff_norm))
    )
  
  # Create composite scores
  analysis1_data <- analysis1_data %>%
    mutate(
      mean_composite_score = (coherence_diff_z + diversity_diff_z) / 2,
      var_composite_score = (coherence_var_diff_z + diversity_var_diff_z) / 2,
      overall_composite_score = (coherence_diff_z + diversity_diff_z + 
                                   coherence_var_diff_z + diversity_var_diff_z) / 4
    )
  
  # Calculate correlations with human ratings
  cat("Calculating correlations for Analysis 1...\n")
  correlations_analysis1 <- list(
    coherence_diff = cor.test(analysis1_data$coherence_diff_z, analysis1_data$human_mean, method = "spearman"),
    diversity_diff = cor.test(analysis1_data$diversity_diff_z, analysis1_data$human_mean, method = "spearman"),
    coherence_var_diff = cor.test(analysis1_data$coherence_var_diff_z, analysis1_data$human_mean, method = "spearman"),
    diversity_var_diff = cor.test(analysis1_data$diversity_var_diff_z, analysis1_data$human_mean, method = "spearman"),
    mean_composite = cor.test(analysis1_data$mean_composite_score, analysis1_data$human_mean, method = "spearman"),
    var_composite = cor.test(analysis1_data$var_composite_score, analysis1_data$human_mean, method = "spearman"),
    overall_composite = cor.test(analysis1_data$overall_composite_score, analysis1_data$human_mean, method = "spearman")
  )
  
  # Print results
  cat("Spearman correlations with human ratings (Analysis 1):\n")
  for (name in names(correlations_analysis1)) {
    cor_result <- correlations_analysis1[[name]]
    cat(sprintf("%s: r = %.3f, p = %.4f\n", 
                name, cor_result$estimate, cor_result$p.value))
  }
} else {
  cat("Insufficient data for Analysis 1 (need >1 observations for z-score normalization)\n")
  correlations_analysis1 <- NULL
}

# ============================================================================
# ANALYSIS 2: Distance from empirical reference vectors
# ============================================================================

cat("\n=== ANALYSIS 2: Distance from empirical reference vectors ===\n")

# Empirical reference vectors
empirical_mean_coherence <- -2.782422166
empirical_mean_diversity <- 0.93484131
empirical_var_coherence <- 7.217569204
empirical_var_diversity <- 0.01266436

# Calculate distances for all methods (including Human)
analysis2_data <- data %>%
  mutate(
    # Distance from empirical means
    mean_distance = sqrt((coherence_mean - empirical_mean_coherence)^2 + 
                           (diversity_mean - empirical_mean_diversity)^2),
    
    # Distance from empirical variances  
    var_distance = sqrt((coherence_variance - empirical_var_coherence)^2 + 
                          (diversity_variance - empirical_var_diversity)^2),
    
    # Combined distance score (lower is better, so we'll use negative)
    combined_distance = sqrt((coherence_mean - empirical_mean_coherence)^2 + 
                               (diversity_mean - empirical_mean_diversity)^2 +
                               (coherence_variance - empirical_var_coherence)^2 + 
                               (diversity_variance - empirical_var_diversity)^2),
    
    # Inverse distance scores (higher is better for correlation)
    mean_score = -mean_distance,
    var_score = -var_distance,
    combined_score = -combined_distance
  )

# Calculate correlations with human ratings
correlations_analysis2 <- list(
  mean_distance = cor.test(analysis2_data$mean_score, analysis2_data$human_mean, method = "spearman"),
  var_distance = cor.test(analysis2_data$var_score, analysis2_data$human_mean, method = "spearman"),
  combined_distance = cor.test(analysis2_data$combined_score, analysis2_data$human_mean, method = "spearman")
)

# Print results
cat("Spearman correlations with human ratings (Analysis 2):\n")
for (name in names(correlations_analysis2)) {
  cor_result <- correlations_analysis2[[name]]
  cat(sprintf("%s: r = %.3f, p = %.4f\n", 
              name, cor_result$estimate, cor_result$p.value))
}

# ============================================================================
# COMPARISON AND RESULTS SUMMARY
# ============================================================================

cat("\n=== COMPARISON OF APPROACHES ===\n")

# Create summary table
if (!is.null(correlations_analysis1)) {
  summary_results <- data.frame(
    Analysis = c("Analysis 1 - Coherence Diff", "Analysis 1 - Diversity Diff", 
                 "Analysis 1 - Coherence Var Diff", "Analysis 1 - Diversity Var Diff",
                 "Analysis 1 - Mean Composite", "Analysis 1 - Var Composite", 
                 "Analysis 1 - Overall Composite",
                 "Analysis 2 - Mean Distance", "Analysis 2 - Var Distance", 
                 "Analysis 2 - Combined Distance"),
    Correlation = c(correlations_analysis1$coherence_diff$estimate,
                    correlations_analysis1$diversity_diff$estimate,
                    correlations_analysis1$coherence_var_diff$estimate,
                    correlations_analysis1$diversity_var_diff$estimate,
                    correlations_analysis1$mean_composite$estimate,
                    correlations_analysis1$var_composite$estimate,
                    correlations_analysis1$overall_composite$estimate,
                    correlations_analysis2$mean_distance$estimate,
                    correlations_analysis2$var_distance$estimate,
                    correlations_analysis2$combined_distance$estimate),
    P_value = c(correlations_analysis1$coherence_diff$p.value,
                correlations_analysis1$diversity_diff$p.value,
                correlations_analysis1$coherence_var_diff$p.value,
                correlations_analysis1$diversity_var_diff$p.value,
                correlations_analysis1$mean_composite$p.value,
                correlations_analysis1$var_composite$p.value,
                correlations_analysis1$overall_composite$p.value,
                correlations_analysis2$mean_distance$p.value,
                correlations_analysis2$var_distance$p.value,
                correlations_analysis2$combined_distance$p.value)
  )
} else {
  summary_results <- data.frame(
    Analysis = c("Analysis 2 - Mean Distance", "Analysis 2 - Var Distance", 
                 "Analysis 2 - Combined Distance"),
    Correlation = c(correlations_analysis2$mean_distance$estimate,
                    correlations_analysis2$var_distance$estimate,
                    correlations_analysis2$combined_distance$estimate),
    P_value = c(correlations_analysis2$mean_distance$p.value,
                correlations_analysis2$var_distance$p.value,
                correlations_analysis2$combined_distance$p.value)
  )
}

# Find best approach
best_correlation <- summary_results[which.max(abs(summary_results$Correlation)), ]
cat("Best approach:", best_correlation$Analysis, 
    "with correlation r =", round(best_correlation$Correlation, 3), 
    "and p-value =", round(best_correlation$P_value, 4), "\n")

# Save summary results
write.csv(summary_results, file.path(output_path, "correlation_summary.csv"), row.names = FALSE)

# ============================================================================
# VISUALIZATIONS
# ============================================================================

cat("\n=== CREATING VISUALIZATIONS ===\n")

# 1. Correlation comparison plot
p1 <- ggplot(summary_results, aes(x = reorder(Analysis, Correlation), y = Correlation)) +
  geom_col(aes(fill = P_value < 0.05), alpha = 0.8) +
  geom_text(aes(label = paste0("r=", round(Correlation, 3))), 
            hjust = ifelse(summary_results$Correlation >= 0, -0.1, 1.1), size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "lightcoral", "TRUE" = "steelblue"),
                    name = "Significant\n(p < 0.05)") +
  labs(title = "Comparison of Correlation Approaches",
       subtitle = "Spearman correlations with human ratings",
       x = "Analysis Method",
       y = "Spearman Correlation") +
  publication_theme

ggsave(file.path(output_path, "correlation_comparison.png"), p1, 
       width = 12, height = 8, dpi = 300)

# 2. Scatter plot: Analysis 2 results
p2 <- ggplot(analysis2_data, aes(x = combined_score, y = human_mean)) +
  geom_point(aes(color = method), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  labs(title = "Analysis 2: Combined Distance Score vs Human Ratings",
       subtitle = paste0("Spearman r = ", round(correlations_analysis2$combined_distance$estimate, 3),
                         ", p = ", round(correlations_analysis2$combined_distance$p.value, 4)),
       x = "Combined Distance Score (Higher = Better)",
       y = "Human Mean Rating",
       color = "Method") +
  publication_theme

ggsave(file.path(output_path, "analysis2_scatter.png"), p2, 
       width = 10, height = 7, dpi = 300)

# 2.5. Analysis 1 scatter plot (if data available)
if (!is.null(analysis1_data) && nrow(analysis1_data) > 0) {
  p2_5 <- ggplot(analysis1_data, aes(x = overall_composite_score, y = human_mean)) +
    geom_point(aes(color = method), size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
    labs(title = "Analysis 1: Overall Composite Score vs Human Ratings",
         subtitle = paste0("Spearman r = ", round(correlations_analysis1$overall_composite$estimate, 3),
                           ", p = ", round(correlations_analysis1$overall_composite$p.value, 4)),
         x = "Overall Composite Score (Z-normalized differences)",
         y = "Human Mean Rating",
         color = "Method") +
    publication_theme
  
  ggsave(file.path(output_path, "analysis1_scatter.png"), p2_5, 
         width = 10, height = 7, dpi = 300)
}

# 3. Method comparison across metrics
metrics_long <- analysis2_data %>%
  select(method, human_mean, coherence_mean, diversity_mean, 
         coherence_variance, diversity_variance) %>%
  pivot_longer(cols = c(coherence_mean, diversity_mean, coherence_variance, diversity_variance),
               names_to = "metric", values_to = "value")

p3 <- ggplot(metrics_long, aes(x = method, y = value, fill = method)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~metric, scales = "free_y") +
  labs(title = "Distribution of Metrics by Method",
       subtitle = "Coherence and Diversity Means and Variances",
       x = "Method",
       y = "Metric Value") +
  publication_theme +
  theme(legend.position = "none")

ggsave(file.path(output_path, "metrics_by_method.png"), p3, 
       width = 12, height = 8, dpi = 300)

# 4. Human ratings vs methods
p4 <- ggplot(data, aes(x = method, y = human_mean, fill = method)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Human Ratings by Method",
       subtitle = "Distribution of human mean ratings across different generation methods",
       x = "Method",
       y = "Human Mean Rating") +
  publication_theme +
  theme(legend.position = "none")

ggsave(file.path(output_path, "human_ratings_by_method.png"), p4, 
       width = 10, height = 7, dpi = 300)

# Save all analysis data
if (!is.null(analysis1_data) && nrow(analysis1_data) > 0) {
  write.csv(analysis1_data, file.path(output_path, "analysis1_data.csv"), row.names = FALSE)
  cat("- analysis1_data.csv\n")
} else {
  cat("- analysis1_data.csv (not created - insufficient data)\n")
}
write.csv(analysis2_data, file.path(output_path, "analysis2_data.csv"), row.names = FALSE)

cat("\nAnalysis complete! Results saved to:", output_path, "\n")
cat("Files created:\n")
cat("- correlation_summary.csv\n")
cat("- analysis2_data.csv\n")
cat("- correlation_comparison.png\n")
cat("- analysis2_scatter.png\n")
if (!is.null(analysis1_data) && nrow(analysis1_data) > 0) {
  cat("- analysis1_scatter.png\n")
}
cat("- metrics_by_method.png\n")
cat("- human_ratings_by_method.png\n")