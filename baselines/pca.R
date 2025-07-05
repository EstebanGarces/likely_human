# Text Quality Analysis - PCA and Correlation Analysis

# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(factoextra)
library(FactoMineR)
library(gridExtra)

# Set working directory and define paths
input_path <- "C:/your_path/text_quality_analysis_results.xlsx"
output_path <- "C:/your_path/"

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

# Load the data
cat("Loading data from Excel file...\n")
data <- read_excel(input_path)

# Display basic information about the dataset
cat("Dataset dimensions:", nrow(data), "rows,", ncol(data), "columns\n")
cat("Column names:", paste(colnames(data), collapse = ", "), "\n")

# Select variables for PCA analysis
pca_vars <- c("coherence_mean", "coherence_variance", "diversity_mean", "diversity_variance")
pca_data <- data %>% 
  select(all_of(pca_vars)) %>%
  na.omit()

cat("PCA analysis will be performed on", nrow(pca_data), "complete observations\n")

# Descriptive statistics
cat("\nDescriptive Statistics:\n")
summary_stats <- pca_data %>%
  summarise_all(list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  ))

print(summary_stats)

# Save descriptive statistics
write.csv(summary_stats, file.path(output_path, "descriptive_statistics.csv"), row.names = FALSE)

# Correlation matrix of PCA variables
correlation_matrix <- cor(pca_data, use = "complete.obs")
cat("\nCorrelation Matrix:\n")
print(round(correlation_matrix, 3))

# Visualize correlation matrix
png(file.path(output_path, "correlation_matrix.png"), width = 10, height = 8, units = "in", res = 300)
corrplot(correlation_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 1.2, tl.col = "black",
         title = "Correlation Matrix of Text Quality Metrics", 
         mar = c(0,0,2,0))
dev.off()

# Perform PCA
cat("\nPerforming PCA analysis...\n")

# Standardize the data before PCA
pca_scaled <- scale(pca_data)

# Perform PCA
pca_result <- PCA(pca_scaled, scale.unit = FALSE, graph = FALSE)

# Extract eigenvalues and variance explained
eigenvalues_raw <- pca_result$eig
variance_percent <- eigenvalues_raw[,2]  # Percentage of variance
cumulative_percent <- eigenvalues_raw[,3]  # Cumulative percentage

# Create a proper eigenvalues data frame
eigenvalues <- data.frame(
  eigenvalue = eigenvalues_raw[,1],
  variance.percent = eigenvalues_raw[,2],
  cumulative.variance.percent = eigenvalues_raw[,3]
)

cat("\nEigenvalues and Variance Explained:\n")
print(eigenvalues)

# Save PCA results
write.csv(eigenvalues, file.path(output_path, "pca_eigenvalues.csv"), row.names = TRUE)

# Variable contributions to principal components
var_contrib <- get_pca_var(pca_result)$contrib
cat("\nVariable Contributions to Principal Components:\n")
print(round(var_contrib, 2))

write.csv(var_contrib, file.path(output_path, "pca_variable_contributions.csv"), row.names = TRUE)

# Individual scores (coordinates) on principal components
individual_scores <- get_pca_ind(pca_result)$coord
colnames(individual_scores) <- paste0("PC", 1:ncol(individual_scores))

# Add original data information
results_df <- data %>%
  filter(!is.na(coherence_mean) & !is.na(coherence_variance) & 
           !is.na(diversity_mean) & !is.na(diversity_variance)) %>%
  bind_cols(as.data.frame(individual_scores))

# Calculate Spearman correlation with human ratings
if ("human_mean" %in% colnames(results_df)) {
  correlations <- sapply(paste0("PC", 1:4), function(pc) {
    cor(results_df[[pc]], results_df$human_mean, method = "spearman", use = "complete.obs")
  })
  
  # Calculate p-values for correlations
  p_values <- sapply(paste0("PC", 1:4), function(pc) {
    test_result <- cor.test(results_df[[pc]], results_df$human_mean, method = "spearman", exact = FALSE)
    test_result$p.value
  })
  
  cat("\nSpearman Correlations with Human Ratings:\n")
  correlation_results <- data.frame(
    Principal_Component = names(correlations),
    Spearman_Correlation = correlations,
    P_Value = p_values,
    Significance = ifelse(p_values < 0.001, "***", 
                          ifelse(p_values < 0.01, "**", 
                                 ifelse(p_values < 0.05, "*", "ns")))
  )
  print(correlation_results)
  
  write.csv(correlation_results, file.path(output_path, "spearman_correlations.csv"), row.names = FALSE)
  
  # Generate LaTeX table for correlations
  latex_table <- paste0(
    "\\begin{table}[h!]\n",
    "\\centering\n",
    "\\caption{Spearman Correlations between Principal Components and Human Ratings}\n",
    "\\label{tab:correlations}\n",
    "\\begin{tabular}{lrrr}\n",
    "\\toprule\n",
    "Principal Component & Correlation ($\\rho$) & P-value & Significance \\\\\n",
    "\\midrule\n"
  )
  
  for(i in 1:nrow(correlation_results)) {
    latex_table <- paste0(latex_table,
                          correlation_results$Principal_Component[i], " & ",
                          sprintf("%.3f", correlation_results$Spearman_Correlation[i]), " & ",
                          sprintf("%.4f", correlation_results$P_Value[i]), " & ",
                          correlation_results$Significance[i], " \\\\\n"
    )
  }
  
  latex_table <- paste0(latex_table,
                        "\\bottomrule\n",
                        "\\end{tabular}\n",
                        "\\end{table}\n"
  )
  
  writeLines(latex_table, file.path(output_path, "correlation_table.tex"))
  
} else {
  cat("\nWarning: 'human_mean' column not found for correlation analysis\n")
}

# Save individual scores
write.csv(results_df, file.path(output_path, "pca_individual_scores.csv"), row.names = FALSE)

# Visualization 1: Scree plot
scree_plot <- fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100)) +
  ggtitle("Scree Plot: Variance Explained by Principal Components") +
  publication_theme

ggsave(file.path(output_path, "scree_plot.png"), scree_plot, 
       width = 10, height = 6, dpi = 300)

# Visualization 2: PCA Biplot
biplot <- fviz_pca_biplot(pca_result, 
                          repel = TRUE,
                          col.var = "contrib",
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          title = "PCA Biplot: Variables and Observations") +
  publication_theme

ggsave(file.path(output_path, "pca_biplot.png"), biplot, 
       width = 12, height = 8, dpi = 300)

# Visualization 3: Variable contributions to PC1 and PC2
contrib_pc1 <- fviz_contrib(pca_result, choice = "var", axes = 1, top = 10) +
  ggtitle("Variable Contributions to PC1") +
  publication_theme

contrib_pc2 <- fviz_contrib(pca_result, choice = "var", axes = 2, top = 10) +
  ggtitle("Variable Contributions to PC2") +
  publication_theme

combined_contrib <- grid.arrange(contrib_pc1, contrib_pc2, ncol = 2)

ggsave(file.path(output_path, "variable_contributions.png"), combined_contrib, 
       width = 14, height = 6, dpi = 300)

# Visualization 4: Individual factor map colored by method (if available)
if ("method" %in% colnames(results_df)) {
  # Generate enough colors for all methods
  n_methods <- length(unique(results_df$method))
  colors_palette <- c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF", 
                      "#443A83FF", "#21908CFF", "#5DC863FF", "#FFEA46FF")[1:n_methods]
  
  individual_plot <- fviz_pca_ind(pca_result,
                                  col.ind = results_df$method,
                                  palette = colors_palette,
                                  addEllipses = TRUE,
                                  ellipse.level = 0.68,
                                  legend.title = "Method",
                                  title = "PCA Individual Factor Map by Method") +
    publication_theme
  
  ggsave(file.path(output_path, "individuals_by_method.png"), individual_plot, 
         width = 12, height = 8, dpi = 300)
}

# Visualization 5: PC1 vs PC2 scatter plot with human ratings
if ("human_mean" %in% colnames(results_df)) {
  pc_scatter <- ggplot(results_df, aes(x = PC1, y = PC2, color = human_mean)) +
    geom_point(size = 3, alpha = 0.7) +
    scale_color_gradient(low = "#440154FF", high = "#FDE725FF", name = "Human\nRating") +
    labs(
      title = "Principal Components vs Human Ratings",
      subtitle = "PC1 vs PC2 colored by human rating scores",
      x = paste0("PC1 (", round(eigenvalues$variance.percent[1], 1), "% variance)"),
      y = paste0("PC2 (", round(eigenvalues$variance.percent[2], 1), "% variance)")
    ) +
    publication_theme
  
  ggsave(file.path(output_path, "pc_vs_human_ratings.png"), pc_scatter, 
         width = 10, height = 8, dpi = 300)
}

# Create a comprehensive summary report
summary_report <- list(
  "Dataset Info" = list(
    "Total Observations" = nrow(data),
    "Complete Cases for PCA" = nrow(pca_data),
    "Variables Analyzed" = pca_vars
  ),
  "PCA Results" = list(
    "PC1 Variance Explained" = paste0(round(eigenvalues$variance.percent[1], 2), "%"),
    "PC2 Variance Explained" = paste0(round(eigenvalues$variance.percent[2], 2), "%"),
    "Cumulative Variance (PC1+PC2)" = paste0(round(sum(eigenvalues$variance.percent[1:2]), 2), "%"),
    "Total Components" = nrow(eigenvalues)
  )
)

if ("human_mean" %in% colnames(results_df)) {
  summary_report$"Correlation Analysis" <- list(
    "Strongest PC-Human Correlation" = paste0("PC", which.max(abs(correlations)), 
                                              " (r = ", round(max(abs(correlations)), 3), 
                                              ", p = ", round(p_values[which.max(abs(correlations))], 4), ")"),
    "All Correlations" = correlations,
    "All P-Values" = p_values
  )
}

# Save summary report
sink(file.path(output_path, "analysis_summary.txt"))
cat("TEXT QUALITY PCA ANALYSIS SUMMARY\n")
cat(paste(rep("=", 50), collapse = ""), "\n\n")

for (section in names(summary_report)) {
  cat(section, "\n")
  cat(paste(rep("-", nchar(section)), collapse = ""), "\n")
  for (item in names(summary_report[[section]])) {
    cat(item, ":", summary_report[[section]][[item]], "\n")
  }
  cat("\n")
}

cat("INTERPRETATION:\n")
cat(paste(rep("-", 15), collapse = ""), "\n")
cat("1. PC1 explains", round(eigenvalues$variance.percent[1], 1), "% of total variance\n")
cat("2. PC2 explains", round(eigenvalues$variance.percent[2], 1), "% of total variance\n")
cat("3. Together, PC1 and PC2 explain", round(sum(eigenvalues$variance.percent[1:2]), 1), "% of total variance\n\n")

if ("human_mean" %in% colnames(results_df)) {
  best_pc <- which.max(abs(correlations))
  cat("4. PC", best_pc, "shows the strongest correlation with human ratings (r =", 
      round(correlations[best_pc], 3), ", p =", round(p_values[best_pc], 4), ")\n")
}

cat("\nFILES GENERATED:\n")
cat(paste(rep("-", 15), collapse = ""), "\n")
cat("- descriptive_statistics.csv\n")
cat("- correlation_matrix.png\n")
cat("- pca_eigenvalues.csv\n")
cat("- pca_variable_contributions.csv\n")
cat("- pca_individual_scores.csv\n")
cat("- spearman_correlations.csv\n")
cat("- scree_plot.png\n")
cat("- pca_biplot.png\n")
cat("- variable_contributions.png\n")
cat("- individuals_by_method.png\n")
cat("- pc_vs_human_ratings.png\n")
cat("- analysis_summary.txt\n")

sink()

cat("Analysis completed successfully!\n")
cat("All results saved to:", output_path, "\n")