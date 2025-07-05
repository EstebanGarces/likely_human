# Simplified DTW Analysis Script with Normalization - Coherence and Diversity Focus
# Implements sequence length normalization + within-dataset z-score standardization

library(readxl)
library(dtw)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Set working directory and read data
setwd("yourrepository")
data <- read_excel("text_quality_analysis_results.xlsx")
data <- as.data.frame(data)

print("=== STARTING DTW ANALYSIS WITH NORMALIZATION ===")
print(paste("Loaded data with", nrow(data), "rows and", ncol(data), "columns"))
print("Methods available in the data:")
print(unique(data$method))

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

# Function to parse time series (handles Python/numpy format)
parse_ts <- function(ts_string) {
  if (is.na(ts_string) || ts_string == "") {
    return(numeric(0))
  }
  
  # Remove brackets and numpy formatting
  ts_string <- gsub("\\[|\\]", "", ts_string)
  ts_string <- gsub("np\\.float64\\(|\\)", "", ts_string)
  
  # Split by comma and convert to numeric
  ts_values <- strsplit(ts_string, ", ")[[1]]
  ts_values <- gsub("^\\s+|\\s+$", "", ts_values)  # trim whitespace
  
  # Convert to numeric, handling any remaining issues
  numeric_values <- suppressWarnings(as.numeric(ts_values))
  
  # Remove any NA values that might have been introduced
  numeric_values <- numeric_values[!is.na(numeric_values)]
  
  return(numeric_values)
}

# Debug: Check data structure
print("Data structure:")
print(str(data))
print("\nColumn names:")
print(names(data))
print("\nMethods in data:")
print(unique(data$method))

# Parse time series for all rows
print("\nParsing time series...")
data$coherence_parsed <- lapply(data$coherence_ts, parse_ts)
data$diversity_parsed <- lapply(data$diversity_ts, parse_ts)

# Check if parsing was successful
print("Parsing results:")
print(paste("Coherence parsed length:", length(data$coherence_parsed)))
print(paste("Diversity parsed length:", length(data$diversity_parsed)))

# Get human baseline (first human observation for examples)
human_indices <- which(data$method == "Human")
print(paste("Human indices found:", paste(human_indices, collapse = ", ")))

if (length(human_indices) == 0) {
  stop("No 'Human' method found in data. Available methods: ", paste(unique(data$method), collapse=", "))
}

# Use first human observation for time series examples
human_example_idx <- human_indices[1]
human_coherence_example <- data$coherence_parsed[[human_example_idx]]
human_diversity_example <- data$diversity_parsed[[human_example_idx]]

print(paste("Using human observation", human_example_idx, "for time series examples"))
print(paste("Human coherence example length:", length(human_coherence_example)))
print(paste("Human diversity example length:", length(human_diversity_example)))

# Modified function to compute DTW with normalization
compute_normalized_dtw <- function(human_ts, llm_ts) {
  # Perform DTW
  dtw_result <- dtw(human_ts, llm_ts, keep=TRUE)
  
  # Step 1: Length normalization
  # Using average of sequence lengths
  avg_length <- (length(human_ts) + length(llm_ts)) / 2
  dtw_length_normalized <- dtw_result$distance / avg_length
  
  # Return both raw and length-normalized distances, plus length info
  return(list(
    raw_distance = dtw_result$distance,
    length_normalized = dtw_length_normalized,
    human_length = length(human_ts),
    llm_length = length(llm_ts),
    avg_length = avg_length,
    dtw_result = dtw_result
  ))
}

# Calculate global y-axis limits for consistent plotting
print("\nCalculating global y-axis limits...")
all_coherence_values <- c()
all_diversity_values <- c()

# Collect all parsed time series values
for (i in 1:6) { # check limits
  if (length(data$coherence_parsed[[i]]) > 0) {
    all_coherence_values <- c(all_coherence_values, data$coherence_parsed[[i]])
  }
  if (length(data$diversity_parsed[[i]]) > 0) {
    all_diversity_values <- c(all_diversity_values, data$diversity_parsed[[i]])
  }
}

# Calculate limits with some padding
coherence_ylim <- c(min(all_coherence_values, na.rm = TRUE) * 0.95, 
                    max(all_coherence_values, na.rm = TRUE) * 1.05)
diversity_ylim <- c(min(all_diversity_values, na.rm = TRUE) * 0.95, 
                    max(all_diversity_values, na.rm = TRUE) * 1.05)

print(paste("Coherence y-axis limits:", round(coherence_ylim[1], 3), "to", round(coherence_ylim[2], 3)))
print(paste("Diversity y-axis limits:", round(diversity_ylim[1], 3), "to", round(diversity_ylim[2], 3)))

# Function to create DTW alignment plot (updated with consistent y-axis limits)
create_dtw_plot <- function(human_ts, llm_ts, method_name, metric_name, dtw_data, ylim_range) {
  # Create data frame for plotting
  plot_data <- data.frame(
    Time = c(1:length(human_ts), 1:length(llm_ts)),
    Value = c(human_ts, llm_ts),
    Series = c(rep("Human", length(human_ts)), rep(method_name, length(llm_ts)))
  )
  
  # Clean method name for display
  clean_method_name <- gsub("\\(|\\)", "", method_name)
  clean_method_name <- gsub("temp", "Temperature", clean_method_name)
  clean_method_name <- gsub("topp", "Top-p", clean_method_name)
  
  # Create the plot with publication-ready styling and consistent y-axis
  p <- ggplot(plot_data, aes(x = Time, y = Value, color = Series)) +
    geom_line(size = 1.5, alpha = 0.8) +
    labs(
      title = paste("DTW Alignment Analysis:", metric_name),
      subtitle = paste("Human vs", clean_method_name, 
                       "| Normalized DTW Distance:", round(dtw_data$length_normalized, 3)),
      x = "Time Point", 
      y = paste(metric_name, "Score"),
      color = "Method"
    ) +
    publication_theme +
    scale_color_manual(
      values = c("Human" = "#2E86AB", method_name = "#A23B72"),
      labels = c("Human", clean_method_name)
    ) +
    ylim(ylim_range)  # Apply consistent y-axis limits
  
  # Add alignment lines (sample every 15th point to avoid overcrowding)
  alignment_indices <- seq(1, length(dtw_data$dtw_result$index1), by = 15)
  for (i in alignment_indices) {
    idx1 <- dtw_data$dtw_result$index1[i]
    idx2 <- dtw_data$dtw_result$index2[i]
    p <- p + geom_segment(
      x = idx1, y = human_ts[idx1], 
      xend = idx2, yend = llm_ts[idx2], 
      alpha = 0.4, color = "gray60", linetype = "dashed", size = 0.5
    )
  }
  
  return(p)
}

# Analyze each LLM method using ALL observations for summary statistics
unique_methods <- unique(data$method)
llm_methods <- unique_methods[unique_methods != "Human"]
print(paste("LLM methods found:", paste(llm_methods, collapse=", ")))

results_all <- list()  # For all observations
results_examples <- list()  # For first observation examples
plot_list <- list()  # Store all plots for saving

# Initialize data frame to store individual observation results for human-DTW alignment analysis
dtw_human_alignment_data <- data.frame(
  Method = character(),
  Observation_ID = integer(),
  Human_Rating = numeric(),
  Coherence_DTW_Length_Norm = numeric(),
  Diversity_DTW_Length_Norm = numeric(),
  Coherence_DTW_Z_Score = numeric(),
  Diversity_DTW_Z_Score = numeric(),
  stringsAsFactors = FALSE
)

for (method in llm_methods) {
  print(paste("Processing method:", method))
  
  method_indices <- which(data$method == method)
  
  if (length(method_indices) == 0) {
    print(paste("Warning: Method", method, "not found"))
    next
  }
  
  print(paste("  Found", length(method_indices), "observations for", method))
  
  # === PROCESS ALL OBSERVATIONS FOR SUMMARY STATISTICS ===
  coherence_dtw_all <- list()
  diversity_dtw_all <- list()
  
  valid_obs_count <- 0
  
  for (i in seq_along(method_indices)) {
    obs_idx <- method_indices[i]
    
    # Get all human observations for comparison
    human_obs_coherence <- list()
    human_obs_diversity <- list()
    
    for (h_idx in human_indices) {
      if (length(data$coherence_parsed[[h_idx]]) > 0 && length(data$diversity_parsed[[h_idx]]) > 0) {
        human_obs_coherence[[length(human_obs_coherence) + 1]] <- data$coherence_parsed[[h_idx]]
        human_obs_diversity[[length(human_obs_diversity) + 1]] <- data$diversity_parsed[[h_idx]]
      }
    }
    
    # Get LLM time series for this observation
    llm_coherence <- data$coherence_parsed[[obs_idx]]
    llm_diversity <- data$diversity_parsed[[obs_idx]]
    
    # Check if parsing was successful
    if (length(llm_coherence) == 0 || length(llm_diversity) == 0) {
      print(paste("Warning: Failed to parse time series for", method, "observation", i))
      next
    }
    
    # Get human rating for this observation
    human_rating <- data$human_mean[obs_idx]
    
    # Check if human rating is available
    if (is.na(human_rating)) {
      print(paste("Warning: No human rating available for", method, "observation", i))
      next
    }
    
    # Calculate DTW against all human observations and take average
    tryCatch({
      coherence_distances <- sapply(human_obs_coherence, function(h_ts) {
        compute_normalized_dtw(h_ts, llm_coherence)$length_normalized
      })
      
      diversity_distances <- sapply(human_obs_diversity, function(h_ts) {
        compute_normalized_dtw(h_ts, llm_diversity)$length_normalized
      })
      
      avg_coherence_dtw <- mean(coherence_distances, na.rm = TRUE)
      avg_diversity_dtw <- mean(diversity_distances, na.rm = TRUE)
      
      coherence_dtw_all[[i]] <- avg_coherence_dtw
      diversity_dtw_all[[i]] <- avg_diversity_dtw
      
      # Store individual observation data for human-DTW alignment analysis
      new_row <- data.frame(
        Method = method,
        Observation_ID = obs_idx,
        Human_Rating = human_rating,
        Coherence_DTW_Length_Norm = avg_coherence_dtw,
        Diversity_DTW_Length_Norm = avg_diversity_dtw,
        Coherence_DTW_Z_Score = NA,  # Will be calculated after all observations
        Diversity_DTW_Z_Score = NA,  # Will be calculated after all observations
        stringsAsFactors = FALSE
      )
      
      dtw_human_alignment_data <- rbind(dtw_human_alignment_data, new_row)
      
      valid_obs_count <- valid_obs_count + 1
      
    }, error = function(e) {
      print(paste("Error processing", method, "observation", i, ":", e$message))
    })
  }
  
  # Calculate summary statistics across all observations
  if (valid_obs_count > 0) {
    results_all[[method]] <- list(
      coherence_mean = mean(unlist(coherence_dtw_all), na.rm = TRUE),
      coherence_sd = sd(unlist(coherence_dtw_all), na.rm = TRUE),
      coherence_median = median(unlist(coherence_dtw_all), na.rm = TRUE),
      diversity_mean = mean(unlist(diversity_dtw_all), na.rm = TRUE),
      diversity_sd = sd(unlist(diversity_dtw_all), na.rm = TRUE),
      diversity_median = median(unlist(diversity_dtw_all), na.rm = TRUE),
      n_observations = valid_obs_count
    )
  }
  
  # === PROCESS FIRST OBSERVATION FOR EXAMPLE PLOTS ===
  first_obs_idx <- method_indices[1]
  llm_coherence_example <- data$coherence_parsed[[first_obs_idx]]
  llm_diversity_example <- data$diversity_parsed[[first_obs_idx]]
  
  # Check if parsing was successful for examples
  if (length(llm_coherence_example) == 0 || length(llm_diversity_example) == 0) {
    print(paste("Warning: Failed to parse time series for", method, "first observation"))
    next
  }
  
  print(paste("  Example coherence length:", length(llm_coherence_example)))
  print(paste("  Example diversity length:", length(llm_diversity_example)))
  
  # Compute DTW for examples (using first human observation)
  tryCatch({
    coherence_dtw_example <- compute_normalized_dtw(human_coherence_example, llm_coherence_example)
    diversity_dtw_example <- compute_normalized_dtw(human_diversity_example, llm_diversity_example)
    
    # Create DTW plots with consistent y-axis limits
    coherence_plot <- create_dtw_plot(human_coherence_example, llm_coherence_example, 
                                      method, "Coherence", coherence_dtw_example, coherence_ylim)
    diversity_plot <- create_dtw_plot(human_diversity_example, llm_diversity_example, 
                                      method, "Diversity", diversity_dtw_example, diversity_ylim)
    
    # Store example results
    results_examples[[method]] <- list(
      coherence = coherence_dtw_example,
      diversity = diversity_dtw_example
    )
    
    # Store plots for saving
    plot_list[[paste0(method, "_coherence")]] <- coherence_plot
    plot_list[[paste0(method, "_diversity")]] <- diversity_plot
    
    # Display plots
    print(coherence_plot)
    print(diversity_plot)
    
    # Save individual plots with descriptive filenames and higher resolution
    coherence_filename <- paste0("DTW_", gsub("[^A-Za-z0-9]", "_", method), "_Coherence_Publication.png")
    diversity_filename <- paste0("DTW_", gsub("[^A-Za-z0-9]", "_", method), "_Diversity_Publication.png")
    
    ggsave(filename = coherence_filename, 
           plot = coherence_plot, 
           width = 14, height = 10, dpi = 300)
    
    ggsave(filename = diversity_filename, 
           plot = diversity_plot, 
           width = 14, height = 10, dpi = 300)
    
    print(paste("Saved plots:", coherence_filename, "and", diversity_filename))
    
  }, error = function(e) {
    print(paste("Error processing", method, "examples:", e$message))
  })
}

# Create comprehensive summary using ALL observations
if (length(results_all) == 0) {
  stop("No methods were successfully processed. Please check your data format.")
}

valid_methods <- names(results_all)

# Create summary with statistics from all observations
distance_summary_all <- data.frame(
  Method = valid_methods,
  Coherence_DTW_Mean = sapply(results_all, function(x) x$coherence_mean),
  Coherence_DTW_SD = sapply(results_all, function(x) x$coherence_sd),
  Coherence_DTW_Median = sapply(results_all, function(x) x$coherence_median),
  Diversity_DTW_Mean = sapply(results_all, function(x) x$diversity_mean),
  Diversity_DTW_SD = sapply(results_all, function(x) x$diversity_sd),
  Diversity_DTW_Median = sapply(results_all, function(x) x$diversity_median),
  N_Observations = sapply(results_all, function(x) x$n_observations)
)

# Apply z-score normalization using means
distance_summary_all$Coherence_DTW_Z_Score <- scale(distance_summary_all$Coherence_DTW_Mean)[,1]
distance_summary_all$Diversity_DTW_Z_Score <- scale(distance_summary_all$Diversity_DTW_Mean)[,1]

# Calculate z-scores for individual observations in alignment data
if (nrow(dtw_human_alignment_data) > 0) {
  dtw_human_alignment_data$Coherence_DTW_Z_Score <- scale(dtw_human_alignment_data$Coherence_DTW_Length_Norm)[,1]
  dtw_human_alignment_data$Diversity_DTW_Z_Score <- scale(dtw_human_alignment_data$Diversity_DTW_Length_Norm)[,1]
  
  print("\n=== HUMAN-DTW ALIGNMENT ANALYSIS ===")
  print(paste("Total observations for alignment analysis:", nrow(dtw_human_alignment_data)))
  
  # Calculate correlations
  coherence_length_norm_cor <- cor(dtw_human_alignment_data$Human_Rating, 
                                   dtw_human_alignment_data$Coherence_DTW_Length_Norm, 
                                   use = "complete.obs")
  
  coherence_zscore_cor <- cor(dtw_human_alignment_data$Human_Rating, 
                              dtw_human_alignment_data$Coherence_DTW_Z_Score, 
                              use = "complete.obs")
  
  diversity_length_norm_cor <- cor(dtw_human_alignment_data$Human_Rating, 
                                   dtw_human_alignment_data$Diversity_DTW_Length_Norm, 
                                   use = "complete.obs")
  
  diversity_zscore_cor <- cor(dtw_human_alignment_data$Human_Rating, 
                              dtw_human_alignment_data$Diversity_DTW_Z_Score, 
                              use = "complete.obs")
  
  # Create correlation summary
  correlation_summary <- data.frame(
    Metric = c("Coherence DTW (Length Normalized)", 
               "Coherence DTW (Z-Score)", 
               "Diversity DTW (Length Normalized)", 
               "Diversity DTW (Z-Score)"),
    Correlation_with_Human_Rating = c(coherence_length_norm_cor, 
                                      coherence_zscore_cor, 
                                      diversity_length_norm_cor, 
                                      diversity_zscore_cor),
    Expected_Direction = c("Negative", "Negative", "Negative", "Negative"),
    Interpretation = c("Lower DTW → Higher Human Rating", 
                       "Lower DTW → Higher Human Rating", 
                       "Lower DTW → Higher Human Rating", 
                       "Lower DTW → Higher Human Rating")
  )
  
  print("Correlation Analysis:")
  correlation_display <- correlation_summary[, c("Metric", "Correlation_with_Human_Rating")]
  correlation_display$Correlation_with_Human_Rating <- round(correlation_display$Correlation_with_Human_Rating, 3)
  print(correlation_display)
  
  # Create visualization for human-DTW alignment
  create_alignment_plot <- function(dtw_data, dtw_col, metric_name, normalization_type) {
    dtw_data$Method_Clean <- gsub("\\(|\\)", "", dtw_data$Method)
    dtw_data$Method_Clean <- gsub("temp", "Temperature", dtw_data$Method_Clean)
    dtw_data$Method_Clean <- gsub("topp", "Top-p", dtw_data$Method_Clean)
    
    correlation_value <- cor(dtw_data$Human_Rating, dtw_data[[dtw_col]], use = "complete.obs")
    
    ggplot(dtw_data, aes(x = !!sym(dtw_col), y = Human_Rating, color = Method_Clean)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", alpha = 0.5) +
      labs(
        title = paste("Human Rating vs DTW Distance:", metric_name),
        subtitle = paste("Normalization:", normalization_type, "| Correlation: r =", round(correlation_value, 3)),
        x = paste("DTW Distance", paste0("(", normalization_type, ")")),
        y = "Human Rating",
        color = "Method"
      ) +
      publication_theme +
      scale_color_viridis_d(option = "plasma", alpha = 0.8) +
      theme(legend.position = "right")
  }
  
  # Create alignment plots
  coherence_length_plot <- create_alignment_plot(dtw_human_alignment_data, 
                                                 "Coherence_DTW_Length_Norm", 
                                                 "Coherence", 
                                                 "Length Normalized")
  
  coherence_zscore_plot <- create_alignment_plot(dtw_human_alignment_data, 
                                                 "Coherence_DTW_Z_Score", 
                                                 "Coherence", 
                                                 "Z-Score")
  
  diversity_length_plot <- create_alignment_plot(dtw_human_alignment_data, 
                                                 "Diversity_DTW_Length_Norm", 
                                                 "Diversity", 
                                                 "Length Normalized")
  
  diversity_zscore_plot <- create_alignment_plot(dtw_human_alignment_data, 
                                                 "Diversity_DTW_Z_Score", 
                                                 "Diversity", 
                                                 "Z-Score")
  
  # Display plots
  print(coherence_length_plot)
  print(coherence_zscore_plot)
  print(diversity_length_plot)
  print(diversity_zscore_plot)
  
  # Save alignment plots
  ggsave(filename = "Human_DTW_Alignment_Coherence_Length_Normalized.png", 
         plot = coherence_length_plot, 
         width = 14, height = 10, dpi = 300)
  
  ggsave(filename = "Human_DTW_Alignment_Coherence_Z_Score.png", 
         plot = coherence_zscore_plot, 
         width = 14, height = 10, dpi = 300)
  
  ggsave(filename = "Human_DTW_Alignment_Diversity_Length_Normalized.png", 
         plot = diversity_length_plot, 
         width = 14, height = 10, dpi = 300)
  
  ggsave(filename = "Human_DTW_Alignment_Diversity_Z_Score.png", 
         plot = diversity_zscore_plot, 
         width = 14, height = 10, dpi = 300)
  
  # Create combined correlation plot
  correlation_plot_data <- correlation_summary %>%
    mutate(
      Metric_Short = c("Coherence\n(Length Norm)", 
                       "Coherence\n(Z-Score)", 
                       "Diversity\n(Length Norm)", 
                       "Diversity\n(Z-Score)"),
      Abs_Correlation = abs(Correlation_with_Human_Rating),
      Direction = ifelse(Correlation_with_Human_Rating < 0, "Negative", "Positive")
    )
  
  correlation_summary_plot <- ggplot(correlation_plot_data, 
                                     aes(x = reorder(Metric_Short, Abs_Correlation), 
                                         y = Correlation_with_Human_Rating, 
                                         fill = Direction)) +
    geom_bar(stat = "identity", alpha = 0.8, width = 0.7) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.8) +
    geom_text(aes(label = round(Correlation_with_Human_Rating, 3)), 
              hjust = ifelse(correlation_plot_data$Correlation_with_Human_Rating < 0, 1.1, -0.1),
              color = "black", size = 4, fontface = "bold") +
    labs(
      title = "Human Rating - DTW Distance Correlations",
      subtitle = "Negative correlations indicate better alignment (lower DTW = higher human rating)",
      x = "DTW Metric",
      y = "Correlation with Human Rating",
      fill = "Correlation\nDirection"
    ) +
    publication_theme +
    scale_fill_manual(values = c("Negative" = "#2E86AB", "Positive" = "#F24236")) +
    coord_flip()
  
  print(correlation_summary_plot)
  
  ggsave(filename = "Human_DTW_Correlation_Summary.png", 
         plot = correlation_summary_plot, 
         width = 14, height = 10, dpi = 300)
  
  # Save alignment data and correlation summary
  write.csv(dtw_human_alignment_data, "DTW_Human_Alignment_Data.csv", row.names = FALSE)
  write.csv(correlation_summary, "DTW_Human_Correlation_Summary.csv", row.names = FALSE)
  
} else {
  print("Warning: No data available for human-DTW alignment analysis")
}

# Display comprehensive summary
print("=== COMPREHENSIVE DTW DISTANCE SUMMARY (ALL OBSERVATIONS) ===")
print("Mean, SD, Median, and Z-Score Normalized Distances:")

# Create a copy for display with rounded numeric columns
display_summary_all <- distance_summary_all
numeric_cols <- sapply(display_summary_all, is.numeric)
display_summary_all[numeric_cols] <- round(display_summary_all[numeric_cols], 3)
print(display_summary_all)

# Create improved comparison plots using ALL observations data
create_comparison_plot_all <- function(data, mean_col, sd_col, zscore_col, metric_name) {
  plot_data <- data %>%
    select(Method, !!sym(mean_col), !!sym(sd_col), !!sym(zscore_col)) %>%
    mutate(
      Method_Clean = gsub("\\(|\\)", "", Method),
      Method_Clean = gsub("temp", "Temperature", Method_Clean),
      Method_Clean = gsub("topp", "Top-p", Method_Clean)
    )
  
  # Create separate plots for mean, SD, and z-score
  mean_plot <- ggplot(plot_data, aes(x = reorder(Method_Clean, !!sym(mean_col)), y = !!sym(mean_col))) +
    geom_bar(stat = "identity", fill = "#E63946", alpha = 0.8, width = 0.7) +
    geom_errorbar(aes(ymin = !!sym(mean_col) - !!sym(sd_col), 
                      ymax = !!sym(mean_col) + !!sym(sd_col)), 
                  width = 0.2, alpha = 0.7) +
    labs(title = paste("Mean DTW Distance:", metric_name),
         subtitle = "Error bars show ± 1 SD across all observations",
         x = "Decoding Strategy", y = "Mean DTW Distance") +
    publication_theme +
    coord_flip()
  
  zscore_plot <- ggplot(plot_data, aes(x = reorder(Method_Clean, !!sym(zscore_col)), y = !!sym(zscore_col))) +
    geom_bar(stat = "identity", fill = "#FCBF49", alpha = 0.8, width = 0.7) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.8) +
    geom_hline(yintercept = c(-1, 1), linetype = "dashed", alpha = 0.6, color = "grey40") +
    labs(title = paste("Z-Score Normalized DTW Distance:", metric_name),
         subtitle = "Standardized across all methods and observations",
         x = "Decoding Strategy", y = "Z-Score") +
    publication_theme +
    coord_flip()
  
  return(list(mean_plot = mean_plot, zscore_plot = zscore_plot))
}

# Create comparison plots using all observations
coherence_plots_all <- create_comparison_plot_all(distance_summary_all, 
                                                  "Coherence_DTW_Mean", 
                                                  "Coherence_DTW_SD",
                                                  "Coherence_DTW_Z_Score", 
                                                  "Coherence")

diversity_plots_all <- create_comparison_plot_all(distance_summary_all, 
                                                  "Diversity_DTW_Mean", 
                                                  "Diversity_DTW_SD",
                                                  "Diversity_DTW_Z_Score", 
                                                  "Diversity")

# Print the plots
print(coherence_plots_all$mean_plot)
print(coherence_plots_all$zscore_plot)
print(diversity_plots_all$mean_plot)
print(diversity_plots_all$zscore_plot)

# Save comparison plots with higher resolution
ggsave(filename = "DTW_Coherence_Normalization_Comparison_Publication.png", 
       plot = coherence_plots_all$zscore_plot, 
       width = 18, height = 12, dpi = 300)

ggsave(filename = "DTW_Diversity_Normalization_Comparison_Publication.png", 
       plot = diversity_plots_all$zscore_plot, 
       width = 18, height = 12, dpi = 300)

# Final normalized distance plot (using z-scores from all observations)
final_normalized_data_all <- distance_summary_all %>%
  select(Method, Coherence_DTW_Z_Score, Diversity_DTW_Z_Score) %>%
  pivot_longer(cols = c(Coherence_DTW_Z_Score, Diversity_DTW_Z_Score), 
               names_to = "Metric", values_to = "Z_Score") %>%
  mutate(
    Metric = gsub("_DTW_Z_Score", "", Metric),
    Method_Clean = gsub("\\(|\\)", "", Method),
    Method_Clean = gsub("temp", "Temperature", Method_Clean),
    Method_Clean = gsub("topp", "Top-p", Method_Clean)
  )

final_plot_all <- ggplot(final_normalized_data_all, aes(x = reorder(Method_Clean, Z_Score), y = Z_Score, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8, width = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.8) +
  geom_hline(yintercept = c(-1, 1), linetype = "dashed", alpha = 0.6, color = "grey40") +
  labs(
    title = "Normalized DTW Distance Analysis (All Observations)",
    subtitle = "Z-Score Standardized Distances (Lower is Better)",
    x = "Decoding Strategy", 
    y = "Z-Score\n(Standard Deviations from Mean)", 
    fill = "Text Quality\nMetric"
  ) +
  publication_theme +
  scale_fill_manual(values = c("Coherence" = "#264653", "Diversity" = "#E76F51")) +
  coord_flip() +
  annotate("text", x = Inf, y = -1, label = "Better than average", 
           hjust = 1.1, vjust = -0.5, size = 4, color = "grey50") +
  annotate("text", x = Inf, y = 1, label = "Worse than average", 
           hjust = 1.1, vjust = 1.5, size = 4, color = "grey50")

print(final_plot_all)

ggsave(filename = "DTW_Final_Normalized_Comparison_Publication.png", 
       plot = final_plot_all, 
       width = 14, height = 10, dpi = 300)

# Find best performing methods using all observations
best_methods_all <- data.frame(
  Metric = c("Coherence", "Diversity"),
  Best_Method_Mean = c(
    distance_summary_all$Method[which.min(distance_summary_all$Coherence_DTW_Mean)],
    distance_summary_all$Method[which.min(distance_summary_all$Diversity_DTW_Mean)]
  ),
  Best_Method_Z_Score = c(
    distance_summary_all$Method[which.min(distance_summary_all$Coherence_DTW_Z_Score)],
    distance_summary_all$Method[which.min(distance_summary_all$Diversity_DTW_Z_Score)]
  ),
  Best_Mean_Distance = c(
    min(distance_summary_all$Coherence_DTW_Mean),
    min(distance_summary_all$Diversity_DTW_Mean)
  ),
  Best_Z_Score = c(
    min(distance_summary_all$Coherence_DTW_Z_Score),
    min(distance_summary_all$Diversity_DTW_Z_Score)
  )
)

print("=== BEST PERFORMING METHODS (ALL OBSERVATIONS) ===")
print(best_methods_all)

# Save all results
write.csv(distance_summary_all, "DTW_Comprehensive_Distance_Summary.csv", row.names = FALSE)
write.csv(best_methods_all, "DTW_Best_Methods_Normalized.csv", row.names = FALSE)

print("\n=== FILES SAVED ===")
print("Publication-ready files saved to your working directory:")
if (length(results_examples) > 0) {
  for (method in names(results_examples)) {
    clean_method <- gsub("[^A-Za-z0-9]", "_", method)
    print(paste("-", paste0("DTW_", clean_method, "_Coherence_Publication.png")))
    print(paste("-", paste0("DTW_", clean_method, "_Diversity_Publication.png")))
  }
  print("- DTW_Coherence_Normalization_Comparison_Publication.png")
  print("- DTW_Diversity_Normalization_Comparison_Publication.png") 
  print("- DTW_Final_Normalized_Comparison_Publication.png")
  print("- DTW_Comprehensive_Distance_Summary.csv")
  print("- DTW_Best_Methods_Normalized.csv")
  
  # Human-DTW alignment analysis files
  if (nrow(dtw_human_alignment_data) > 0) {
    print("\nHuman-DTW Alignment Analysis Files:")
    print("- DTW_Human_Alignment_Data.csv")
    print("- DTW_Human_Correlation_Summary.csv")
    print("- Human_DTW_Alignment_Coherence_Length_Normalized.png")
    print("- Human_DTW_Alignment_Coherence_Z_Score.png")
    print("- Human_DTW_Alignment_Diversity_Length_Normalized.png")
    print("- Human_DTW_Alignment_Diversity_Z_Score.png")
    print("- Human_DTW_Correlation_Summary.png")
  }
}
print(paste("Working directory:", getwd()))

print("\n=== ANALYSIS SUMMARY ===")
print("Data usage strategy:")
print("- Summary statistics and comparison plots: ALL observations in dataset")
print("- Individual time series plots: First observation only (as examples)")
print("- Y-axis limits: Consistent across all time series plots for comparability")
print("\nHuman-DTW Alignment Analysis:")
print("- Correlates DTW distances with human ratings (human_mean column)")
print("- Negative correlations are expected (lower DTW = higher human rating)")
print("- Provides validation of DTW as a quality metric")
print("- Includes both length-normalized and z-score normalized correlations")
print("\nNormalization strategy:")
print("1. Length normalization: DTW_distance / ((human_length + llm_length) / 2)")
print("2. Z-score normalization: (method_mean - overall_mean) / overall_std_dev")
print("Z-scores interpretation: Negative = Better than average, Positive = Worse than average")
print("\nPublication improvements:")
print("- Increased font sizes for all text elements")
print("- Enhanced color schemes for better contrast")
print("- Improved plot margins and spacing")
print("- Higher resolution output (300 DPI)")
print("- Cleaner method name formatting")
print("- Consistent y-axis limits for time series comparability")
print("- Comprehensive human-DTW alignment validation")