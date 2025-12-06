# ============================================================================
# 03_func_visualization.R
# Purpose: Visualization functions for exploratory analysis
# ============================================================================

library(ggplot2)
library(scales)

# Function: Success rate bar chart
plot_success_bar <- function(data, x_var, fill_var = NULL, title = "Success Rates") {
  
  # Calculate success rates if not already done
  if(!"success_rate" %in% names(data)) {
    data <- data %>%
      group_by(across(all_of(c(x_var, fill_var)))) %>%
      summarise(
        n_total = n(),
        n_won = sum(primary_outcome == 1),
        success_rate = mean(primary_outcome == 1),
        .groups = "drop"
      )
  }
  
  p <- ggplot(data, aes(x = .data[[x_var]], y = success_rate))
  
  if(!is.null(fill_var)) {
    p <- p + 
      geom_col(aes(fill = .data[[fill_var]]), position = "dodge", alpha = 0.8) +
      geom_text(aes(label = sprintf("%.1f%%", success_rate * 100), 
                    group = .data[[fill_var]]),
                position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
      scale_fill_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c"))
  } else {
    p <- p + 
      geom_col(fill = "steelblue", alpha = 0.8) +
      geom_text(aes(label = sprintf("%.1f%%", success_rate * 100)),
                vjust = -0.5, size = 3.5)
  }
  
  p + 
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    labs(title = title, x = NULL, y = "Success Rate") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))
}

# Function: Heatmap of success rates
plot_success_heatmap <- function(data, x_var, y_var, title = "Success Rate Heatmap") {
  
  # Calculate success rates if needed
  if(!"success_rate" %in% names(data)) {
    data <- data %>%
      group_by(across(all_of(c(x_var, y_var)))) %>%
      summarise(
        n_total = n(),
        success_rate = mean(primary_outcome == 1),
        .groups = "drop"
      )
  }
  
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], fill = success_rate)) +
    geom_tile(color = "white", size = 1.5) +
    geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", success_rate * 100, n_total)),
              color = "white", fontface = "bold", size = 3.5) +
    scale_fill_gradient2(
      low = "#d73027", 
      mid = "#fee08b", 
      high = "#1a9850",
      midpoint = 0.3,
      labels = percent,
      name = "Success\nRate"
    ) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      panel.grid = element_blank()
    )
}

cat("Visualization functions loaded successfully\n")