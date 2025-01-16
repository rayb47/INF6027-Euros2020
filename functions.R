# FUNCTION to check and install missing libraries
install_if_missing <- function(libs) {
  for (lib in libs) {
    if (!require(lib, character.only = TRUE)) {
      install.packages(lib, dependencies = TRUE)
      library(lib, character.only = TRUE)
    }
  }
}

# FUNCTION to load the installed libraries
load_libraries <- function(libs) {
  for (lib in libs) {
    library(lib, character.only = TRUE)
  }
}

# FUNCTION to aggregate stats for teams
aggregate_team_stats <- function(data, team_type, is_knockout = FALSE) {
  if (team_type == "Home") {
    team_stats <- data %>%
      # group_by(TeamName = HomeTeamName) %>%
      group_by(HomeTeamName) %>%
      summarize(
        Wins = if (!is_knockout) sum(ScoreHome > ScoreAway, na.rm = TRUE) else
          sum(Winner == "Home", na.rm = TRUE),
        Losses = if (!is_knockout) sum(ScoreHome < ScoreAway, na.rm = TRUE) else
          sum(Winner == "Away", na.rm = TRUE),
        Draws = if (!is_knockout) sum(ScoreHome == ScoreAway, na.rm = TRUE) else NULL,
        .groups = "drop"
      )
  } else if (team_type == "Away") {
    team_stats <- data %>%
      group_by(AwayTeamName) %>%
      summarize(
        Wins = if (!is_knockout) sum(ScoreHome < ScoreAway, na.rm = TRUE) else
          sum(Winner == "Away", na.rm = TRUE),
        Losses = if (!is_knockout) sum(ScoreHome > ScoreAway, na.rm = TRUE) else
          sum(Winner == "Home", na.rm = TRUE),
        Draws = if (!is_knockout) sum(ScoreHome == ScoreAway, na.rm = TRUE) else NULL,
        .groups = "drop"
      )
  } else {
    stop("Invalid team_type. Use 'Home' or 'Away'.")
  }
  
  return(team_stats)
}

# FUNCTION to calculate different team performance metrics over group & knockout stages
calculate_stagewise_metrics <- function(data, stage_condition = "Group Stage", metric_type = "both", team_name = NULL) {
  
  # Filters data by tournament stage
  filtered_data <- data %>%
    filter(Stage == stage_condition)
  
  # Further filters by team name if provided
  if (!is.null(team_name)) {
    filtered_data <- filtered_data %>% filter(TeamName == team_name)
  }
  
  # Computes defensive metrics
  if (metric_type == "defense" || metric_type == "both") {
    defensive_metrics <- filtered_data %>%
      group_by(TeamName) %>%
      summarize(
        avg_goals_conceded = round(mean(`Goals conceded`, na.rm = TRUE), 2),
        avg_tackles = round(mean(`Tackles won`, na.rm = TRUE), 1),
        avg_recoveries = round(mean(`Recovered balls`, na.rm = TRUE), 1),
        avg_blocks = round(mean(`Blocks`, na.rm = TRUE), 1),
        avg_fouls = round(mean(`Fouls committed`, na.rm = TRUE), 1),
        avg_yellow_cards = round(mean(`Yellow cards`, na.rm = TRUE), 1),
        avg_fouls_per_yellow_card = round(ifelse(mean(`Yellow cards`, na.rm = TRUE) == 0, NA, avg_fouls / avg_yellow_cards),2),
        avg_clearances = round(mean(`Clearances`, na.rm = TRUE),2),
        .groups = "drop"
      )
  }
  
  # Computes attacking metrics
  if (metric_type == "attack" || metric_type == "both") {
    attacking_metrics <- filtered_data %>%
      group_by(TeamName) %>%
      summarize(
        avg_goals = round(mean(`Goals`, na.rm = TRUE), 2),
        avg_corners = round(mean(`Corners`, na.rm = TRUE), 2),
        avg_possession = round(mean(`Ball Possession`, na.rm = TRUE), 1),
        avg_total_attempts = round(mean(`Total Attempts`, na.rm = TRUE), 1),
        avg_total_attacks = round(mean(`Total Attacks`, na.rm = TRUE), 1),
        avg_passes = round(mean(`Passes completed`, na.rm = TRUE), 1),
        avg_passes_acc = round(mean(`Passes accuracy`, na.rm = TRUE),2),
        avg_attempts_acc = round(mean(`Attempts Accuracy`, na.rm = TRUE), 2),
        .groups = "drop"
      )
  }
  
  # Combines both metrics if both are requested
  if (metric_type == "both") {
    combined_metrics <- defensive_metrics %>%
      full_join(attacking_metrics, by = "TeamName")
    return(combined_metrics)
  }
  
  # Returns the respective metrics based on the parameter
  if (metric_type == "defense") return(defensive_metrics)
  if (metric_type == "attack") return(attacking_metrics)
}



# FUNCTION to check if variable is suitable for the t-test
suitability_and_test <- function(data, field, group_var) {
  library(dplyr)
  
  # Ensures the grouping variable has exactly two levels
  group_levels <- unique(data[[group_var]])
  if (length(group_levels) != 2) {
    stop("The grouping variable must have exactly two levels.")
  }
  
  # Ensures the dependent variable is numeric
  if (!is.numeric(data[[field]])) {
    stop("The field to test must be numeric.")
  }
  
  # Checks for normality within each group
  normality_results <- data %>%
    group_by(.data[[group_var]]) %>%
    summarize(
      p_value = shapiro.test(.data[[field]])$p.value,
      .groups = "drop"
    )
  
  # Checks for equal variances in variable distribution
  variance_test <- var.test(as.formula(paste0("`", field, "` ~ `", group_var, "`")), data = data)
  
  # Determines if assumptions are violated
  normality_violated <- any(normality_results$p_value < 0.05)
  variance_violated <- variance_test$p.value < 0.05
  
  # Prints suitability check results
  cat("### T-Test Suitability Check ###\n")
  cat("1. Normality (Shapiro-Wilk Test):\n")
  print(normality_results)
  
  cat("\n2. Variance Equality (F-Test):\n")
  print(variance_test)
  
  # Calculates group means of metric
  group_means <- data %>%
    group_by(.data[[group_var]]) %>%
    summarize(
      Mean = mean(.data[[field]], na.rm = TRUE),
      Median = median(.data[[field]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Chooses the appropriate test and prints results 
  if (normality_violated) {
    cat("\n### Distribution is NOT normal. Performing Mann-Whitney U Test ###\n")
    wilcox_test_result <- wilcox.test(
      as.formula(paste0("`", field, "` ~ `", group_var, "`")), 
      data = data, 
      exact = FALSE
    )
    print(wilcox_test_result)
    cat("\n### Group Means ###\n")
    print(group_means)
  } else if (variance_violated) {
    cat("\n### Equal Variance NOT Exhibited. Performing Welch's T-Test ###\n")
    t_test_result <- t.test(as.formula(paste0("`", field, "` ~ `", group_var, "`")), data = data, var.equal = FALSE)
    print(t_test_result)
  } else {
    cat("\n### Assumptions met. Performing Student's T-Test ###\n")
    t_test_result <- t.test(as.formula(paste0("`", field, "` ~ `", group_var, "`")), data = data, var.equal = TRUE)
    print(t_test_result)
  }
}

# FUNCTION to create a boxplot for comparison of metrics over the group and knockout stage
create_boxplot <- function(data, field, group_var, seed = 123) {
  library(ggplot2)
  library(dplyr)
  
  # Sets random seed for consistent jitter
  set.seed(seed)
  
  # Calculates summary statistics for boxplot
  box_summary <- data %>%
    group_by(.data[[group_var]]) %>%
    summarize(
      Max = max(.data[[field]], na.rm = TRUE),
      Min = min(.data[[field]], na.rm = TRUE),
      Median = median(.data[[field]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Generates the boxplot
  ggplot(data, aes(x = .data[[group_var]], y = .data[[field]], fill = .data[[group_var]])) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.5) +
    geom_text(
      data = box_summary,
      aes(
        x = .data[[group_var]], 
        y = Max, 
        label = paste0("Max: ", round(Max, 1))
      ),
      vjust = -0.5,
      fontface = "bold",
      inherit.aes = FALSE
    ) +
    geom_text(
      data = box_summary,
      aes(
        x = .data[[group_var]], 
        y = Min, 
        label = paste0("Min: ", round(Min, 1))
      ),
      vjust = 1.5,
      fontface = "bold",
      inherit.aes = FALSE
    ) +
    geom_text(
      data = box_summary,
      aes(
        x = .data[[group_var]], 
        y = Median, 
        label = paste0("Median: ", round(Median, 1))
      ),
      vjust = 1.5,
      fontface = "bold",
      inherit.aes = FALSE
    ) +
    labs(
      title = paste(field, "per Game : Group Stage vs Knockouts"),
      x = group_var,
      y = field
    ) +
    theme_minimal()
}

# FUNCTION to plot bar charts for metric comparisons across tournament stages
plot_stage_comparison <- function(data_long, metric_name, title) {
  
  # Filters data for the selected metric
  filtered_data <- data_long %>%
    filter(Metric == metric_name)
  
  # Creates the bar plot
  ggplot(filtered_data, aes(x = TeamName, y = Value, fill = Stage)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(
      aes(label = round(Value, 1)), 
      position = position_dodge(width = 0.9), 
      vjust = -0.3, 
      size = 5
    ) +
    labs(
      title = title,
      x = "Team",
      y = metric_name,
      fill = "Stage"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# FUNCTION to plot a stage-wise scatter plot of all teams in the tournament based on performance metrics
plot_stage_comparison_scatter <- function(data_long, metric_name, label_size = 5) {
  
  # Filters data for the selected metric
  filtered_data <- data_long %>%
    filter(Metric == metric_name)
  
  # Creates the scatter plot with connecting lines
  ggplot(filtered_data, aes(x = TeamName, y = Value, color = Stage)) +
    geom_segment(
      data = filtered_data %>%
        pivot_wider(names_from = Stage, values_from = Value) %>%
        # Ensure both stages exist for the team
        drop_na(),  
      aes(
        x = TeamName,
        xend = TeamName,
        y = `Group Stage`,
        yend = `Knockout Stage`
      ),
      color = "black",
      linetype = "dashed"
    ) +
    geom_point(size = 4, alpha = 0.8) +
    geom_text_repel(
      aes(label = round(Value, 1)), 
      size = label_size,
      show.legend = FALSE
    ) +
    labs(
      title = paste("Stage-Wise Scatter Plot for", metric_name),
      x = "Team",
      y = "Metric Value",
      color = "Stage"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
}

# Plots feature importance based on Correlation Analysis results
plot_feature_importance <- function(data, title = "Feature Importance Based on Random Forest Model") {
  ggplot(data, aes(x = reorder(Metric, Importance), y = Importance, fill = Category)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = round(Importance, 2)), 
      hjust = -0.2,
      size = 4
    ) + 
    coord_flip() +
    scale_fill_manual(
      values = c("Attacking" = "skyblue", "Defensive" = "salmon"),
      name = "Category"
    ) +
    labs(
      title = title,
      x = "Metrics",
      y = "Importance"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "top",
      axis.text.y = element_text(size = 12, face = "bold")
    )
}

# Plots feature importance using results of the Random Forest Model
plot_correlation_analysis <- function(data, title = "Metric Correlation with Qualification") {
  ggplot(data, aes(x = reorder(Metric, Correlation), y = Correlation, fill = Category)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(
      aes(label = round(Correlation, 2)), 
      position = position_dodge(width = 0.9), 
      vjust = 0.5, 
      hjust = -0.1, 
      size = 4
    ) +
    coord_flip() +
    labs(
      title = title,
      x = "Metrics",
      y = "Correlation with Qualification",
      fill = "Category"
    ) +
    scale_fill_manual(
      values = c("Attacking" = "skyblue", "Defensive" = "salmon")
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      axis.text.y = element_text(size = 12, face = "bold")
    )
}

# FUNCTION to plot a clusters of teams for a 2D comparison of metrics between all the teams
plot_team_clusters_split <- function(data, x_var, y_var, x_label, y_label, plot_title, black_labels = NULL) {
  # Add a column to specify label color for specific teams
  data <- data %>%
    mutate(
      LabelColor = ifelse(TeamName %in% black_labels, "black", as.factor(Cluster))  # Highlight specific labels in black, others by cluster
    )
  
  # Calculate midpoints for the x and y axes
  x_midpoint <- median(data[[x_var]], na.rm = TRUE)  # Calculate median for x-axis
  y_midpoint <- median(data[[y_var]], na.rm = TRUE)  # Calculate median for y-axis
  
  ggplot(data, aes_string(x = x_var, y = y_var, color = "as.factor(Cluster)")) +
    # Adds colored backgrounds for each quadrant
    annotate("rect", xmin = -Inf, xmax = x_midpoint, ymin = y_midpoint, ymax = Inf, fill = "lightblue", alpha = 0.2) +  # Top left
    annotate("rect", xmin = x_midpoint, xmax = Inf, ymin = y_midpoint, ymax = Inf, fill = "lightgreen", alpha = 0.2) +  # Top right
    annotate("rect", xmin = -Inf, xmax = x_midpoint, ymin = -Inf, ymax = y_midpoint, fill = "lightpink", alpha = 0.2) +  # Bottom left
    annotate("rect", xmin = x_midpoint, xmax = Inf, ymin = -Inf, ymax = y_midpoint, fill = "lightblue", alpha = 0.2) +  # Bottom right
    
    # Add points
    geom_point(size = 3) +
    
    # Add labels with conditional color
    geom_text_repel(
      aes(label = TeamName, color = LabelColor),
      size = 3
    ) +
    
    # Add dashed lines for quadrants
    geom_vline(xintercept = x_midpoint, linetype = "dashed", color = "gray") +
    geom_hline(yintercept = y_midpoint, linetype = "dashed", color = "gray") +
    
    # Add labels and theme
    labs(
      title = plot_title,
      x = x_label,
      y = y_label,
      color = "Cluster"
    ) +
    scale_color_manual(
      values = c("1" = "brown1", "2" = "green2", "3" = "dodgerblue2", "black" = "black"),  # Custom cluster and label colors
      breaks = c("1", "2", "3"),  # Legend only for clusters
      labels = c("Cluster 1", "Cluster 2", "Cluster 3")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(face = "bold"),
      legend.position = "top"  # Position legend at the top
    )
}

# FUNCTION to plot a heatmap for a given metric (across the group and knockout stage)
create_heatmap <- function(data, metric, title = "Heatmap of Metric by Stage and Team") {
  library(ggplot2)
  
  # Checks if the metric exists in the data
  if (!metric %in% colnames(data)) {
    stop(paste("The metric", metric, "does not exist in the dataset."))
  }
  
  # Creates the heatmap
  ggplot(data, aes(x = Stage, y = TeamName, fill = .data[[metric]])) +
    geom_tile(color = "white") +
    geom_text(aes(label = .data[[metric]]), color = "yellow2", fontface = "bold", size = 5) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = metric) +
    labs(
      title = title,
      x = "Tournament Stage",
      y = "Team Name"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      axis.text.y = element_text(size = 10, face = "bold"), 
      axis.title.x = element_text(face = "bold"),  
      axis.title.y = element_text(face = "bold"),  
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
}
      
