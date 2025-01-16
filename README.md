# INF6027-Euros2020

Repository Link: https://github.com/rayb47/INF6027-Euros2020/tree/main

## Profile
- A result-oriented and driven IT individual with a passion for developing innovative software solutions.
- A proficient programmer with work experience in coding and testing, software design and development.

## Skills
- Technical: Python, JavaScript, HTML, R, SQL, PowerBI, Java
- Core: Problem Solving, Time Management, Team Collaboratiion, Critical Thinking

## Project Description

This project analyzes football tournament data (from the 2020 Euros) to address 3 main proposed research questions,
1. What are the differences in statistics between the group stage and knockout stage for different teams?
2. Do attacking or defensive metrics play a more critical role in determining whether a team qualifies for the knockout stage?
3. Can clustering teams based on performance metrics be used as an indicator of overall tournament success?

The code for this project is well-documented and neatly organized to ensure clarity and reproducibility. The scripts are well-indented with appropriate comments to explain the steps of the analysis. All the code is divided into 2 files,
1. main.R - This file handles the processing of data, computation of metrics and the entire analysis.
2. functions.R - This files contains all the user-created functions that are necessary to carry out the required analysis. This file does not need to be visited, as it is executed through `main.R`.


## Steps to Execute the Code
1. Clone the contents of the repository to your local machine.
   
   ```git clone https://github.com/rayb47/INF6027-Euros2020.git```
2. Open R or RStudio and set the working directory to the cloned repository

   ```setwd("path/to/your-repo-name")```
3. Run the code step-by-step

   - Highlight a section of code, or place your cursor on a specific line that you want to execute and press Ctrl+Enter to execute the code.
   - This will display results in the R Console (Tables or resutls of statistical tests) or the Plots panel (Visualizations).

## Code

`main.R`

File main.R is split into 4 main parts,
- The first handles the loading and processing of data to create a custom dataframe to store all the computed metrics (average goals scored, fouls committed, possession percentage, etc.). It uses the data from the multiple dataset files ('Match events.csv', 'Match team statistics.csv' and 'Match information.csv') to tabularize the wins, losses and draws for teams at the tournament, in the knockout stage and group stage respectively.
- The second, third and fourth part handle the analysis for the first, second and third research questions respectively.

#### Part One
```R
source("functions.R")

# Lists out the required libraries
required_libraries <- c(
  "dplyr", "ggplot2", "ggrepel", "tibble", "tidyr", 
  "reshape2", "randomForest", "corrplot", "factoextra", 
  "stringr", "ggtext"
)

# Check and install missing libraries
install_if_missing(required_libraries)

# Load all libraries
load_libraries(required_libraries)

# Loads data from CSV files
match_events <- read.csv("Match events.csv")
match_information <- read.csv("Match information.csv")
match_team_statistics <- read.csv("Match team statistics.csv")

# Checks for missing values in all dataset files
sapply(match_events, function(x) sum(is.na(x)))
sapply(match_information, function(x) sum(is.na(x)))
sapply(match_team_statistics, function(x) sum(is.na(x)))

# Gets Team Names and IDs of participating teams
unique_teams <- match_team_statistics %>%
  select(TeamName, TeamID) %>%
  distinct() %>%
  arrange(TeamName)

# Filters out group stage matches
group_stage_matches <- match_information %>%
  filter(RoundName == "final tournament")

# Filters out knockout stage matches
knockout_stage_matches <- match_information %>%
  filter(RoundName != "final tournament")

# Computes data to decide the winners of each match
match_data <- group_stage_matches %>%
  mutate(
    HomeResult = case_when(
      ScoreHome > ScoreAway ~ "Win",
      ScoreHome < ScoreAway ~ "Loss",
      ScoreHome == ScoreAway ~ "Draw"
    ),
    AwayResult = case_when(
      ScoreHome > ScoreAway ~ "Loss",
      ScoreHome < ScoreAway ~ "Win",
      ScoreHome == ScoreAway ~ "Draw"
    )
  )

# Creates Table with Match Result Stats for each team (for their home & away fixtures)
group_home_stats <- aggregate_team_stats(match_data, "Home")
group_away_stats <- aggregate_team_stats(match_data, "Away")

# Combines home and away group stage stats
group_team_stats <- group_home_stats %>%
  rename(Team = HomeTeamName) %>%  # Rename for consistency
  full_join(
    group_away_stats %>% rename(Team = AwayTeamName),
    by = "Team"
  ) %>%
  mutate(
    Wins = coalesce(Wins.x, 0) + coalesce(Wins.y, 0),
    Losses = coalesce(Losses.x, 0) + coalesce(Losses.y, 0),
    Draws = coalesce(Draws.x, 0) + coalesce(Draws.y, 0),
  ) %>%
  select(Team, Wins, Losses, Draws) %>%
  arrange(-Wins)

# Appends ID to stats table
final_group_stats <- group_team_stats %>%
  left_join(unique_teams, by = c("Team" = "TeamName")) %>%
  relocate(TeamID, .before = Team)  # Reorder columns to place TeamID first

# Work on knockout stage match data
knockout_stage_matches <- knockout_stage_matches %>%
  left_join(unique_teams, by = c("HomeTeamName" = "TeamName")) %>%
  rename(HomeTeamID = TeamID) %>%
  left_join(unique_teams, by = c("AwayTeamName" = "TeamName")) %>%
  rename(AwayTeamID = TeamID)

# Computes penalty goals for matches that went to penalties
penalty_shootout_results <- match_events %>%
  filter(Event == "PenaltyScored") %>%  # Focus on penalties scored
  left_join(
    knockout_stage_matches %>% select(MatchID, HomeTeamID, AwayTeamID),
    by = "MatchID"
  ) %>%
  group_by(MatchID) %>%
  summarize(
    HomePenaltyGoals = sum(TeamFromID == HomeTeamID, na.rm = TRUE),
    AwayPenaltyGoals = sum(TeamFromID == AwayTeamID, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PenaltyWinner = case_when(
      HomePenaltyGoals > AwayPenaltyGoals ~ "Home",
      AwayPenaltyGoals > HomePenaltyGoals ~ "Away",
      TRUE ~ NA_character_  # Handle unexpected cases
    )
  )

# Establishes which teams won the penalty shootouts
match_results_penalties <- knockout_stage_matches %>%
  left_join(penalty_shootout_results, by = "MatchID") %>%
  mutate(
    Winner = case_when(
      ScoreHome > ScoreAway ~ "Home",  # Home team won in regular time
      ScoreHome < ScoreAway ~ "Away",  # Away team won in regular time
      ScoreHome == ScoreAway & !is.na(PenaltyWinner) ~ PenaltyWinner,  # Penalty shootout winner
      TRUE ~ NA_character_  # Handle unexpected cases
    )
  )

# Computes knockout stats for the home & away teams
knockout_home_stats <- aggregate_team_stats(match_results_penalties, "Home", is_knockout = TRUE)
knockout_away_stats <- aggregate_team_stats(match_results_penalties, "Away", is_knockout = TRUE)

# Combines home and away knockout stats to have overall knockout stats
knockout_team_stats <- knockout_home_stats %>%
  rename(Team = HomeTeamName) %>%
  full_join(
    knockout_away_stats %>% rename(Team = AwayTeamName),
    by = "Team"
  ) %>%
  mutate(
    Wins = coalesce(Wins.x, 0) + coalesce(Wins.y, 0),
    Losses = coalesce(Losses.x, 0) + coalesce(Losses.y, 0),
  ) %>%
  select(Team, Wins, Losses) %>%
  arrange(Team)  # Sort alphabetically by team name

# Appends ID to stats table
final_knockout_stats <- knockout_team_stats %>%
  left_join(unique_teams, by = c("Team" = "TeamName")) %>%
  relocate(TeamID, .before = Team)

# Define stats of interest (Note: Typos in the strings below are intended, to match the metric names in the dataset)
stats_of_interest <- c(
  "Goals", "Goals conceded", "Ball Possession", "Corners", "Total Attempts", "Attempts on target", 
  "Tackles won", "Clearances", "Recovered balls", "Fouls committed",
  "Passes completed", "Passes accuracy", "Attempts Accuracy", "Blocks",
  "Lost balls", "Big Chances", "Instance of possession ", "Change of possession",
  "Goals scored in open play", "Goals scored on direct free-kcik", "Goals scored on indirect free-kcik", 
  "Goals scored on penalty ", "Goals on corner ", "Own-goals",
  "Goals conceded from open play ", "Goals conceded from set pieces", 
  "Goals conceded in penalty area", "Goals conceded outside penalty area",
  "Yellow cards", "Total Attacks"
)

# Create a single vector for numeric and N/A replacement
numeric_columns <- stats_of_interest

# Process data
detailed_match_team_stats <- match_team_statistics %>%
  filter(StatsName %in% stats_of_interest) %>%
  pivot_wider(names_from = StatsName, values_from = Value) %>%
  left_join(match_information, by = "MatchID") %>%
  mutate(
    Stage = if_else(RoundName == "final tournament", "Group Stage", "Knockouts"),
    across(all_of(numeric_columns), ~ as.numeric(.x)),
    across(all_of(numeric_columns), ~ replace_na(.x, 0))
  )

# Summarize match statistics
detailed_match_team_stats <- detailed_match_team_stats %>%
  group_by(MatchID, TeamName) %>%
  summarize(across(all_of(stats_of_interest), ~ sum(.x, na.rm = TRUE), .names = "{.col}"), .groups = "drop") %>%
  left_join(
    detailed_match_team_stats %>%
      select(MatchID, HomeTeamName.x, AwayTeamName.x, RoundName, Stage) %>%
      distinct(),
    by = "MatchID"
  )

# Summarize team tournament stats
team_tournament_stats <- detailed_match_team_stats %>%
  group_by(TeamName) %>%
  summarize(
    # Compute averages for listed columns
    across(
      all_of(stats_of_interest), 
      ~ mean(.x, na.rm = TRUE), 
      .names = "{str_replace_all(.col, ' ', '')}"
    ),
    # Compute specific custom calculations
    avg_fouls_per_yellow_card = ifelse(mean(`Yellow cards`, na.rm = TRUE) == 0, NA, mean(`Fouls committed`, na.rm = TRUE) / mean(`Yellow cards`, na.rm = TRUE)),
    avg_inst_poss = mean(`Instance of possession `, na.rm = TRUE),
    avg_change_poss = mean(`Change of possession`, na.rm = TRUE),
    .groups = "drop"
  )

# Computes group stage metrics
group_stage_metrics <- calculate_stagewise_metrics(detailed_match_team_stats, "Group Stage")
View(group_stage_metrics)

# Computes knockout stage metrics
knockout_stage_metrics <- calculate_stagewise_metrics(detailed_match_team_stats, "Knockouts")
View(knockout_stage_metrics)
```
**Note**: The reason the top 8 teams are hardcoded here is because in the Euros teams that do not finish second in their groups can still qualify based on goal difference. The 4 best 3rd placed teams get through to the knockout stage. Since goal difference was not a metric that was computed, the teams that qualified had to be hardcoded.
```R
# Top 8 Teams in the Tournament
top_8_teams <- c("Belgium", "Italy", "Switzerland", "Spain", "England", "Ukraine", "Czech Republic", "Denmark")

# Fetches stage-wise statistics for the top 8 teams in the tournament
group_stage_filtered <- group_stage_metrics %>%
  filter(TeamName %in% top_8_teams) %>%
  mutate(Stage = "Group Stage")
knockout_stage_filtered <- knockout_stage_metrics %>%
  filter(TeamName %in% top_8_teams) %>%
  mutate(Stage = "Knockout Stage")
```
#### Part Two
```R
# -------- RESEARCH QUESTION 1 ------------------

# Combines the stage-wise statistics
comparison_data <- bind_rows(group_stage_filtered, knockout_stage_filtered)

# Pivot the data to long format for visualization
comparison_data_long <- comparison_data %>%
  select(TeamName, Stage, avg_goals, avg_possession, avg_goals_conceded, avg_total_attempts, avg_tackles, avg_fouls, avg_fouls_per_yellow_card) %>%
  pivot_longer(cols = starts_with("avg_"), names_to = "Metric", values_to = "Value")

# Plot for "avg_goals" and "avg_goals_conceded"  with connecting lines
plot_stage_comparison_scatter(comparison_data_long, "avg_goals")
plot_stage_comparison_scatter(comparison_data_long, "avg_goals_conceded")

# Plots bar chart for comparison
plot_stage_comparison(comparison_data_long, "avg_possession", "Stage-wise Comparison for Average Possession per Game")

# Plots a heat map for fouls committed comparison
create_heatmap(comparison_data, metric = "avg_fouls", title = "Fouls Committed per Game (Top 8 Teams)")

# Filters data only for the knockout teams
filtered_knockout_team_data <- detailed_match_team_stats %>%
  filter(TeamName %in% final_knockout_stats$Team)

# Checks suitability (normality and variance) of metric and runs appropriate test
suitability_and_test(filtered_knockout_team_data, "Yellow cards", "Stage")

# Creates box-plot for metric
create_boxplot(detailed_match_team_stats, "Fouls committed", "Stage")
# ---------- END OF RESEARCH QUESTION 1 -----------------
```
#### Part Three
```R

# ---------- RESEARCH QUESTION 2 ---------------
# Adds and populates 'Qualified' column to the group stage data  
group_stage_metrics_with_qual <- group_stage_metrics %>%
  mutate(Qualified = ifelse(TeamName %in% final_knockout_stats$Team, 1, 0))

# Selects group stage attacking metrics
group_stage_attacking_metrics <- group_stage_metrics_with_qual %>%
  select(avg_corners, avg_total_attempts, avg_total_attacks, avg_goals, avg_attempts_acc)

# Selects group stage defensive metrics
group_stage_defense_metrics <- group_stage_metrics_with_qual %>%
  select(avg_tackles, avg_recoveries, avg_clearances, avg_blocks, avg_goals_conceded)

# Calculates correlation for attacking metrics
attacking_correlation <- cor(group_stage_attacking_metrics, group_stage_metrics_with_qual$Qualified)

# Calculates correlation for defensive metrics
defensive_correlation <- cor(group_stage_defense_metrics, group_stage_metrics_with_qual$Qualified)

# Creates and populates a correlation dataframe (separates category based on type of attacking/defensive)
correlation_df <- data.frame(
  Metric = c(names(group_stage_attacking_metrics), names(group_stage_defense_metrics)),
  Correlation = c(attacking_correlation, defensive_correlation),
  Category = c(rep("Attacking", length(attacking_correlation)), rep("Defensive", length(defensive_correlation)))
)

# Plots results of correlation analysis
plot_correlation_analysis(correlation_df)

# Runs Random Forest Model for attacking metrics
rf_model_attacking <- randomForest(
  x = group_stage_attacking_metrics, 
  y = as.factor(group_stage_metrics_with_qual$Qualified), 
  importance = TRUE
)

# Runs Random Forest Model for defensive metrics
rf_model_defensive <- randomForest(
  x = group_stage_defense_metrics, 
  y = as.factor(group_stage_metrics_with_qual$Qualified), 
  importance = TRUE
)

# Stores attacking and defensive feature importances
attacking_feature_importance <- data.frame(
  Metric = rownames(importance(rf_model_attacking)),
  Importance = importance(rf_model_attacking)[, 1],
  Category = "Attacking"
)
defensive_feature_importance <- data.frame(
  Metric = rownames(importance(rf_model_defensive)),
  Importance = importance(rf_model_defensive)[, 1],
  Category = "Defensive"
)

# Combines both dataframes
feature_importance_combined <- rbind(attacking_feature_importance, defensive_feature_importance)

# Plots feature importance bar chart
plot_feature_importance(feature_importance_combined)
# ------------- END OF RESEARCH QUESTION 2 ----------------
```
#### Part Four
```R
# --------- RESEARCH QUESTION 3 -----------------
# Scaling tournament stats to create clusters
team_tournament_stats_scaled <- scale(team_tournament_stats[, -1])
fviz_nbclust(team_tournament_stats_scaled, kmeans, method = "wss")
set.seed(123)
kmeans_result <- kmeans(team_tournament_stats_scaled, centers = 3)  # 3 clusters
team_tournament_stats$Cluster <- kmeans_result$cluster

# Plots of  the required clusters
plot_team_clusters_split(
  data = team_tournament_stats,
  x_var = "BallPossession", 
  y_var = "Goals",
  x_label = "Possession Maintained (per game)", 
  y_label = "Goals (per game)",
  plot_title = "Possession Maintained vs Goals (Teams with black labels qualified for the knockout stage)",
  black_labels = final_knockout_stats$Team
)

plot_team_clusters_split(
  data = team_tournament_stats,
  x_var = "TotalAttacks", 
  y_var = "TotalAttempts",
  x_label = "Attacks (per game)", 
  y_label = "Attempts on Goal (per game)",
  plot_title = "Attacks vs Attempts on Goal (Teams with black labels qualified for the knockout stage)",
  black_labels = final_knockout_stats$Team
)

plot_team_clusters_split(
  data = team_tournament_stats,
  x_var = "Passesaccuracy", 
  y_var = "Passescompleted",
  x_label = "Passing Accuracy (per game)", 
  y_label = "Passes Attempted (per game)",
  plot_title = "Passing Accuracy vs Passes Attempted (Teams with black labels qualified for the knockout stage)",
  black_labels = final_knockout_stats$Team
)

```

`functions.R`

```R
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
 ```


  
## Generated Visualizations
Once you have finished running the `main.R` file, you would have seen the following visualizations (in order of generation in the code).

### Research Question 1
All the metric values here are averages (per game) and only for the top 8 teams in the tournament. That is, the 8 teams that made it to the quarter finals.

### Dotplot for Average Goals Scored (Across Group and Knockout stages)
![DotPlot1](https://github.com/user-attachments/assets/46900303-19f8-4e7b-8125-a69c2f63b7a8)

### Dotplot for Average Goals Conceded (Across Group and Knockout stages)
![DotPlot2](https://github.com/user-attachments/assets/65f94b5a-4171-4434-b47e-1e49139cfcc4)

### Clustered Bar Chart showing Average Possession Percentage Maintained (Across Group and Knockout stages)
![BarChart1](https://github.com/user-attachments/assets/c9f9e6cd-4f94-48f6-921e-951b9ee6c96c)

### Heatmap for Average Number of Fouls Committed 
![HeatMap](https://github.com/user-attachments/assets/c7dd011e-29e4-46fb-9fbe-4a8a8210e48e)

### Boxplots for Combined Average Fouls Committed by All Teams Grouped by Stage
![Boxplot](https://github.com/user-attachments/assets/e9229a26-8c8e-4d63-8e84-ffd8bee33b51)

The output of the statistical tests (Mann Whitney Test, Welch's t-tests) conducted for this research question will look like this in the R console,

![image](https://github.com/user-attachments/assets/712ea251-49fe-4af7-88e4-8dd5ecfa8fa4)



### Research Question 2
All the metric values here are averages for all teams in the Group Stage of the tournament, since we are measuring the impact they have on qualification to the knockout stage.

### Bar Chart for Correlation Analysis Results
![CorrelationAnalysis](https://github.com/user-attachments/assets/0ae1f5e6-8ce8-4adb-bec2-7a3b68a5a551)

### Bar Chart for Feature Importance Results using Random Forest Model 
![FeatureImportance2](https://github.com/user-attachments/assets/bc104b44-37ea-4c08-83cf-9945c1f421bf)


### Research Question 3
All metric values here are averages (per-game) for all the teams in tournaments, combined across both stages.

### Possession Maintained vs Goals Scored
![Cluster1](https://github.com/user-attachments/assets/2bcc679b-5107-48df-8ef4-5500cbf84b94)

### Attacks vs Attempts on Goal
![Cluster2](https://github.com/user-attachments/assets/429296db-6328-4b17-90e4-f2d007256b7a)

### Passing Accuracy vs Attempted Passes
![Cluster3](https://github.com/user-attachments/assets/bf3597f5-00bc-4e45-a4a8-db907be0db68)

## INF4000 Project
The link to the project page for INF4000 is https://rayb47.github.io/INF4000_Euros2020/








