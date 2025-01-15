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
  summarize(across(all_of(numeric_columns), ~ sum(.x, na.rm = TRUE), .names = "{.col}"), .groups = "drop") %>%
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
      all_of(numeric_columns), 
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

# Top 8 Teams in the Tournament
top_8_teams <- c("Belgium", "Italy", "Switzerland", "Spain", "England", "Ukraine", "Czech Republic", "Denmark")

# Fetches stage-wise statistics for the top 8 teams in the tournament
group_stage_filtered <- group_stage_metrics %>%
  filter(TeamName %in% top_8_teams) %>%
  mutate(Stage = "Group Stage")
knockout_stage_filtered <- knockout_stage_metrics %>%
  filter(TeamName %in% top_8_teams) %>%
  mutate(Stage = "Knockout Stage")

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

# --------- RESEARCH QUESTIOn 3 -----------------
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
