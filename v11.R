# 1. Setup

library(tidyverse)      
library(stringi)       
library(ggrepel)       
library(patchwork)    
library(gridExtra)      
library(plotly)         
library(cluster)     
library(NbClust)
# install once from GitHub:
# install.packages("remotes")
# remotes::install_github("abresler/nbastatR")
library(nbastatR)

# eliminate scientific notation in salaries
options(scipen = 999)
library(tidyverse)
library(readr)

# ============================================================
# STEP 1 — Load and clean salary CSV
# ============================================================

salaries_raw <- read_csv("sportsref_download.csv", skip = 1)

# Preview
glimpse(salaries_raw)

# Function to clean a salary string and return a numeric value
# Handles inputs like "$59,606,817" or "59606817" or "$59606817"

clean_salary <- function(salary) {
  
  cleaned <- c()  # empty vector to store results
  
  for (i in salary) {
    if (is.character(i)) {
      # if salary is a string, strip $ signs, commas, and whitespace
      i <- i |>
        str_remove_all("\\$") |>
        str_remove_all(",") |>
        str_trim() |>
        as.numeric()
    }
    cleaned <- c(cleaned, i)
  }
  return(cleaned)
}

# Apply the function inside mutate()

salary_df <- salaries_raw |>
  select(Player, Tm, `2025-26`) |>
  rename(salary = `2025-26`) |>
  filter(!is.na(Player), !is.na(salary)) |>
  mutate(salary = clean_salary(salary))

#will only have one player per row. keeps highest salary row
salary_df_clean <- salary_df |>
  group_by(Player) |>
  summarize(
    team   = Tm[which.max(salary)],   # keep team associated with highest salary
    salary = max(salary),             # keep highest salary value
    .groups = "drop"
  )

# Confirm duplicates are gone
salary_df_clean |>
  count(Player) |>
  filter(n > 1)


# ============================================================
# STEP 2 Performance data (nbastatR, 2024-25 regular season)
# ============================================================

# nbastatR labels seasons by the END year, so 2024-25 = 2025.
# bref_players_stats() pulls per season averages directly from Basketball Reference
Sys.setenv(VROOM_CONNECTION_SIZE = 500072)

bref_pg <- nbastatR::bref_players_stats(
  seasons = 2025,
  tables  = "per_game"
)

colnames(bref_pg)
glimpse(bref_pg)

# rename to match what our downstream plots / model expect.
# note: bref_players_stats returns groupPosition already bucketed
# as G / F / C, so we use that directly.
stats_df <- bref_pg |>
  select(
    player_name = namePlayer,
    position    = groupPosition,
    GP          = countGames,
    MPG         = minutesPerGame,
    PPG         = ptsPerGame,
    RPG         = trbPerGame,
    APG         = astPerGame,
    SPG         = stlPerGame,
    BPG         = blkPerGame,
    TOPG        = tovPerGame,
    PFPG        = pfPerGame,
    FG_PCT      = pctFG,
    FG3_PCT     = pctFG3,
    FT_PCT      = pctFT
  ) |>
  # bref lists traded players once per stint plus a "TOT" row.
  # keep the row with the most games played per player.
  group_by(player_name) |>
  slice_max(order_by = GP, n = 1, with_ties = FALSE) |>
  ungroup() |>
  filter(GP >= 20, !is.na(position))   # drop low GP and missing positions

# Verify
stats_df |>
  count(position, sort = TRUE)

# ============================================================
# STEP 3 — Use a join command to combine Salary and Player Stats
# ============================================================

# Join stats_df + salary_df
# inner_join — only keep players with both stats AND a salary
# names differ between tables so we specify both sides

nba_df <- stats_df |>
  inner_join(salary_df_clean, by = c("player_name" = "Player"))


# Check for any duplicate player rows
nba_df |>
  count(player_name) |>
  filter(n > 1)

# Spot check to confirm if the join method worked
nba_df |>
  select(player_name, position, team, salary, GP, PPG, RPG, APG) |>
  arrange(desc(salary)) |>
  head(10)

# ============================================================
# STEP 4 — Visualize Data
# ============================================================

# Histogram
p1_hist_salarydistribution <- nba_df |>
  ggplot(aes(x = salary/1e6)) + #dividing by 1e6 to make the Xaxis more readable
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  labs(
    title    = "NBA Player Salary Distribution",
    caption  = "Data: Basketball Reference + nbastatR",
    x        = "Salary (USD Millions)",
    y        = "Number of Players"
  ) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
#Graph1
p1_hist_salarydistribution #show data

p_top10_salary <- nba_df |>
  arrange(desc(salary)) |>
  slice_head(n = 10) |>
  ggplot(aes(x = salary / 1e6, y = reorder(player_name, salary), fill = position)) +
  geom_bar(stat = "identity") +
  labs(
    title   = "Top 10 Highest Paid NBA Players",
    x       = "Salary (Millions USD)",
    y       = "Player",
    fill    = "Position",
    caption = "Data: Basketball Reference + nbastatR"
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
#Graph2
p_top10_salary #show graph

p_top10_ppg <- nba_df |>
  arrange(desc(PPG)) |>
  slice_head(n = 10) |>
  ggplot(aes(x = PPG, y = reorder(player_name, PPG), fill = position)) +
  geom_bar(stat = "identity") +
  labs(
    title   = "Top 10 Scorers in the NBA",
    x       = "Points Per Game (PPG)",
    y       = "Player",
    fill    = "Position",
    caption = "Data: Basketball Reference + nbastatR"
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
#Graph3
p_top10_ppg #show graph

#Boxplot
p_salary_position <- nba_df |>
  ggplot(aes(x = position, y = salary / 1e6, color = position)) +
  geom_boxplot() +
  labs(
    title    = "NBA Salary Distribution by Position",
    subtitle = "How does pay vary across Guards, Forwards and Centers?",
    x        = "Position",
    y        = "Salary (Millions USD)",
    caption  = "Data: Basketball Reference + nbastatR"
  ) +
  theme_light() +
  theme(
    plot.title      = element_text(hjust = 0.5),
    plot.subtitle   = element_text(hjust = 0.5),
    legend.position = "none"
  )
#Graph4
p_salary_position

#scatterplot
p_salary_ppg <- nba_df |>
  ggplot(aes(x = PPG, y = salary / 1e6)) +
  geom_point(aes(color = position), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title   = "Salary vs. Points Per Game",
    x       = "Points Per Game (PPG)",
    y       = "Salary (Millions USD)",
    color   = "Position",
    caption = "Data: Basketball Reference + nbastatR"
  ) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))
#Graph5
p_salary_ppg #scatter salary vs PPG

p_salary_rpg <- nba_df |>
  ggplot(aes(x = RPG, y = salary / 1e6)) +
  geom_point(aes(color = position), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title   = "Salary vs. Rebounds Per Game",
    x       = "Rebounds Per Game (RPG)",
    y       = "Salary (Millions USD)",
    color   = "Position",
    caption = "Data: Basketball Reference + nbastatR"
  ) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

#Graph6
p_salary_rpg #scatter salary vs RPG

p_salary_apg <- nba_df |>
  ggplot(aes(x = APG, y = salary / 1e6)) +
  geom_point(aes(color = position), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title   = "Salary vs. Assists Per Game",
    x       = "Assists Per Game (APG)",
    y       = "Salary (Millions USD)",
    color   = "Position",
    caption = "Data: Basketball Reference + nbastatR"
  ) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

#Graph7
p_salary_apg #scatter salary vs APG


#adding a new variable (value score = PPG + RPG + APG)
nba_df <- nba_df |>
  mutate(
    salary_M    = salary / 1e6,
    value_score = (PPG + RPG + APG) / salary_M
  )

#Graph 8: top 10 most underpaid nba players
p_underpaid <- nba_df |>
  arrange(desc(value_score)) |>
  slice_head(n = 10) |>
  ggplot(aes(x = value_score, y = reorder(player_name, value_score), fill = position)) +
  geom_bar(stat = "identity") +
  labs(
    title    = "Top 10 Most Underpaid NBA Players",
    subtitle = "Highest production per million dollars earned",
    x        = "Value Score (PPG + RPG + APG) / Salary (Millions)",
    y        = "Player",
    fill     = "Position",
    caption  = "Data: Basketball Reference + nbastatR"
  ) +
  theme_light() +
  theme(
    plot.title    = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

p_underpaid

#Graph 9: top 10 most overderpaid nba players
p_overpaid <- nba_df |>
  arrange(value_score) |>
  slice_head(n = 10) |>
  ggplot(aes(x = value_score, y = reorder(player_name, value_score), fill = position)) +
  geom_bar(stat = "identity") +
  labs(
    title    = "Top 10 Most Overpaid NBA Players",
    subtitle = "Lowest production per million dollars earned",
    x        = "Value Score (PPG + RPG + APG) / Salary (Millions)",
    y        = "Player",
    fill     = "Position",
    caption  = "Data: Basketball Reference + nbastatR"
  ) +
  theme_light() +
  theme(
    plot.title    = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

p_overpaid

#Graph 10: Scatter plot of Salary vs Value score
p_salary_value <- nba_df |>
  ggplot(aes(x = salary_M, y = value_score)) +
  geom_point(aes(color = position), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title    = "NBA Salary vs. Value Score",
    x        = "Salary (Millions USD)",
    y        = "Value Score (PPG + RPG + APG) / Salary (Millions)",
    color    = "Position",
    caption  = "Data: Basketball Reference + nbastatR"
  ) +
  theme_light() +
  theme(
    plot.title    = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

p_salary_value

# ============================================================
# STEP 5 — create a linear regression model
# ============================================================

#multiple linear regression using PPG,Apg,rpg, position as predictors
#salary as the dependent variable
lm.salary <- lm(salary~PPG+RPG+APG+position,data=nba_df)


summary(lm.salary)
#Adjusted R Squared Value is 0.6711
lm.salary
#Salary = -4603246 + 102851*RPG + 1382454*PPG + 1550150*APG + positionF*223638 -3057858*positionG
#Intercept (-4603246): Baseline Salary when PPG, APG, RPG and Position are 0 (reference point)
#PPG (1382454): Holding the other predictors constant, a 1 unit increase in PPG increases Salary by 1382454
#RPG(102851):  Holding the other predictors constant, a 1 unit increase in RPG increases Salary by 102851
#APG (1550150):  Holding the other predictors constant, a 1 unit increase in APG increases Salary by 1550150
#PositionF (223638):  Holding the other predictors constant, a 1 unit increase in APG increases Salary by 223638
#PositionG (-3057858):  Holding the other predictors constant, a 1 unit increase in RPG decreases Salary by 3057858

# ============================================================
# STEP 6 — create a clustering model
# ============================================================
install.packages("cluster")
library(cluster)
install.packages("NbClust")
library(NbClust)

#############Guards Cluster ###############################################
#will leave salary out since that is the dependent variable
Guards_data <- nba_df |>
  filter(position=='G')|>
  select(PPG,RPG,APG)
guard_scale <- scale(Guards_data)

# Estimate the best number of clusters from 2 to 10 using k-means criteria
guard_number_cluster_estimate <- NbClust(
  guard_scale,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 10,
  method = "kmeans"
)

# Show the voting results for the best number of clusters
guard_number_cluster_estimate$Best.nc

# Set seed for reproducibility
set.seed(123)

# Run PAM clustering with 5 clusters
# Note: the slides call this k-means, but this function is PAM
kmeans_guard_scalecluster <- pam(guard_scale, k = 5)

# Show medoids for the clusters
kmeans_guard_scalecluster$medoids

# Show the cluster assignment for each row
kmeans_guard_scalecluster$clustering

# Plot the clustering result in two reduced dimensions
plot(kmeans_guard_scalecluster)

# Add the assigned cluster to the Guard data
guard_cluster <- Guards_data %>%
  mutate(cluster = kmeans_guard_scalecluster$clustering)

# Show the dataset with assigned clusters
guard_cluster

# Compute the average of each variable by cluster
guard_cluster_summary <- guard_cluster %>%
  group_by(cluster) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

# Show cluster summaries
guard_cluster_summary

#############Forward Cluster ###############################################
#will leave salary out since that is the dependent variable
Forward_data <- nba_df |>
  filter(position=='F')|>
  select(PPG,RPG,APG)
Forward_scale <- scale(Forward_data)

# Estimate the best number of clusters from 2 to 10 using k-means criteria
forward_number_cluster_estimate <- NbClust(
  Forward_scale,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 10,
  method = "kmeans"
)

# Show the voting results for the best number of clusters
forward_number_cluster_estimate$Best.nc

# Set seed for reproducibility
set.seed(123)

# Run PAM clustering with 5 clusters
# Note: the slides call this k-means, but this function is PAM
kmeans_forward_scalecluster <- pam(Forward_scale, k = 5)

# Show medoids for the clusters
kmeans_forward_scalecluster$medoids

# Show the cluster assignment for each row
kmeans_forward_scalecluster$clustering

# Plot the clustering result in two reduced dimensions
plot(kmeans_forward_scalecluster)

# Add the assigned cluster to the Guard data
forward_cluster <- Forward_data %>%
  mutate(cluster = kmeans_forward_scalecluster$clustering)

# Show the dataset with assigned clusters
forward_cluster

# Compute the average of each variable by cluster
forward_cluster_summary <- forward_cluster %>%
  group_by(cluster) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

# Show cluster summaries
forward_cluster_summary

#############Center Cluster ###############################################
#will leave salary out since that is the dependent variable
Center_data <- nba_df |>
  filter(position=='C')|>
  select(PPG,RPG,APG)
Center_scale <- scale(Center_data)

# Estimate the best number of clusters from 2 to 10 using k-means criteria
Center_number_cluster_estimate <- NbClust(
  Center_scale,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 10,
  method = "kmeans"
)

# Show the voting results for the best number of clusters
Center_number_cluster_estimate$Best.nc

# Set seed for reproducibility
set.seed(123)

# Run PAM clustering with 5 clusters
# Note: the slides call this k-means, but this function is PAM
kmeans_center_scalecluster <- pam(Center_scale, k = 5)

# Show medoids for the clusters
kmeans_center_scalecluster$medoids

# Show the cluster assignment for each row
kmeans_center_scalecluster$clustering

# Plot the clustering result in two reduced dimensions
plot(kmeans_center_scalecluster)

# Add the assigned cluster to the Guard data
center_cluster <- Center_data %>%
  mutate(cluster = kmeans_center_scalecluster$clustering)

# Show the dataset with assigned clusters
center_cluster

# Compute the average of each variable by cluster
center_cluster_summary <- center_cluster %>%
  group_by(cluster) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

# Show cluster summaries
center_cluster_summary

# ============================================================
# STEP 7 — create interactive Plots
# ============================================================

# Salary vs PPG animated by postion (G,F,C)
library(plotly)
library(ggplot2)
plot_ly(
  data=nba_df,
  x=~salary,
  y=~PPG,
  frame = ~position,
  type='histogram2d'
) |>
  layout(
    title='NBA Salary vs Points Per Game By Position',
    xaxis=list(title='Salary'),
    yaxis=list(title='Points Per Game')
  )

# Interactive scatter salary vs value score 

p_interactive_value <- nba_df |>
  ggplot(aes(
    x     = salary_M,
    y     = value_score,
    color = position,
    text  = paste0(
      player_name,
      "<br>Team: ",     team,
      "<br>Position: ", position,
      "<br>Salary: $",  round(salary_M, 2), "M",
      "<br>PPG: ",      round(PPG, 1),
      "<br>RPG: ",      round(RPG, 1),
      "<br>APG: ",      round(APG, 1),
      "<br>Value: ",    round(value_score, 2)
    )
  )) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title   = "NBA Salary vs. Value Score",
    x       = "Salary (Millions USD)",
    y       = "Value Score (PPG + RPG + APG) / Salary (Millions)",
    color   = "Position",
    caption = "Hover to identify players; data: Basketball Reference + nbastatR"
  ) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(p_interactive_value, tooltip = "text")
