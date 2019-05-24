library(lubridate)
library(dplyr)
library(ggplot2)

results <- read.csv("data/results_1872_2018.csv", encoding = "UTF-8", stringsAsFactors = FALSE) %>%
  mutate(year = year(ymd(date))) %>%
  filter(year > 1980)
  

all_teams <- union(results$home_team, results$away_team)

agg_df <- NULL

for (team in all_teams) {
  
  played_home <- results %>% 
    filter(home_team == team)
  
  played_away <- results %>% 
    filter(away_team == team)
  
  temp_df <- data.frame(
    "team" = team,
    
    "local_matches" = nrow(played_home),
    "away_matches" = nrow(played_away),
    "total_matches" = nrow(played_home) + nrow(played_away),
    "friendly_matches" = sum(played_home$tournament == "Friendly", na.rm = TRUE) + sum(played_away$tournament == "Friendly", na.rm = TRUE),
    
    "matches_won_home" = sum(played_home$home_score > played_home$away_score),
    "matches_won_away" = sum(played_away$home_score < played_away$away_score),
    "matches_draw_home" = sum(played_home$home_score == played_home$away_score),
    "matches_draw_away" = sum(played_away$home_score == played_away$away_score),
    
    "goals_score_home" = sum(played_home$home_score, na.rm = TRUE),
    "goals_score_away" = sum(played_away$away_score, na.rm = TRUE),
    "goals_against_home" = sum(played_home$away_score, na.rm = TRUE), 
    "goals_against_away" = sum(played_away$home_score, na.rm = TRUE), 
    stringsAsFactors = FALSE)

  agg_df <- bind_rows(agg_df, temp_df)
    
}

write.csv(agg_df, "data/results_1980_2018.csv", row.names = FALSE)


agg_df <- agg_df %>% 
  mutate(share_local_wins = matches_won_home / local_matches,
         share_away_wins = matches_won_away / away_matches,
         goals_scored_per_match_home = goals_score_home / local_matches,
         goals_scored_per_match_away = goals_score_away / away_matches,
         goals_against_per_match_home = goals_against_home / local_matches,
         goals_against_per_match_away = goals_against_away / away_matches) %>%
  filter(complete.cases(.))


vars_cluster <- c("share_local_wins", "share_away_wins",
                  "goals_scored_per_match_home", "goals_scored_per_match_away",
                  "goals_against_per_match_home", "goals_against_per_match_away",
                  "total_matches")

data_kmeans <- agg_df[, vars_cluster]
data_kmeans <- scale(data_kmeans)


kmeans_model <- kmeans(agg_df[, vars_cluster], 5)

agg_df <- agg_df %>% 
  mutate(cluster = kmeans_model$cluster)

split(agg_df$team, agg_df$cluster)

agg_df %>%
  mutate(share_total_wins = (matches_won_home + matches_won_away) / total_matches) %>%
  group_by(cluster) %>%
  summarise(mean_win_ratio = mean(share_total_wins)) %>%
  ggplot(aes(x = reorder(cluster, mean_win_ratio), y = mean_win_ratio)) +
  geom_col() +
  coord_flip()

ggplot(agg_df, aes(x = matches_won_home, y = matches_won_away, col = factor(cluster))) +
  geom_point()


windows()

