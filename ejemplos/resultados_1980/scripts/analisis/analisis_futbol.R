# cargar paquetes ---------------------------------------------------------

library(lubridate)
library(dplyr)
library(ggplot2)
library(esquisse)

# cargar datos ------------------------------------------------------------

data_partidos <- read.csv("ejemplos/resultados_1980/data/input/results_1980_2018.csv", 
                          stringsAsFactors = TRUE, 
                          encoding = "UTF-8")


# revisar datos -----------------------------------------------------------

dim(data_partidos)
names(data_partidos)
sapply(data_partidos, function(x){sum(is.na(x))}) # revisar donde hay missing values


# revision visual de las variables ----------------------------------------

esquisser(data = data_partidos)


# creando nuevas variables ------------------------------------------------

data_partidos <- data_partidos %>% 
  mutate(share_local_wins = matches_won_home / local_matches,
         share_away_wins = matches_won_away / away_matches,
         goals_scored_per_match_home = goals_score_home / local_matches,
         goals_scored_per_match_away = goals_score_away / away_matches,
         goals_against_per_match_home = goals_against_home / local_matches,
         goals_against_per_match_away = goals_against_away / away_matches) %>%
  filter(complete.cases(.))


# creemos grupos de paises similares segun sus resultados -----------------

vars_cluster <- c("share_local_wins", "share_away_wins",
                  "goals_scored_per_match_home", "goals_scored_per_match_away",
                  "goals_against_per_match_home", "goals_against_per_match_away",
                  "total_matches")


data_kmeans <- data_partidos[, vars_cluster]
data_kmeans <- scale(data_kmeans)

kmeans_model <- kmeans(data_partidos[, vars_cluster], 5)

data_partidos <- data_partidos %>% 
  mutate(cluster = kmeans_model$cluster)

split(data_partidos$team, data_partidos$cluster) # revision de los resultados


# revisión visual de los resultados ---------------------------------------

windows()

ggplot(data_partidos, aes(x = matches_won_home, y = matches_won_away, col = factor(cluster))) +
  geom_point()

data_partidos %>%
  mutate(share_total_wins = (matches_won_home + matches_won_away) / total_matches) %>%
  group_by(cluster) %>%
  summarise(mean_win_ratio = mean(share_total_wins)) %>%
  ggplot(aes(x = reorder(cluster, mean_win_ratio), y = mean_win_ratio)) +
  geom_col() +
  coord_flip()




