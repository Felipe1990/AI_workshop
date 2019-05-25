# cargar paquetes ---------------------------------------------------------

library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(esquisse)


# cargar datos ------------------------------------------------------------

jump_scares <- read.csv("ejemplos/jumpscares/data/input/jump_scare_movies.csv",
                        stringsAsFactors = FALSE)

# revisar datos -----------------------------------------------------------

dim(jump_scares)
names(jump_scares)
sapply(jump_scares, function(x){sum(is.na(x))}) # revisar donde hay missing values

# revision visual de las variables ----------------------------------------

esquisser(data = jump_scares)

# comportamiento en el tiempo ---------------------------------------------

# de la manera inapropiada usando ggplot
yearly_ev_jumps <- jump_scares %>%
  group_by(year) %>%
  summarise(mean_jump_score = mean(jump_score),
            mean_imdb_score = mean(imdb_score))

ggplot(yearly_ev_jumps, aes(x = year, y = mean_jump_score)) +
  geom_line() +
  geom_line(aes(y = mean_imdb_score))

# la manera apropiada usando ggplot
yearly_ev_jumps <- jump_scares %>%
  group_by(year) %>%
  summarise(mean_jump_score = mean(jump_score),
            mean_imdb_score = mean(imdb_score)) %>%
  gather(key = variable, value = variable_value, -1)

ggplot(yearly_ev_jumps, aes(x = year, y = variable_value, col = variable)) +
  geom_line() 

## haciendolo comparable
yearly_ev_jumps <- jump_scares %>%
  group_by(year) %>%
  summarise(mean_jump_score = mean(jump_score),
            mean_imdb_score = mean(imdb_score)) %>%
  ungroup() %>%
  filter(year >= 1990) %>%
  arrange(year) %>%
  mutate(index_jump = mean_jump_score / first(mean_jump_score) * 100,
         index_imdb = mean_imdb_score / first(mean_imdb_score) * 100) %>%
  select(year, index_jump, index_imdb) %>%
  gather(key = variable, value = variable_value, -1)

ggplot(yearly_ev_jumps, aes(x = year, y = variable_value, col = variable)) +
  geom_line() 

## ajustemoslo un poco
ggplot(yearly_ev_jumps, aes(x = year, y = variable_value, col = variable)) +
  geom_hline(yintercept = 100, linetype = 2) +
  geom_line() +
  theme(legend.position = "bottom")

# revisando algunos años
jump_scares %>%
  filter(year == 2001)

jump_scares %>%
  filter(year == 2003)

# ¿el puntaje de jump scare afecta la calidad?

# creemos tres categorias y usemos una regresión linear para saberlo

jump_reg_data <- jump_scares %>% 
  mutate(group_jumps = cut(jump_score, breaks = c(0, 2, 4, 5), 
                           labels = c("low", "mid", "high"), include.lowest = TRUE, right = TRUE))

lm_jump <- lm(data = jump_reg_data, imdb_score ~ group_jumps)

summary(lm_jump)

broom::tidy(lm_jump) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.1) +
  coord_flip()


