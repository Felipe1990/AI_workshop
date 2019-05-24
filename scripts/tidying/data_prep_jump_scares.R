library(dplyr)
library(ggplot2)
library(tidyr)

jump_scares <- read.csv("data/jump_scare_movies.csv", stringsAsFactors = FALSE)

hist(jump_scares$jump_score)

# como ha cambiado en el tiempo?

yearly_ev_jumps <- jump_scares %>%
  group_by(year) %>%
  summarise(mean_jump_score = mean(jump_score),
            mean_imdb_score = mean(imdb_score))

ggplot(yearly_ev_jumps, aes(x = year, y = mean_jump_score)) +
  geom_line() +
  geom_line(aes(y = mean_imdb_score))

##

yearly_ev_jumps <- jump_scares %>%
  group_by(year) %>%
  summarise(mean_jump_score = mean(jump_score),
            mean_imdb_score = mean(imdb_score)) %>%
  gather(key = variable, value = variable_value, -1)

ggplot(yearly_ev_jumps, aes(x = year, y = variable_value, col = variable)) +
  geom_line() 

##

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
#

ggplot(yearly_ev_jumps, aes(x = year, y = variable_value, col = variable)) +
  geom_hline(yintercept = 100, linetype = 2) +
  geom_line() +
  theme(legend.position = "bottom")

#

jump_scares %>%
  filter(year == 2001)

jump_scares %>%
  filter(year == 2003)

# cuenta de jump scare afecta la calidad?

# transform data 

jump_reg_data <- jump_scares %>% 
  mutate(group_jumps = cut(jump_score, breaks = c(0, 2, 4, 5), 
                            labels = c("low", "mid", "high"), include.lowest = TRUE, right = TRUE))

lm_jump <- lm(data = jump_reg_data, imdb_score ~ group_jumps)

broom::tidy(lm_jump) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.1) +
  coord_flip()

