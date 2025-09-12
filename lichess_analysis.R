library(tidyverse)
library(data.table)

full_data <- readRDS("lichess_data/full_data.RDS")

full_data$log_move_time <- log(full_data$move_time)

full_data %>% 
  ggplot(
    aes(log_move_time, group = group, fill = group)
  )+
  geom_density(alpha = 0.5)

t.test(log_move_time ~ prev_own_mistake, full_data)

pes_data <- full_data %>%
  pivot_wider(
    names_from = group,
    values_from = !subclass  
  ) %>% 
  mutate(pes = move_time_posterror - move_time_control)

effectsize::cohens_d(
  pes_data$log_move_time_control,
  pes_data$log_move_time_posterror
)

effectsize::cohens_d(
  pes_data$move_time_control,
  pes_data$move_time_posterror
)

pes_data %>% 
  mutate(
    interaction = move_num_posterror
  ) %>% 
  group_by(interaction) %>% 
  summarize(
    mean_pes = mean(pes),
    sd_pes = sd(pes)
  ) %>% 
  ggplot(
    aes(
      x = interaction,
      y = mean_pes
    )
  )+
  geom_point()+
  geom_smooth()

pes_data %>%
  ggplot(
    aes(
      x = move_num_posterror,
      y = pes
    )
  ) +
  geom_smooth(method = "loess", se = FALSE)

pes_data %>%
  ggplot(
    aes(
      x = player_moving_elo_posterror,
      y = pes
    )
  ) +
  geom_smooth(method = "loess", se = FALSE)

pes_data %>%
  ggplot(
    aes(
      x = clock_posterror,
      y = pes
    )
  ) +
  geom_smooth(method = "loess", se = FALSE)

hist(pes_data$pes)
hist(pes_data$move_time_posterror)
hist(pes_data$move_time_control)
