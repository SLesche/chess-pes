library(tidyverse)
library(data.table)

full_data <- readRDS("lichess_data/full_data_350k_blitz_elo.RDS")

full_data$log_move_time <- log(full_data$move_time)

full_data %>% 
  ggplot(
    aes(opponent_elo, group = group, fill = group)
  )+
  geom_density(alpha = 0.5)

t.test(log_move_time ~ prev_own_mistake, full_data)
t.test(move_time_secs ~ prev_own_mistake, full_data)

t.test(corrected_move_eval ~ prev_own_mistake, full_data)

pes_data <- full_data %>%
  mutate(subclass = paste0(subclass, ".", batch)) %>% 
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

effectsize::cohens_d(
  pes_data$corrected_move_eval_control,
  pes_data$corrected_move_eval_posterror
)

eff_data <- pes_data %>% 
  mutate(
    interaction = prev_own_move_time_posterror
  ) %>% 
  group_by(interaction, mistake_made_in_losing_position_posterror, mistake_flipped_sign_posterror) %>% 
  # group_by(interaction) %>% 
  summarize(
    mean_pes = mean(pes),
    sd_pes = sd(pes),
    effsize= effectsize::cohens_d(
      move_time_control,
      move_time_posterror
    )
  ) %>% 
  janitor::clean_names()

colnames(eff_data) <- c("interaction", "mistake_in_losing_position_posterror", "sign_flipped_posterror", "mean_pes", "sd_pes", "cohen_d")
# colnames(eff_data) <- c("interaction", "mean_pes", "sd_pes", "cohen_d", "ci", "ci_low", "ci_high")

eff_data %>% 
  ggplot(
    aes(
      x = interaction,
      y = cohen_d$Cohens_d,
      # y = mean_pes
    )
  )+
  geom_point()+
  geom_smooth()+
  # facet_wrap(~interaction(mistake_in_losing_position_posterror, sign_flipped_posterror))+
  geom_hline(yintercept = 0, color = "red", linetype = "dashed")

eff_data %>% View()

