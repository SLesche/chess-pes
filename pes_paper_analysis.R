library(tidyverse)
library(data.table)

data_before_match <- readRDS("lichess_data/data_before_match_350k_bullet.RDS")

bullet_avg_response_speed <- mean(data_before_match$move_time_secs)
bullet_sd_response_speed <- sd(data_before_match$move_time_secs)
bullet_median_response_speed <- median(data_before_match$move_time_secs)
bullet_avg_response_acc <- mean(data_before_match$corrected_move_eval)
bullet_sd_response_acc <- sd(data_before_match$corrected_move_eval)
bullet_perc_errors <- mean(data_before_match$mistake == 1)
bullet_num_moves <- nrow(data_before_match)

data_before_match <- readRDS("lichess_data/data_before_match_350k_blitz.RDS")

blitz_avg_response_speed <- mean(data_before_match$move_time_secs)
blitz_sd_response_speed <- sd(data_before_match$move_time_secs)
blitz_median_response_speed <- median(data_before_match$move_time_secs)
blitz_avg_response_acc <- mean(data_before_match$corrected_move_eval)
blitz_sd_response_acc <- sd(data_before_match$corrected_move_eval)
blitz_perc_errors <- mean(data_before_match$mistake == 1)
blitz_num_moves <- nrow(data_before_match)

data_before_match <- readRDS("lichess_data/data_before_match_350k_rapid.RDS")

rapid_avg_response_speed <- mean(data_before_match$move_time_secs)
rapid_sd_response_speed <- sd(data_before_match$move_time_secs)
rapid_median_response_speed <- median(data_before_match$move_time_secs)
rapid_avg_response_acc <- mean(data_before_match$corrected_move_eval)
rapid_sd_response_acc <- sd(data_before_match$corrected_move_eval)
rapid_perc_errors <- mean(data_before_match$mistake == 1)
rapid_num_moves <- nrow(data_before_match)

# data_before_match <- readRDS("lichess_data/data_before_match_350k_classical.RDS")
# 
# classical_avg_response_speed <- mean(data_before_match$move_time_secs)
# classical_sd_response_speed <- sd(data_before_match$move_time_secs)
# classical_median_response_speed <- median(data_before_match$move_time_secs)
# classical_avg_response_acc <- mean(data_before_match$corrected_move_eval)
# classical_sd_response_acc <- sd(data_before_match$corrected_move_eval)
# classical_perc_errors <- mean(data_before_match$mistake == 1)
# classical_num_moves <- nrow(data_before_match)

full_data_bullet <- readRDS("lichess_data/full_data_350k_bullet.RDS")

pes_data_bullet <- full_data_bullet %>%
  mutate(subclass = paste0(subclass, ".", batch)) %>% 
  pivot_wider(
    names_from = group,
    values_from = !subclass  
  ) %>% 
  mutate(pes = move_time_posterror - move_time_control,
         pea = corrected_move_eval_posterror - corrected_move_eval_control)

rm(full_data_bullet)

full_data_blitz <- readRDS("lichess_data/full_data_350k_blitz.RDS")

pes_data_blitz <- full_data_blitz %>%
  mutate(subclass = paste0(subclass, ".", batch)) %>% 
  pivot_wider(
    names_from = group,
    values_from = !subclass  
  ) %>% 
  mutate(pes = move_time_posterror - move_time_control,
         pea = corrected_move_eval_posterror - corrected_move_eval_control)

rm(full_data_blitz)

full_data_rapid <- readRDS("lichess_data/full_data_350k_rapid.RDS")

pes_data_rapid <- full_data_rapid %>%
  mutate(subclass = paste0(subclass, ".", batch)) %>% 
  pivot_wider(
    names_from = group,
    values_from = !subclass  
  ) %>% 
  mutate(pes = move_time_posterror - move_time_control,
         pea = corrected_move_eval_posterror - corrected_move_eval_control)

rm(full_data_rapid)


pes_data <- rbind(
  pes_data_bullet %>% mutate(time_format = "bullet"),
  pes_data_blitz %>% mutate(time_format = "blitz"),
  pes_data_rapid %>% mutate(time_format = "rapid")
)

rm(pes_data_bullet, pes_data_blitz, pes_data_rapid)

# Moderators: 
# Opp move time (opp_move_time_posterror)
# Remaining time on player click (clock_secs_posterror)
# Severity of the error (prev_own_move_eval_posterror)
# Speed of the error (prev_own_move_time_posterror)
# Elo rating (player_moving_elo_posterror)

source("helper_functions.R")
plot_moderator_analysis(pes_data %>% filter(time_format == "bullet"), player_moving_elo_posterror)
plot_moderator_analysis(pes_data %>% filter(time_format == "blitz"), player_moving_elo_posterror)
plot_moderator_analysis(pes_data %>% filter(time_format == "rapid"), player_moving_elo_posterror)

# plot_moderator_grouped(pes_data, player_moving_elo_posterror)

# this is shit. Make them separate