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

data_before_match <- readRDS("lichess_data/data_before_match_350k_classical.RDS")

classical_avg_response_speed <- mean(data_before_match$move_time_secs)
classical_sd_response_speed <- sd(data_before_match$move_time_secs)
classical_median_response_speed <- median(data_before_match$move_time_secs)
classical_avg_response_acc <- mean(data_before_match$corrected_move_eval)
classical_sd_response_acc <- sd(data_before_match$corrected_move_eval)
classical_perc_errors <- mean(data_before_match$mistake == 1)
classical_num_moves <- nrow(data_before_match)


result_overview <- data.frame(
  time_format = c("bullet", "blitz", "rapid", "classical"),
  avg_speed = c(
    bullet_avg_response_speed,
    blitz_avg_response_speed,
    rapid_avg_response_speed,
    classical_avg_response_speed
  ),
  sd_speed = c(
    bullet_sd_response_speed,
    blitz_sd_response_speed,
    rapid_sd_response_speed,
    classical_sd_response_speed
  ),
  median_speed = c(
    bullet_median_response_speed,
    blitz_median_response_speed,
    rapid_median_response_speed,
    classical_median_response_speed
  ),
  avg_acc = c(
    bullet_avg_response_acc,
    blitz_avg_response_acc,
    rapid_avg_response_acc,
    classical_avg_response_acc
  ),
  sd_acc = c(
    bullet_sd_response_acc,
    blitz_sd_response_acc,
    rapid_sd_response_acc,
    classical_sd_response_acc
  ),
  perc_errors = c(
    bullet_perc_errors,
    blitz_perc_errors,
    rapid_perc_errors,
    classical_perc_errors
  ),
  num_moves = c(
    bullet_num_moves,
    blitz_num_moves,
    rapid_num_moves,
    classical_num_moves
  )
)

write.csv(result_overview, file = "lichess_data/overview_table.csv")
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

full_data_classical <- readRDS("lichess_data/full_data_350k_classical.RDS")

pes_data_classical <- full_data_classical %>%
  mutate(subclass = paste0(subclass, ".", batch)) %>% 
  pivot_wider(
    names_from = group,
    values_from = !subclass  
  ) %>% 
  mutate(pes = move_time_posterror - move_time_control,
         pea = corrected_move_eval_posterror - corrected_move_eval_control)

rm(full_data_classical)


pes_data <- rbind(
  pes_data_bullet %>% mutate(time_format = "bullet"),
  pes_data_blitz %>% mutate(time_format = "blitz"),
  pes_data_rapid %>% mutate(time_format = "rapid"),
  pes_data_classical %>% mutate(time_format = "classical")
)

rm(pes_data_bullet, pes_data_blitz, pes_data_rapid, pes_data_classical)

saveRDS(pes_data, file = "lichess_data/pes_data_full.RDS")