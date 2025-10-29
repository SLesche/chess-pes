library(tidyverse)

pes_data <- readRDS(file = "lichess_data/pes_data_full.RDS")
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