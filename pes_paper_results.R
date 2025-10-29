library(tidyverse)

pes_data <- readRDS(file = "lichess_data/pes_data_full.RDS")

overview_table <- read.csv("lichess_data/overview_table.csv")

pes_overview <- pes_data %>% 
  group_by(
    time_format
  ) %>% 
  summarize(
    mean_pes_secs = mean(pes) / 100,
    mean_time_control = mean(move_time_secs_control, na.rm = TRUE),
    mean_time_posterror = mean(move_time_secs_posterror, na.rm = TRUE),
    mean_acc_control = mean(corrected_move_eval_control, na.rm = TRUE),
    mean_acc_posterror = mean(corrected_move_eval_posterror, na.rm = TRUE),
    t_test_pes_t = t.test(move_time_posterror, move_time_control)$statistic,
    t_test_pes_p = t.test(move_time_posterror, move_time_control)$p.value,
    t_test_pes_bf = BayesFactor::ttestBF(move_time_posterror, move_time_control)@bayesFactor$bf,
    t_test_log_t = t.test(log(move_time_posterror), log(move_time_control))$statistic,
    t_test_log_p = t.test(log(move_time_posterror), log(move_time_control))$p.value,
    t_test_log_bf = BayesFactor::ttestBF(log(move_time_posterror), log(move_time_control))@bayesFactor$bf,
    t_test_pea_t = t.test(corrected_move_eval_posterror, corrected_move_eval_control)$statistic,
    t_test_pea_p = t.test(corrected_move_eval_posterror, corrected_move_eval_control)$p.value,
    t_test_pea_bf = BayesFactor::ttestBF(corrected_move_eval_posterror, corrected_move_eval_control)@bayesFactor$bf,
    d_pes = effectsize::cohens_d(move_time_posterror, move_time_control)$Cohens_d,
    d_pes_low = effectsize::cohens_d(move_time_posterror, move_time_control)$CI_low,
    d_pes_high = effectsize::cohens_d(move_time_posterror, move_time_control)$CI_high,
    d_pea = effectsize::cohens_d(corrected_move_eval_posterror, corrected_move_eval_control)$Cohens_d,
    d_pea_low = effectsize::cohens_d(corrected_move_eval_posterror, corrected_move_eval_control)$CI_low,
    d_pea_high = effectsize::cohens_d(corrected_move_eval_posterror, corrected_move_eval_control)$CI_high,
  )

overview_table <- overview_table %>% 
  mutate(num_errors = num_moves * perc_errors) %>% 
  left_join(., pes_overview) %>% 
  mutate_if(is.numeric, ~round(.,2 ))

write.csv(overview_table, file = "results/overview_table.csv")

## Moderator analysis -----
# Moderators: 
# Opp move time (opp_move_time_posterror)
# Remaining time on player click (clock_secs_posterror)
# Severity of the error (prev_own_move_eval_posterror)
# Speed of the error (prev_own_move_time_posterror)
# Elo rating (player_moving_elo_posterror)

source("helper_functions.R")
# plot_moderator_analysis(pes_data %>% filter(time_format == "bullet"), opp_move_time_secs_posterror)
# plot_moderator_analysis(pes_data %>% filter(time_format == "blitz"), opp_move_time_secs_posterror)
# plot_moderator_analysis(pes_data %>% filter(time_format == "rapid"), opp_move_time_secs_posterror)
# plot_moderator_analysis(pes_data %>% filter(time_format == "classical"), opp_move_time_secs_posterror)

### Opp Move time -----
# Extract each panel as its own ggplot
plots_move_time <- lapply(split(pes_data, factor(pes_data$time_format, levels = c("bullet", "blitz", "rapid", "classical"))), function(subdf) {
  plot_moderator_analysis(subdf, opp_move_time_secs_posterror)
})

# Combine and label with A–D
combined_move_time <- cowplot::plot_grid(
  plots_move_time[[1]], plots_move_time[[2]], plots_move_time[[3]], plots_move_time[[4]],
  labels = c("A", "B", "C", "D"),
  label_size = 18,
  ncol = 2
)

# Save the combined plot
ggplot2::ggsave(
  filename = "imgs/move_time_moderator.jpg",
  plot = combined_move_time,
  width = 16,       # adjust width as needed
  height = 9,       # adjust height as needed
  dpi = 300         # high-quality resolution for publication
)

### Time pressure -----
plots_time_pressure <- lapply(split(pes_data, factor(pes_data$time_format, levels = c("bullet", "blitz", "rapid", "classical"))), function(subdf) {
  plot_moderator_analysis(subdf, clock_secs_posterror)
})

# Combine and label with A–D
combined_time_pressure <- cowplot::plot_grid(
  plots_time_pressure[[1]], plots_time_pressure[[2]], plots_time_pressure[[3]], plots_time_pressure[[4]],
  labels = c("A", "B", "C", "D"),
  label_size = 18,
  ncol = 2
)

# Save the combined plot
ggplot2::ggsave(
  filename = "imgs/time_pressure_moderator.jpg",
  plot = combined_time_pressure,
  width = 16,       # adjust width as needed
  height = 9,       # adjust height as needed
  dpi = 300         # high-quality resolution for publication
)

### Error severity -----
plots_error_severity <- lapply(split(pes_data, factor(pes_data$time_format, levels = c("bullet", "blitz", "rapid", "classical"))), function(subdf) {
  plot_moderator_analysis(subdf, prev_own_move_eval_posterror)
})

# Combine and label with A–D
combined_error_severity <- cowplot::plot_grid(
  plots_error_severity[[1]], plots_error_severity[[2]], plots_error_severity[[3]], plots_error_severity[[4]],
  labels = c("A", "B", "C", "D"),
  label_size = 18,
  ncol = 2
)

# Save the combined plot
ggplot2::ggsave(
  filename = "imgs/error_severity_moderator.jpg",
  plot = combined_error_severity,
  width = 16,       # adjust width as needed
  height = 9,       # adjust height as needed
  dpi = 300         # high-quality resolution for publication
)

### Error speed -----
plots_error_speed <- lapply(split(pes_data, factor(pes_data$time_format, levels = c("bullet", "blitz", "rapid", "classical"))), function(subdf) {
  plot_moderator_analysis(subdf, prev_own_move_time_posterror)
})

# Combine and label with A–D
combined_error_speed <- cowplot::plot_grid(
  plots_error_speed[[1]], plots_error_speed[[2]], plots_error_speed[[3]], plots_error_speed[[4]],
  labels = c("A", "B", "C", "D"),
  label_size = 18,
  ncol = 2
)

# Save the combined plot
ggplot2::ggsave(
  filename = "imgs/error_speed_moderator.jpg",
  plot = combined_error_speed,
  width = 16,       # adjust width as needed
  height = 9,       # adjust height as needed
  dpi = 300         # high-quality resolution for publication
)

### Player ability -----
plots_player_ability <- lapply(split(pes_data, factor(pes_data$time_format, levels = c("bullet", "blitz", "rapid", "classical"))), function(subdf) {
  plot_moderator_analysis(subdf, player_moving_elo_posterror)
})

# Combine and label with A–D
combined_player_ability <- cowplot::plot_grid(
  plots_player_ability[[1]], plots_player_ability[[2]], plots_player_ability[[3]], plots_player_ability[[4]],
  labels = c("A", "B", "C", "D"),
  label_size = 18,
  ncol = 2
)

# Save the combined plot
ggplot2::ggsave(
  filename = "imgs/player_ability_moderator.jpg",
  plot = combined_player_ability,
  width = 16,       # adjust width as needed
  height = 9,       # adjust height as needed
  dpi = 300         # high-quality resolution for publication
)
