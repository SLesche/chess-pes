library(tidyverse)

pes_data <- readRDS(file = "lichess_data/pes_data_full.RDS")

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
