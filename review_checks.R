library(tidyverse)

pes_data <- readRDS(file = "lichess_data/pes_data_full.RDS")

## Check relationship move time and acc (overall, not PES/PEA)
source("helper_functions.R")

# check it in control moves as representative sample to post-error moves
plots_rt_acc <- lapply(split(pes_data, factor(pes_data$time_format, levels = c("bullet", "blitz", "rapid", "classical"))), function(subdf) {
  plot_moderator_analysis(
    subdf, 
    move_time_control,
    corrected_move_eval_control,
    player_var = player_moving_posterror,
    "Move time (in s)",
    labs(x = "Accuracy (eval)")
  )
})

combined_rt_acc <- cowplot::plot_grid(
  plots_rt_acc[[1]], plots_rt_acc[[2]], plots_rt_acc[[3]], plots_rt_acc[[4]],
  labels = c("A", "B", "C", "D"),
  label_size = 30,
  ncol = 2
)

# Save the combined plot
ggplot2::ggsave(
  filename = "imgs/rt_acc_relationship.jpg",
  plot = combined_rt_acc,
  width = 16,       # adjust width as needed
  height = 9,       # adjust height as needed
  dpi = 300         # high-quality resolution for publication
)

pes_data_3 <- pes_data %>% 
  filter(prev_own_move_eval_posterror < -3)

pes_data_5 <- pes_data %>% 
  filter(prev_own_move_eval_posterror < -5)

## Rerun analysis with 3 as cutoff
pes_overview_3 <- pes_data_3 %>% 
  group_by(
    time_format
  ) %>% 
  summarize(
    mean_pes_secs = mean(pes) / 100,
    sd_pes_secs = sd(pes/100),
    mean_pea = mean(pea),
    mean_time_control = mean(move_time_secs_control, na.rm = TRUE),
    sd_time_control = sd(move_time_secs_control),
    mean_time_posterror = mean(move_time_secs_posterror, na.rm = TRUE),
    sd_time_posterror = sd(move_time_secs_posterror),
    mean_acc_control = mean(corrected_move_eval_control, na.rm = TRUE),
    sd_acc_control = sd(corrected_move_eval_control),
    mean_acc_posterror = mean(corrected_move_eval_posterror, na.rm = TRUE),
    sd_acc_posterror = sd(corrected_move_eval_posterror, na.rm = TRUE),
    t_test_pes_t = t.test(move_time_posterror, move_time_control)$statistic,
    t_test_pes_p = t.test(move_time_posterror, move_time_control)$p.value,
    t_test_pes_df = t.test(move_time_posterror, move_time_control)$parameter,
    # t_test_pes_bf = BayesFactor::ttestBF(move_time_posterror, move_time_control)@bayesFactor$bf,
    t_test_log_t = t.test(log(move_time_posterror), log(move_time_control))$statistic,
    t_test_log_p = t.test(log(move_time_posterror), log(move_time_control))$p.value,
    t_test_log_df = t.test(log(move_time_posterror), log(move_time_control))$parameter,
    # t_test_log_bf = BayesFactor::ttestBF(log(move_time_posterror), log(move_time_control))@bayesFactor$bf,
    t_test_pea_t = t.test(corrected_move_eval_posterror, corrected_move_eval_control)$statistic,
    t_test_pea_p = t.test(corrected_move_eval_posterror, corrected_move_eval_control)$p.value,
    t_test_pea_df = t.test(corrected_move_eval_posterror, corrected_move_eval_control)$parameter,
    # t_test_pea_bf = BayesFactor::ttestBF(corrected_move_eval_posterror, corrected_move_eval_control)@bayesFactor$bf,
    d_pes = effectsize::cohens_d(move_time_posterror, move_time_control)$Cohens_d,
    d_pes_low = effectsize::cohens_d(move_time_posterror, move_time_control)$CI_low,
    d_pes_high = effectsize::cohens_d(move_time_posterror, move_time_control)$CI_high,
    d_pea = effectsize::cohens_d(corrected_move_eval_posterror, corrected_move_eval_control)$Cohens_d,
    d_pea_low = effectsize::cohens_d(corrected_move_eval_posterror, corrected_move_eval_control)$CI_low,
    d_pea_high = effectsize::cohens_d(corrected_move_eval_posterror, corrected_move_eval_control)$CI_high,
  )

pes_overview_5 <- pes_data_5 %>% 
  group_by(
    time_format
  ) %>% 
  summarize(
    mean_pes_secs = mean(pes) / 100,
    sd_pes_secs = sd(pes/100),
    mean_pea = mean(pea),
    mean_time_control = mean(move_time_secs_control, na.rm = TRUE),
    sd_time_control = sd(move_time_secs_control),
    mean_time_posterror = mean(move_time_secs_posterror, na.rm = TRUE),
    sd_time_posterror = sd(move_time_secs_posterror),
    mean_acc_control = mean(corrected_move_eval_control, na.rm = TRUE),
    sd_acc_control = sd(corrected_move_eval_control),
    mean_acc_posterror = mean(corrected_move_eval_posterror, na.rm = TRUE),
    sd_acc_posterror = sd(corrected_move_eval_posterror, na.rm = TRUE),
    t_test_pes_t = t.test(move_time_posterror, move_time_control)$statistic,
    t_test_pes_p = t.test(move_time_posterror, move_time_control)$p.value,
    t_test_pes_df = t.test(move_time_posterror, move_time_control)$parameter,
    # t_test_pes_bf = BayesFactor::ttestBF(move_time_posterror, move_time_control)@bayesFactor$bf,
    t_test_log_t = t.test(log(move_time_posterror), log(move_time_control))$statistic,
    t_test_log_p = t.test(log(move_time_posterror), log(move_time_control))$p.value,
    t_test_log_df = t.test(log(move_time_posterror), log(move_time_control))$parameter,
    # t_test_log_bf = BayesFactor::ttestBF(log(move_time_posterror), log(move_time_control))@bayesFactor$bf,
    t_test_pea_t = t.test(corrected_move_eval_posterror, corrected_move_eval_control)$statistic,
    t_test_pea_p = t.test(corrected_move_eval_posterror, corrected_move_eval_control)$p.value,
    t_test_pea_df = t.test(corrected_move_eval_posterror, corrected_move_eval_control)$parameter,
    # t_test_pea_bf = BayesFactor::ttestBF(corrected_move_eval_posterror, corrected_move_eval_control)@bayesFactor$bf,
    d_pes = effectsize::cohens_d(move_time_posterror, move_time_control)$Cohens_d,
    d_pes_low = effectsize::cohens_d(move_time_posterror, move_time_control)$CI_low,
    d_pes_high = effectsize::cohens_d(move_time_posterror, move_time_control)$CI_high,
    d_pea = effectsize::cohens_d(corrected_move_eval_posterror, corrected_move_eval_control)$Cohens_d,
    d_pea_low = effectsize::cohens_d(corrected_move_eval_posterror, corrected_move_eval_control)$CI_low,
    d_pea_high = effectsize::cohens_d(corrected_move_eval_posterror, corrected_move_eval_control)$CI_high,
  )

# Extract each panel as its own ggplot
plots_move_time_3 <- lapply(split(pes_data_3, factor(pes_data_3$time_format, levels = c("bullet", "blitz", "rapid", "classical"))), function(subdf) {
  plot_moderator_analysis(
    subdf,
    opp_move_time_secs_posterror,
    pes,
    "RSI (in s)",
    "PES (in ms)",
    y_limits = c(quantile(subdf$pes, 0.45), quantile(subdf$pes, 0.65))
  )
})

# Combine and label with A–D
combined_move_time_3 <- cowplot::plot_grid(
  plots_move_time_3[[1]], plots_move_time_3[[2]], plots_move_time_3[[3]], plots_move_time_3[[4]],
  labels = c("A", "B", "C", "D"),
  label_size = 30,
  ncol = 2
)

plots_move_time_5 <- lapply(split(pes_data_5, factor(pes_data_5$time_format, levels = c("bullet", "blitz", "rapid", "classical"))), function(subdf) {
  plot_moderator_analysis(
    subdf,
    opp_move_time_secs_posterror,
    pes,
    "RSI (in s)",
    "PES (in ms)",
    y_limits = c(quantile(subdf$pes, 0.45), quantile(subdf$pes, 0.65))
  )
})

# Combine and label with A–D
combined_move_time_5 <- cowplot::plot_grid(
  plots_move_time_5[[1]], plots_move_time_5[[2]], plots_move_time_5[[3]], plots_move_time_5[[4]],
  labels = c("A", "B", "C", "D"),
  label_size = 30,
  ncol = 2
)

# Player ability
plots_player_ability_3 <- lapply(split(pes_data_3, factor(pes_data_3$time_format, levels = c("bullet", "blitz", "rapid", "classical"))), function(subdf) {
  plot_moderator_analysis(
    subdf, 
    player_moving_elo_posterror,
    pea,
    "Player Ability (in elo)",
    labs(x = "PEA (Δ eval)"),
    y_limits = c(-1, 1)
  )
})

# Combine and label with A–D
combined_player_ability_3 <- cowplot::plot_grid(
  plots_player_ability_3[[1]], plots_player_ability_3[[2]], plots_player_ability_3[[3]], plots_player_ability_3[[4]],
  labels = c("A", "B", "C", "D"),
  label_size = 30,
  ncol = 2
)

plots_player_ability_5 <- lapply(split(pes_data_5, factor(pes_data_5$time_format, levels = c("bullet", "blitz", "rapid", "classical"))), function(subdf) {
  plot_moderator_analysis(
    subdf, 
    player_moving_elo_posterror,
    pea,
    "Player Ability (in elo)",
    labs(x = "PEA (Δ eval)"),
    y_limits = c(-1, 1)
  )
})

# Combine and label with A–D
combined_player_ability_5 <- cowplot::plot_grid(
  plots_player_ability_5[[1]], plots_player_ability_5[[2]], plots_player_ability_5[[3]], plots_player_ability_5[[4]],
  labels = c("A", "B", "C", "D"),
  label_size = 30,
  ncol = 2
)

