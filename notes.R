data <- readRDS("lichess_data/data_before_match_350k_rapid.RDS")

data <- data %>% 
  mutate(
    player_moving_elo_hun = round(player_moving_elo, -2)
  )    


pes_data %>%
  filter(time_format == "bullet") %>% 
  summarize(q = quantile(opp_move_time_secs_posterror, probs = seq(0.05, 0.95, 0.1), na.rm = TRUE)) %>%
  pull(q)
  
# But dont see anything in the plots
## PES Analysis -----
pes_data %>% 
  select(
    time_format,
    move_time_posterror,
    move_time_control,
    corrected_move_eval_posterror,
    corrected_move_eval_control
  ) %>% 
  pivot_longer(
    cols = -time_format,
    names_to = c(".value", "condition"),   # split names into value & condition
    names_pattern = "(.*)_(posterror|control)"  # regex to separate base name and condition
  ) %>%
  mutate(
    log_move_time = log(move_time)
  ) %>% 
  ggplot() +
  gghalves::geom_half_violin(
    aes(x = factor(time_format), y = corrected_move_eval, split = condition, fill = condition),  # use side = "control" or "posterror"
    position = "identity"
  ) +
  # theme_classic() +
  labs(
    x = "Time Format",
    y = "Log Response Time (s)",
    fill = "Condition"
  )

ggplot(
  aes(x = time_format, y = move_time, fill = condition, group = condition)
) +
  introdataviz::geom_split_violin(alpha = .4, trim = FALSE) +
  # geom_boxplot(width = .2, alpha = .6, fatten = NULL, show.legend = FALSE) +
  # stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
  #              position = position_dodge(.175)) +
  # scale_x_discrete(name = "Condition", labels = c("Non-word", "Word")) +
  # scale_y_continuous(name = "Reaction time (ms)",
  #                    breaks = seq(200, 800, 100), 
  #                    limits = c(200, 800)) +
  # scale_fill_brewer(palette = "Dark2", name = "Language group") +
  theme_minimal()