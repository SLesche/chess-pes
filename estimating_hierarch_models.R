library(tidyverse)
library(lme4)
library(splines)

pes_data <- readRDS(file = "lichess_data/pes_data_full.RDS")

model_data <- pes_data %>% 
  mutate(match_id = row_number()) %>% 
  select(id_control, id_posterror,
         player_moving_control, player_moving_posterror,
         move_time_control, move_time_posterror,
         corrected_move_eval_posterror, corrected_move_eval_control,
         match_id,
         time_format, opp_move_time_secs_posterror, prev_own_move_eval_posterror,
         player_moving_elo_posterror)

rm(pes_data)

# Convert matched observations to long format
model_data_long <- model_data %>%
  pivot_longer(
    cols = c(
      id_control, id_posterror,
      player_moving_control, player_moving_posterror,
      move_time_control, move_time_posterror,
      corrected_move_eval_posterror, corrected_move_eval_control
    ),
    names_to = c(".value", "condition"),
    names_pattern = "(id|player_moving|move_time|corrected_move_eval)_(control|posterror)"
  )

# Models ----
model_bullet_rt <- lmer(
  move_time ~ condition +
    # (1 | match_id) +
    (1 | player_moving),
  data = model_data_long %>% filter(time_format == "bullet")
)

saveRDS(model_bullet_rt, "results/movel_bullet_rt.RDS")

model_bullet_acc <- lmer(
  corrected_move_eval ~ condition +
    # (1 | match_id) +
    (1 | player_moving),
  data = model_data_long %>% filter(time_format == "bullet")
)

saveRDS(model_bullet_acc, "results/movel_bullet_acc.RDS")

# Blitz
model_blitz_rt <- lmer(
  move_time ~ condition +
    # (1 | match_id) +
    (1 | player_moving),
  data = model_data_long %>% filter(time_format == "blitz")
)

saveRDS(model_blitz_rt, "results/movel_blitz_rt.RDS")

model_blitz_acc <- lmer(
  corrected_move_eval ~ condition +
    # (1 | match_id) +
    (1 | player_moving),
  data = model_data_long %>% filter(time_format == "blitz")
)

saveRDS(model_blitz_acc, "results/movel_blitz_acc.RDS")

# Rapid
model_rapid_rt <- lmer(
  move_time ~ condition +
    # (1 | match_id) +
    (1 | player_moving),
  data = model_data_long %>% filter(time_format == "rapid")
)

saveRDS(model_rapid_rt, "results/movel_rapid_rt.RDS")

model_rapid_acc <- lmer(
  corrected_move_eval ~ condition +
    # (1 | match_id) +
    (1 | player_moving),
  data = model_data_long %>% filter(time_format == "rapid")
)

saveRDS(model_rapid_acc, "results/movel_rapid_acc.RDS")

# Classical
model_classical_rt <- lmer(
  move_time ~ condition +
    # (1 | match_id) +
    (1 | player_moving),
  data = model_data_long %>% filter(time_format == "classical")
)

saveRDS(model_classical_rt, "results/movel_classical_rt.RDS")

model_classical_acc <- lmer(
  corrected_move_eval ~ condition +
    # (1 | match_id) +
    (1 | player_moving),
  data = model_data_long %>% filter(time_format == "classical")
)

saveRDS(model_classical_acc, "results/movel_classical_acc.RDS")
