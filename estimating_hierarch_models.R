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
         player_moving_elo_posterror,
         pes, pea)

rm(pes_data)

# Models ----
model_bullet_rt <- lmer(
  pes ~ 1 +
    (1 | player_moving_posterror),
  data = model_data %>% filter(time_format == "bullet")
)

saveRDS(model_bullet_rt, "results/movel_bullet_rt.RDS")

# Comp those models once, see if the player moving intercept is beneficial

model_bullet_acc <- lmer(
  pea ~ 1 +
    (1 | player_moving_posterror),
  data = model_data %>% filter(time_format == "bullet")
)

saveRDS(model_bullet_acc, "results/movel_bullet_acc.RDS")

# Blitz
model_blitz_rt <- lmer(
  pes ~ 1 +
    (1 | player_moving_posterror),
  data = model_data %>% filter(time_format == "blitz")
)

saveRDS(model_blitz_rt, "results/movel_blitz_rt.RDS")

model_blitz_acc <- lmer(
  pea ~ 1 +
    (1 | player_moving_posterror),
  data = model_data %>% filter(time_format == "blitz")
)

saveRDS(model_blitz_acc, "results/movel_blitz_acc.RDS")

# Rapid
model_rapid_rt <- lmer(
  pes ~ 1 +
    (1 | player_moving_posterror),
  data = model_data %>% filter(time_format == "rapid")
)

saveRDS(model_rapid_rt, "results/movel_rapid_rt.RDS")

model_rapid_acc <- lmer(
  pea ~ 1 +
    (1 | player_moving_posterror),
  data = model_data %>% filter(time_format == "rapid")
)

saveRDS(model_rapid_acc, "results/movel_rapid_acc.RDS")

# Classical
model_classical_rt <- lmer(
  pes ~ 1 +
    (1 | player_moving_posterror),
  data = model_data %>% filter(time_format == "classical")
)

saveRDS(model_classical_rt, "results/movel_classical_rt.RDS")

model_classical_acc <- lmer(
  pea ~ 1 +
    (1 | player_moving_posterror),
  data = model_data %>% filter(time_format == "classical")
)

saveRDS(model_classical_acc, "results/movel_classical_acc.RDS")
