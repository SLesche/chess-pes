library(tidyverse)
library(data.table)
library(MatchIt)

source("read_pgn_moves.R")
# Replace with your PGN file path
pgn_file <- "eval_games_21_06_blitz.pgn"

# test <- read_pgn_moves(pgn_file, max.lines = 100000
con <- file(pgn_file, open = "r")
games <- read_pgn_moves_stream(con, max.games = 500000)  # only first 1000 games
close(con)

data <- games %>% 
  mutate(
    move_num = as.numeric(str_extract(Move, "^\\d+")),
    color = ifelse(str_detect(Move, "\\.{3}"), "black", "white"),
  ) %>% 
  mutate(
    player_moving = ifelse(color == "white", White, Black),
    player_moving_elo = ifelse(color == "white", WhiteElo, BlackElo),
    opponent_elo = ifelse(color == "white", BlackElo, WhiteElo),
  ) %>% 
  mutate(increment = as.numeric(str_remove(TimeControl, "^\\d+\\+")),
         time_control = as.numeric(str_remove(TimeControl, "\\+\\d+$"))) %>% 
  mutate(increment = ifelse(is.na(increment), 0, increment)) %>% 
  select(
    -contains("White"), -contains("Black"), -Move, -TimeControl
  ) %>% 
  janitor::clean_names()

rm(games)
gc()

data <- data %>% 
  mutate(eval = case_when(
    str_detect(eval, "^#\\d+$") ~ "99", 
    str_detect(eval, "^#-\\d+$") ~ "-99",
    TRUE ~ eval
  )) %>% 
  mutate(eval = as.numeric(eval)) %>% 
  group_by(id) %>% 
  mutate(prev_eval = lag(eval)) %>% 
  group_by(id, color) %>% 
  mutate(prev_time = lag(clock)) %>% 
  ungroup()

data <- data %>% 
  mutate(
    move_time = prev_time - clock + increment*100,
    move_eval = eval - prev_eval
  ) %>% 
  mutate(move_time = ifelse(move_time < 0, NA, move_time)) %>% 
  mutate(
    corrected_move_eval = ifelse(color == "black", -1 * move_eval, move_eval),
    corrected_eval = ifelse(color == "black", -1 * eval, eval),
    corrected_prev_eval = ifelse(color == "black", -1 * prev_eval, prev_eval),
  ) %>% 
  group_by(id, color) %>% 
  mutate(
    prev_own_move_eval = lag(corrected_move_eval),
  ) %>% 
  group_by(id) %>% 
  mutate(
    prev_opp_move_eval = lag(corrected_move_eval),
    opp_move_time = lag(move_time),
    opp_clock = lag(clock)
  ) %>% 
  ungroup()

data <- data %>% 
  mutate(
    mistake = factor(ifelse(corrected_move_eval < -2 & ((color == "black" & eval > 0) | (color == "white" & eval < 0)), 1, 0))
  ) %>% 
  group_by(id) %>% 
  mutate(
    prev_opp_mistake = factor(lag(mistake))
  ) %>% 
  group_by(id, color) %>% 
  mutate(prev_own_mistake = factor(lag(mistake))) %>% 
  ungroup() 

data <- data %>% 
  group_by(id) %>% 
  filter(move_num != max(move_num)) %>% 
  ungroup() %>% 
  filter(!is.na(move_num), !is.na(eval), !is.na(clock), !is.na(opp_move_time)) %>% 
  filter(!abs(eval) > 10, !abs(prev_eval) > 10) %>% 
  filter(!clock > 18000) %>% 
  filter(move_time < 6000, move_time > 0) %>% 
  mutate(group = ifelse(prev_own_mistake == 1, "posterror", "control"))

# 
# posterror_move_data <- data %>% 
#   filter(
#     prev_own_mistake == 1
#   )

# Example: match sampled_data to full_data by move_num and eval

match_result <- matchit(
  group ~ move_num + prev_eval + clock + color + prev_opp_move_eval +  opp_move_time + opp_clock, 
  data = data,
  method = "quick",
  distance = "mahalanobis"
)

full_data <- match.data(match_result)

saveRDS(match_result, file = "lichess_data/match_result.RDS")
saveRDS(full_data, file = "lichess_data/full_data.RDS")
