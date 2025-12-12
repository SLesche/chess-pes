library(tidyverse)
library(data.table)
library(MatchIt)

source("read_pgn_moves.R")
# Replace with your PGN file path
pgn_file <- "eval_games_21_06_classical.pgn"

# test <- read_pgn_moves(pgn_file, max.lines = 100000
con <- file(pgn_file, open = "r")
games <- read_pgn_moves_stream(con, max.games = 350000)  # only first 1000 games
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
    prev_own_move_time = lag(move_time)
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
    mistake = factor(ifelse(corrected_move_eval < -2, 1, 0)),
    sign_flipped = ifelse(prev_eval * eval < 0, 1, 0),
    player_losing_before_move = ifelse(corrected_prev_eval < 0, 1, 0),
    player_losing_after_move = ifelse(corrected_eval < 0, 1, 0)
  ) %>% 
  group_by(id) %>% 
  mutate(
    prev_opp_mistake = factor(lag(mistake))
  ) %>% 
  group_by(id, color) %>% 
  mutate(
    prev_own_mistake = factor(lag(mistake)),
    mistake_made_in_losing_position = factor(lag(player_losing_before_move)),
    mistake_result_in_losing_position = factor(lag(player_losing_after_move)),
    mistake_flipped_sign = factor(lag(sign_flipped))
    ) %>% 
  ungroup() 

data <- data %>% 
  group_by(id) %>% 
  filter(move_num != max(move_num)) %>% 
  ungroup() %>% 
  filter(!is.na(move_num), !is.na(eval), !is.na(clock), !is.na(opp_move_time)) %>% 
  filter(!abs(eval) > 10, !abs(prev_eval) > 10) %>% 
  # filter(!clock > 18000) %>% 
  filter(move_time > 0) %>%
  mutate(group = ifelse(prev_own_mistake == 1, "posterror", "control")) %>% 
  mutate(clock_secs = clock / 100, opp_clock_secs = opp_clock / 100) %>% 
  mutate(
    move_num_dec = round(move_num, -1),
    clock_secs_dec = round(clock_secs, -1),
    prev_eval_one = round(prev_eval),
    prev_opp_move_eval_one = round(prev_opp_move_eval),
    opp_clock_secs_dec = round(opp_clock_secs, -1),
    opp_move_time_secs = opp_move_time / 100,
    opp_move_time_secs_dec = round(opp_move_time / 100, -1),
    move_time_secs = move_time / 100,
    prev_move_time_secs = prev_own_move_time / 100,
    player_moving_elo_hun = round(player_moving_elo, -2)
  )

saveRDS(data, file = "lichess_data/data_before_match_350k_classical.RDS")

# 
# posterror_move_data <- data %>% 
#   filter(
#     prev_own_mistake == 1
#   )

# Example: match sampled_data to full_data by move_num and eval

# match_result <- matchit(
#   group ~ move_num + prev_eval + clock_secs + color + prev_opp_move_eval +  opp_move_time + opp_clock_secs,
#   data = data,
#   method = "nearest",
#   distance = "mahalanobis"
# )
# full_data <- match.data(match_result)

library(parallel)
batch_ids <- split(data,
                   interaction(
                     data$color,
                     data$move_num_dec,
                     # data$prev_eval_one,
                     # data$prev_opp_move_eval_one
                     data$clock_secs_dec,
                     data$player_moving_elo_hun
                     # data$opp_clock_secs_dec,
                     # data$opp_move_time_secs_dec
                     )
                   )  # or any grouping

batch_ids <- batch_ids[sapply(batch_ids, nrow) > 100]
batch_ids <- batch_ids[sapply(batch_ids, function(df) any(df$group == "posterror"))]

rm(data)
gc()
cores <- detectCores() - 1
cl <- makeCluster(cores)

# Export needed packages and objects to each worker
clusterEvalQ(cl, library(MatchIt))
clusterExport(cl, varlist = c("batch_ids"))

# Run MatchIt in parallel safely on Windows
results <- parLapply(cl, batch_ids, function(df) {
  matchit(group ~ move_num + prev_eval + clock_secs +
            prev_opp_move_eval + opp_move_time + opp_clock_secs + 
            player_moving_elo + opponent_elo,
          data = df,
          method = "nearest",
          distance = "mahalanobis")
})

stopCluster(cl)

# Extract matched datasets from each batch
matched_dfs <- lapply(results, function(m) {
  if (!is.null(m)) {
    match.data(m)
  } else {
    NULL
  }
})

rm(results)
gc()
# Remove any NULLs (safety)
matched_dfs <- matched_dfs[!sapply(matched_dfs, is.null)]
full_data <- bind_rows(matched_dfs, .id = "batch")
rm(matched_dfs)
gc()

# saveRDS(match_result, file = "lichess_data/match_result.RDS")
saveRDS(full_data, file = "lichess_data/full_data_350k_classical.RDS")
rm(full_data)
gc()
